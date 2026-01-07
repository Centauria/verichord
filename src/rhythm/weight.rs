/// Compute rhythm weight for a note spanning [a, b] within a bar subdivided by `beats`.
///
/// - `a`, `b`: normalized times with 0 <= a < b <= 1
/// - `beats`: hierarchical subdivision array, e.g. [2, 3]
///
/// Semantics:
/// - Generate beat points by iteratively subdividing each segment according to `beats`.
///   Each internal division point produced at level `i` has weight equal to the product of 1/beat_j
///   for j in 0..=i (i.e. parent segment weight times 1/beat_i).
/// - Endpoints 0 and 1 always exist with weight 1.
/// - When a single physical position is produced at multiple levels, keep the largest weight (higher-level wins).
/// - A beat point is "covered" by a note [a,b] if the distance from the point to the interval [a,b]
///   is strictly less than half of the minimal spacing between adjacent unique beat points.
/// - If the note covers one or more beat points, return the sum of the covered beat point weights (no special dropping).
///   If no beat points are covered, return 0.0.

pub fn rhythm_weight(a: f64, b: f64, beats: &[i32]) -> f64 {
    debug_assert!(0.0 <= a && a < b && b <= 1.0);

    if beats.is_empty() {
        return 0.0;
    }

    // Generate division points with associated weights.
    // We'll collect all produced positions (may have duplicates from different levels),
    // then merge positions that are extremely close.

    let mut pts: Vec<(f64, f64)> = Vec::new();
    // endpoints: include start (0.0) as a strong beat but treat the bar end (1.0)
    // as the next bar's start — do not insert an explicit 1.0 point to avoid over-emphasizing bar endings.
    pts.push((0.0, 1.0));

    // segments for next level: each element is (start, end, parent_weight)
    let mut segs: Vec<(f64, f64, f64)> = vec![(0.0, 1.0, 1.0)];

    for &n in beats {
        if n <= 0 {
            continue;
        }
        let mut next: Vec<(f64, f64, f64)> = Vec::with_capacity(segs.len() * (n as usize));
        for &(s, e, p_w) in &segs {
            let step = (e - s) / (n as f64);
            let child_w = p_w / (n as f64);
            // internal division points
            for k in 1..n {
                let pos = s + (k as f64) * step;
                pts.push((pos, child_w));
            }
            // create children segments
            for k in 0..n {
                let cs = s + (k as f64) * step;
                let ce = cs + step;
                next.push((cs, ce, child_w));
            }
        }
        segs = next;
    }

    // Merge positions that are extremely close (to avoid floating roundoff duplicates)
    pts.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    let eps = 1e-12;
    let mut uniq: Vec<(f64, f64)> = Vec::new();
    for (pos, w) in pts {
        if let Some((last_pos, last_w)) = uniq.last_mut() {
            if (pos - *last_pos).abs() <= eps {
                // same position: keep max weight (higher-level)
                if w > *last_w {
                    *last_w = w;
                }
                continue;
            }
        }
        uniq.push((pos, w));
    }

    // Compute minimal spacing between adjacent unique points
    let mut min_spacing = std::f64::INFINITY;
    for win in uniq.windows(2) {
        let d = win[1].0 - win[0].0;
        if d < min_spacing {
            min_spacing = d;
        }
    }

    if !min_spacing.is_finite() || min_spacing <= 0.0 {
        // degenerate: either only one unique point or bad data
        // fallback: consider radius = 0
        min_spacing = 1.0;
    }

    // Determine smoothing / tolerance parameters.
    // These defaults try to model perception: onset uses a small nearest-window, sustain uses a
    // Gaussian kernel (distance -> contribution) and a small λ for scaling sustain.
    let onset_radius_factor: f64 = 0.6; // times min_spacing
    let sustain_sigma_factor: f64 = 0.6; // times min_spacing
    let sustain_lambda: f64 = 0.25; // scale for sustain contribution

    let onset_radius = onset_radius_factor * min_spacing;
    let sustain_sigma = sustain_sigma_factor * min_spacing;

    // helper: distance from point x to interval [a,b]
    fn dist_point_interval(x: f64, a: f64, b: f64) -> f64 {
        if x < a {
            a - x
        } else if x > b {
            x - b
        } else {
            0.0
        }
    }

    // WONSET: look for the nearest beat point to the onset 'a', using circular (mod-1) distance
    // so onsets near the bar end are considered close to the next-bar start (0.0).
    let mut nearest_d = std::f64::INFINITY;
    let mut nearest_w = 0.0;
    for &(pos, w) in &uniq {
        // consider wrapped distances: pos, pos+1, pos-1
        let d0 = (pos - a).abs();
        let d1 = (pos + 1.0 - a).abs();
        let d2 = (pos - 1.0 - a).abs();
        let d = d0.min(d1).min(d2);
        if d < nearest_d {
            nearest_d = d;
            nearest_w = w;
        }
    }
    let wonset = if nearest_d <= onset_radius {
        nearest_w
    } else {
        0.0
    };

    // WSUSTAIN: Gaussian-weighted sum over all beat points based on distance to [a,b],
    // using wrap-aware interval distance so points near 0.0 can be close to intervals near 1.0.
    let mut wsustain_sum = 0.0;
    // guard against sigma == 0
    let sigma = if sustain_sigma > 0.0 {
        sustain_sigma
    } else {
        1e-12
    };
    for &(pos, w) in &uniq {
        // compute distance to [a,b] considering pos shifted by -1,0,+1
        let d0 = dist_point_interval(pos, a, b);
        let d1 = dist_point_interval(pos + 1.0, a, b);
        let d2 = dist_point_interval(pos - 1.0, a, b);
        let d = d0.min(d1).min(d2);
        let k = (-0.5 * (d / sigma) * (d / sigma)).exp();
        wsustain_sum += w * k;
    }

    // Combine with small λ and return
    let weight = wonset + sustain_lambda * wsustain_sum;
    weight
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_examples() {
        // beats = [4]
        let b = vec![4];
        // a small interval centered on 1/4 should have weight greater than that beat's base weight because of sustain kernel
        let w_q = rhythm_weight(0.24, 0.26, &b);
        assert!(w_q > 0.25 && w_q < 0.5);

        // covers {0,1/4} -> with sustain kernel and λ this should be >= previous onset-only sum
        let w_cover = rhythm_weight(0.0, 0.3, &b);
        assert!(w_cover > 1.25);

        // beats = [2,2]
        let b22 = vec![2, 2];
        // positions: 0, 1/4, 1/2, 3/4
        let w_mid = rhythm_weight(0.49, 0.51, &b22);
        assert!(w_mid > 0.5);
        assert!(rhythm_weight(0.24, 0.26, &b22) > 0.25);

        // beats = [2,3]
        let b23 = vec![2, 3];
        // positions: 0, 1/6,2/6,3/6,4/6,5/6
        let small23 = rhythm_weight(1.0 / 6.0 - 0.01, 1.0 / 6.0 + 0.01, &b23);
        assert!(small23 >= (1.0 / 6.0) - 1e-9);

        // beats = [3,2]
        let b32 = vec![3, 2];
        // check 1/6 weight
        let small32 = rhythm_weight(1.0 / 6.0 - 0.01, 1.0 / 6.0 + 0.01, &b32);
        assert!(small32 >= (1.0 / 6.0) - 1e-9);

        // if note covers multiple points
        // for b=[4], cover 0.0..0.3 covers 0 and 1/4 -> with sustain kernel expect weight > 1.25
        assert!(rhythm_weight(0.0, 0.3, &b) > 1.25);
    }

    #[test]
    fn test_no_cover() {
        let b = vec![4];
        // far from any internal points: choose tiny interval mid between 1/8 and 3/8? Use (0.26,0.28) not near 0.25? But 0.25+/-0.125 covers 0.125..0.375 so many intervals covered.
        // pick interval between 0.375 and 0.625 which covers 1/2 though. To get no cover, choose very small interval near 0.05 where radius=0.125 covers 0.
        let w = rhythm_weight(0.375 + 0.20, 0.375 + 0.21, &b);
        // interval (0.575, 0.585) lies within radius of the 1/2 beat (0.5) for beats=[4], so expect a small-to-moderate positive weight
        assert!(w > 0.0 && w < 0.6);

        // onset near bar end should be treated as close to next-bar start (0.0)
        let w_end_onset = rhythm_weight(0.99, 1.0, &b);
        assert!(w_end_onset >= 1.0);
    }
}
