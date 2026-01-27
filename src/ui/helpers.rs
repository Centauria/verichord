use std::time::Duration;

pub fn format_duration_adaptive(d: Duration) -> String {
    let ns = d.as_nanos() as f64;
    if ns >= 1_000_000_000.0 {
        format!("{:.3} s", ns / 1e9)
    } else if ns >= 1_000_000.0 {
        format!("{:.3} ms", ns / 1e6)
    } else if ns >= 1_000.0 {
        format!("{:.3} us", ns / 1e3)
    } else {
        format!("{} ns", d.as_nanos())
    }
}
