use std::time::Duration;

pub fn format_duration_adaptive(d: Duration) -> String {
    let ns = d.as_nanos();
    let (val, unit) = if ns >= 1_000_000_000 {
        (ns as f64 / 1e9, "s")
    } else if ns >= 1_000_000 {
        (ns as f64 / 1e6, "ms")
    } else if ns >= 1_000 {
        (ns as f64 / 1e3, "us")
    } else {
        return format!("{} ns", d.as_nanos());
    };

    if val >= 100.0 {
        format!("{:.0} {}", val, unit)
    } else if val >= 10.0 {
        format!("{:.1} {}", val, unit)
    } else {
        format!("{:.2} {}", val, unit)
    }
}
