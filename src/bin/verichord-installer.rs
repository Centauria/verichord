use std::env;
use std::error::Error;

/// Thin wrapper binary that delegates installer functionality to the library's `install` module.
fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().skip(1).collect();
    // Delegate to the shared installer implementation in the install module
    verichord::install::run_installer(&args)
}
