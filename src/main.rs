#![cfg_attr(
    all(target_os = "windows", not(debug_assertions)),
    windows_subsystem = "windows"
)]

mod algo_load;
mod chord;
mod metronome;
mod midi;
mod rhythm;
mod ui;

#[cfg(all(target_os = "windows", not(debug_assertions)))]
fn redirect_standard_streams() {
    use std::fs::File;
    use std::os::windows::io::IntoRawHandle;

    // STD_OUTPUT_HANDLE = -11 (0xFFFFFFF5)
    // STD_ERROR_HANDLE = -12 (0xFFFFFFF4)
    const STD_OUTPUT_HANDLE: u32 = 0xFFFFFFF5;
    const STD_ERROR_HANDLE: u32 = 0xFFFFFFF4;

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn SetStdHandle(nStdHandle: u32, hHandle: *mut std::ffi::c_void) -> i32;
    }

    unsafe {
        // Redirect stdout to NUL
        if let Ok(f) = File::create("NUL") {
            SetStdHandle(STD_OUTPUT_HANDLE, f.into_raw_handle() as _);
        }
        // Redirect stderr to NUL
        if let Ok(f) = File::create("NUL") {
            SetStdHandle(STD_ERROR_HANDLE, f.into_raw_handle() as _);
        }
    }
}

fn main() {
    // In release mode on Windows, we don't have a console.
    // If external DLLs (C++) try to print to stdout/stderr, it can cause
    // blocking or significant slowdowns (e.g. 2ms -> 2s) because the handles are invalid.
    // We redirect them to NUL to consume the output efficiently.
    #[cfg(all(target_os = "windows", not(debug_assertions)))]
    redirect_standard_streams();

    let mut options = eframe::NativeOptions::default();

    // Set persistence path to executable directory to save window state file locally
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            options.persistence_path = Some(exe_dir.join("verichord_store.ron"));
        }
    }

    eframe::run_native(
        "VeriChord",
        options,
        Box::new(|cc| Ok(Box::new(ui::MidiApp::new(cc)))),
    )
    .unwrap();
}
