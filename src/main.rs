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

fn main() {
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
