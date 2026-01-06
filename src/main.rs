#![cfg_attr(
    all(target_os = "windows", not(debug_assertions)),
    windows_subsystem = "windows"
)]

mod algo_load;
mod chord;
mod midi;
mod rhythm;
mod ui;

fn main() {
    let options = eframe::NativeOptions::default();
    eframe::run_native(
        "VeriChord",
        options,
        Box::new(|_cc| Box::new(ui::MidiApp::default())),
    )
    .unwrap();
}
