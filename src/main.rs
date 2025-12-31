mod algo_load;
mod midi;
mod chord;
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
