use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum ScrollMode {
    Smooth,
    Bar,
}

#[derive(Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum ChordScrollDirection {
    Horizontal,
    Vertical,
}

#[derive(Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum ChordUpdateFrequency {
    Beat,
    Measure,
}

#[derive(Deserialize, Serialize)]
#[serde(default)]
pub struct AppState {
    pub tempo_bpm: u32,
    pub scroll_mode: ScrollMode,
    pub chords_scroll_direction: ChordScrollDirection,
    pub log_width_frac: f32,
    pub measures: u32,
    pub time_sig_a: u8,
    pub time_sig_b: u32,
    pub metronome_enabled: bool,
    pub power_save_mode: bool,
    pub lookahead_enabled: bool,
    pub selected_algo_idx: Option<usize>,
    pub show_initial_chord: bool,
    pub chord_update_frequency: ChordUpdateFrequency,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            tempo_bpm: 120,
            scroll_mode: ScrollMode::Smooth,
            chords_scroll_direction: ChordScrollDirection::Horizontal,
            log_width_frac: 0.35,
            measures: 2,
            time_sig_a: 4,
            time_sig_b: 4,
            metronome_enabled: false,
            power_save_mode: false,
            lookahead_enabled: true,
            selected_algo_idx: None,
            show_initial_chord: true,
            chord_update_frequency: ChordUpdateFrequency::Beat,
        }
    }
}
