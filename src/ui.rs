use chrono::Local;
use eframe::{egui, egui::ComboBox};
use midir::{Ignore, MidiInput, MidiInputConnection};
use serde::{Deserialize, Serialize};
use std::sync::mpsc::{self, Receiver, Sender};
use std::time::{Duration, Instant};

use crate::metronome::Metronome;
use wmidi::MidiMessage;

use crate::chord::PitchOrderedSet;
use crate::midi::{NoteAction, NoteHit, generate_chord_for_measure, parse_note_action};

use rfd::FileDialog;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn format_duration_adaptive(d: Duration) -> String {
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

/// Render a menu row that includes a right-aligned, monospace shortcut hint.
/// Returns the `Response` for the whole row so callers can call `.clicked()` etc.
fn menu_item_with_shortcut(
    ui: &mut egui::Ui,
    label: &str,
    shortcut: &str,
    desired_width: Option<f32>,
) -> egui::Response {
    use egui::{Align2, FontId, TextStyle};

    // Reserve a menu row that is clickable; use provided desired_width if any, otherwise fall back to a reasonable default.
    let height = ui.spacing().interact_size.y.max(18.0);
    let width = desired_width.unwrap_or_else(|| ui.available_width().min(280.0));
    let size = egui::vec2(width, height);
    let (rect, resp) = ui.allocate_exact_size(size, egui::Sense::click());
    let painter = ui.painter();

    // Subtle hover/active background
    if resp.hovered() || resp.is_pointer_button_down_on() {
        let fill = ui.style().visuals.widgets.hovered.bg_fill;
        painter.rect_filled(rect, 0.0, fill);
    }

    // Left label (normal text style)
    let label_font = TextStyle::Button.resolve(ui.style());
    painter.text(
        rect.left_center(),
        Align2::LEFT_CENTER,
        label,
        label_font.clone(),
        ui.style().visuals.text_color(),
    );

    // Right shortcut: use proportional font on macOS so symbols like ⌘ and ⇧ render reliably, otherwise monospace.
    let shortcut_font = if cfg!(target_os = "macos") {
        // Use a proportional font on macOS so symbols like ⌘ and ⇧ render reliably
        FontId::proportional(label_font.size * 0.95)
    } else {
        FontId::monospace(label_font.size * 0.90)
    };
    painter.text(
        rect.right_center(),
        Align2::RIGHT_CENTER,
        shortcut,
        shortcut_font,
        ui.style().visuals.text_color(),
    );

    resp
}

#[derive(Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum ScrollMode {
    Smooth,
    Bar,
}

#[derive(Deserialize, Serialize)]
#[serde(default)]
pub struct AppState {
    pub tempo_bpm: u32,
    pub scroll_mode: ScrollMode,
    pub log_width_frac: f32,
    pub measures: u32,
    pub time_sig_a: u8,
    pub time_sig_b: u32,
    pub metronome_enabled: bool,
    pub power_save_mode: bool,
    pub selected_algo_idx: Option<usize>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            tempo_bpm: 120,
            scroll_mode: ScrollMode::Smooth,
            log_width_frac: 0.35,
            measures: 2,
            time_sig_a: 4,
            time_sig_b: 4,
            metronome_enabled: false,
            power_save_mode: false,
            selected_algo_idx: None,
        }
    }
}

pub struct MidiApp {
    midi_in: MidiInput,
    ports: Vec<String>,
    selected: Option<usize>,
    connection: Option<MidiInputConnection<()>>,
    tx: Sender<(Vec<u8>, Instant)>,
    rx: Receiver<(Vec<u8>, Instant)>,
    log: Vec<String>,
    status: String,
    tempo_bpm: u32,
    note_hits: Vec<NoteHit>,
    scroll_mode: ScrollMode,
    start_time: Option<Instant>,
    measures: u32,
    log_width_frac: f32,
    chords: Vec<(u32, PitchOrderedSet, Duration)>,
    chords_auto_scroll: bool,
    chord_pending_scroll_index: Option<usize>,
    save_path: Option<PathBuf>,
    scrolling_active: bool,
    frozen_view_end: Option<Instant>,
    recording_ended_at: Option<Instant>,
    /// True when user has started recording (via Record button / Space). MIDI input shouldn't auto-start recording when false.
    recording_enabled: bool,

    // time signature a/b where a in 1..=16 and b in {2,4,8,16}
    time_sig_a: u8,
    time_sig_b: u32,
    log_auto_scroll: bool,
    log_pending_scroll: bool,
    last_log_scroll_offset: Option<f32>,
    last_chord_scroll_offset: Option<f32>,

    // algorithm plugins discovered in the executable directory
    algos: Vec<crate::algo_load::AlgoLib>,
    // index into `algos` for the selected algorithm (None == no plugin selected)
    selected_algo_idx: Option<usize>,
    power_save_mode: bool,
    // metronome: play click on each beat when enabled
    metronome_enabled: bool,
    metronome: Option<Metronome>,
}

impl Default for MidiApp {
    fn default() -> Self {
        let midi_in = MidiInput::new("verichord")
            .unwrap_or_else(|_| MidiInput::new("verichord-fallback").unwrap());
        let (tx, rx) = mpsc::channel();
        let mut app = MidiApp {
            midi_in,
            ports: Vec::new(),
            selected: None,
            connection: None,
            tx,
            rx,
            log: Vec::new(),
            status: "Idle".to_string(),
            tempo_bpm: 120,
            note_hits: Vec::new(),
            scroll_mode: ScrollMode::Smooth,
            start_time: None,
            measures: 2,
            log_width_frac: 0.35_f32,
            chords: vec![(1, PitchOrderedSet::new(), std::time::Duration::ZERO)],
            chords_auto_scroll: true,
            chord_pending_scroll_index: None,
            save_path: None,
            scrolling_active: false,
            frozen_view_end: None,
            recording_ended_at: None,
            recording_enabled: false,

            time_sig_a: 4,
            time_sig_b: 4,
            log_auto_scroll: true,
            log_pending_scroll: false,
            last_log_scroll_offset: None,
            last_chord_scroll_offset: None,
            algos: Vec::new(),
            selected_algo_idx: None,
            power_save_mode: false,
            metronome_enabled: true,
            metronome: None,
        };
        app.midi_in.ignore(Ignore::None);
        app.refresh_ports();
        app.refresh_algos();
        app
    }
}

impl MidiApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut app = Self::default();

        if let Some(storage) = cc.storage {
            if let Some(state) = eframe::get_value::<AppState>(storage, eframe::APP_KEY) {
                app.tempo_bpm = state.tempo_bpm;
                app.scroll_mode = state.scroll_mode;
                app.log_width_frac = state.log_width_frac;
                app.measures = state.measures;
                app.time_sig_a = state.time_sig_a;
                app.time_sig_b = state.time_sig_b;
                app.metronome_enabled = state.metronome_enabled;
                app.power_save_mode = state.power_save_mode;
                app.selected_algo_idx = state.selected_algo_idx;
            }
        }

        // Refresh ports/algos to ensure valid state even if loaded config is stale
        app.refresh_ports();
        app.refresh_algos();

        app
    }

    fn refresh_ports(&mut self) {
        self.ports.clear();
        let list = self.midi_in.ports();
        for p in list {
            match self.midi_in.port_name(&p) {
                Ok(name) => self.ports.push(name),
                Err(_) => self.ports.push("Unnamed port".to_string()),
            }
        }
        if self.selected.is_none() && !self.ports.is_empty() {
            self.selected = Some(0);
        }
    }

    /// Refresh the list of algorithm plugins found next to the running executable.
    /// Keeps the currently selected index if it still exists; otherwise chooses
    /// the first found plugin (if any).
    fn refresh_algos(&mut self) {
        match crate::algo_load::find_algos_in_exe_dir() {
            Ok(list) => {
                self.algos = list;
                if self.selected_algo_idx.is_none() && !self.algos.is_empty() {
                    self.selected_algo_idx = Some(0);
                } else if let Some(idx) = self.selected_algo_idx {
                    if idx >= self.algos.len() {
                        self.selected_algo_idx = None;
                    }
                }
            }
            Err(_) => {
                self.algos.clear();
                self.selected_algo_idx = None;
            }
        }
    }

    pub fn open_selected(&mut self, ctx: &egui::Context) {
        if self.connection.is_some() {
            self.status = "Already open".to_string();
            return;
        }
        if let Some(idx) = self.selected {
            let ports = self.midi_in.ports();
            if idx >= ports.len() {
                self.status = "Invalid port selected".to_string();
                return;
            }
            let port = &ports[idx];
            let port_name = self
                .midi_in
                .port_name(port)
                .unwrap_or_else(|_| "Unnamed".to_string());

            // clone tx for callback
            let cb_tx = self.tx.clone();
            let ctx_clone = ctx.clone();
            let connector = MidiInput::new("verichord-connection")
                .unwrap_or_else(|_| MidiInput::new("verichord-conn-fallback").unwrap());
            match connector.connect(
                port,
                "verichord-connection",
                move |_, message, _| {
                    let _ = cb_tx.send((message.to_vec(), Instant::now()));
                    ctx_clone.request_repaint();
                },
                (),
            ) {
                Ok(conn) => {
                    self.status = format!("Open: {}", port_name);
                    self.connection = Some(conn);
                    // Reset state for a fresh recording session: clear MIDI event log, piano hits, and chord cards.
                    // Keep the frozen view (if any) so reopening doesn't cause the grid to jump; scrolling will begin on first Note On.
                    self.log.clear();
                    self.note_hits.clear();
                    self.chords.clear();
                    self.chords
                        .push((1, PitchOrderedSet::new(), std::time::Duration::ZERO));
                    self.chord_pending_scroll_index = None;
                    // reset scroll bookkeeping so UI doesn't immediately disable auto-scroll or jump
                    self.log_pending_scroll = false;
                    self.last_log_scroll_offset = None;
                    self.last_chord_scroll_offset = None;
                    // ensure no chord generation until first Note On
                    self.start_time = None;
                    self.scrolling_active = false;
                    self.recording_ended_at = None;
                    // require user to explicitly start recording (Record button / Space)
                    self.recording_enabled = false;
                }
                Err(e) => {
                    self.status = format!("Failed to open: {}", e);
                }
            }
        } else {
            self.status = "No port selected".to_string();
        }
    }

    pub fn close_connection(&mut self) {
        if let Some(conn) = self.connection.take() {
            drop(conn);
            self.status = "Closed".to_string();

            // Calculate current view_end to freeze correctly (preserving bar snap if active)
            let now = Instant::now();
            let freeze_time = if let Some(start) = self.start_time {
                match self.scroll_mode {
                    ScrollMode::Smooth => now,
                    ScrollMode::Bar => {
                        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
                        let measure_secs = beat_secs * (self.time_sig_a as f32);

                        let elapsed = now.duration_since(start).as_secs_f32();
                        let measure_idx = (elapsed / measure_secs).floor();
                        start
                            + std::time::Duration::from_secs_f32((measure_idx + 1.0) * measure_secs)
                    }
                }
            } else {
                now
            };

            // stop scrolling and freeze the current view so piano and chord windows stop moving
            self.scrolling_active = false;
            self.frozen_view_end = Some(freeze_time);
            self.recording_ended_at = Some(freeze_time);
            // prevent automatic scroll of the chord strip
            self.chord_pending_scroll_index = None;
        } else {
            self.status = "No open connection".to_string();
        }
    }

    fn start_recording(&mut self, anchor: Instant) {
        self.recording_enabled = true;
        self.start_time = Some(anchor);
        self.scrolling_active = true;
        self.frozen_view_end = None;

        // If the selected algorithm provides reset_state(), call it before clearing generated content
        if let Some(idx) = self.selected_algo_idx {
            if let Some(algo) = self.algos.get(idx) {
                if algo.has_reset_state() {
                    match algo.reset_state() {
                        Ok(()) => {
                            self.status = format!(
                                "{}: reset_state()",
                                algo.file_stem().unwrap_or_else(|| algo.name.clone())
                            );
                        }
                        Err(e) => {
                            self.status = format!(
                                "Failed to reset {}: {}",
                                algo.file_stem().unwrap_or_else(|| algo.name.clone()),
                                e
                            );
                        }
                    }
                }
            }
        }

        // Clear old generated content so re-recording starts fresh
        self.chords.clear();
        self.chords
            .push((1, PitchOrderedSet::new(), std::time::Duration::ZERO));
        self.chord_pending_scroll_index = None;
        // Clear prior MIDI hits and event log to avoid showing previous recordings
        self.note_hits.clear();
        self.log.clear();
        self.recording_ended_at = None;

        // Update status bar with start time
        self.status = format!("Recording started at {}", Local::now().format("%H:%M:%S"));

        // If the user has enabled the metronome in settings, start or enable it now
        if self.metronome_enabled {
            if let Some(m) = &self.metronome {
                m.set_params(
                    self.tempo_bpm,
                    self.time_sig_a as usize,
                    self.time_sig_b,
                    true,
                );
                m.set_anchor(anchor);
            } else {
                let m = Metronome::start(
                    self.tempo_bpm,
                    self.time_sig_a as usize,
                    self.time_sig_b,
                    true,
                );
                m.set_anchor(anchor);
                self.metronome = Some(m);
            }
        }
    }

    fn stop_recording(&mut self) {
        let now = Instant::now();
        let freeze_time = if let Some(start) = self.start_time {
            match self.scroll_mode {
                ScrollMode::Smooth => now,
                ScrollMode::Bar => {
                    let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                    let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
                    let measure_secs = beat_secs * (self.time_sig_a as f32);

                    let elapsed = now.duration_since(start).as_secs_f32();
                    let measure_idx = (elapsed / measure_secs).floor();
                    start + std::time::Duration::from_secs_f32((measure_idx + 1.0) * measure_secs)
                }
            }
        } else {
            now
        };

        // stop scrolling and freeze the current view so piano and chord windows stop moving
        self.scrolling_active = false;
        self.frozen_view_end = Some(freeze_time);
        self.recording_ended_at = Some(freeze_time);
        self.chord_pending_scroll_index = None;
        self.recording_enabled = false;

        // Update status bar with stop time
        self.status = format!("Recording stopped at {}", Local::now().format("%H:%M:%S"));
    }

    fn drain_messages(&mut self) {
        while let Ok((bytes, ts)) = self.rx.try_recv() {
            let time = Local::now();
            // format with wmidi when possible
            let parsed = MidiMessage::try_from(bytes.as_slice())
                .map(|m| format!("{:?}", m))
                .unwrap_or_else(|_| {
                    bytes
                        .iter()
                        .map(|b| format!("{:02X}", b))
                        .collect::<Vec<_>>()
                        .join(" ")
                });

            // process note actions using helper from midi module
            if let Some(action) = parse_note_action(&bytes, ts) {
                match action {
                    NoteAction::On { pitch, vel, time } => {
                        // start scrolling from the first Note On timestamp, but only if user enabled recording (Record button / Space)
                        if self.start_time.is_none() {
                            if self.recording_enabled {
                                // reset start state and clear generated chords so the chord cards restart from measure 1
                                self.start_time = Some(time);
                                self.scrolling_active = true;
                                self.frozen_view_end = None;
                                self.chords.clear();
                                self.chords.push((
                                    1,
                                    PitchOrderedSet::new(),
                                    std::time::Duration::ZERO,
                                ));

                                // If the user has enabled the metronome in settings, start or enable it now
                                if self.metronome_enabled {
                                    if let Some(m) = &self.metronome {
                                        m.set_params(
                                            self.tempo_bpm,
                                            self.time_sig_a as usize,
                                            self.time_sig_b,
                                            true,
                                        );
                                        m.set_anchor(time);
                                    } else {
                                        let m = Metronome::start(
                                            self.tempo_bpm,
                                            self.time_sig_a as usize,
                                            self.time_sig_b,
                                            true,
                                        );
                                        m.set_anchor(time);
                                        self.metronome = Some(m);
                                    }
                                }
                            }
                        }
                        // Only record note hits if recording has started (start_time set)
                        if self.start_time.is_some() {
                            // measure length in seconds at the current tempo/signature
                            let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                            let beat_secs = quarter_secs * (4.0_f32 / self.time_sig_b as f32);
                            let measure_secs = beat_secs * (self.time_sig_a as f32);

                            let normalized = if let Some(st) = self.start_time {
                                (time.duration_since(st).as_secs_f32()) / measure_secs
                            } else {
                                0.0
                            };

                            self.note_hits.push(NoteHit {
                                pitch: pitch,
                                start: normalized,
                                end: None,
                                velocity: vel,
                            });
                        }
                    }
                    NoteAction::Off { pitch, time } => {
                        if self.start_time.is_some() {
                            // measure length in seconds at the current tempo/signature
                            let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                            let beat_secs = quarter_secs * (4.0_f32 / self.time_sig_b as f32);
                            let measure_secs = beat_secs * (self.time_sig_a as f32);

                            if let Some(hit) = self
                                .note_hits
                                .iter_mut()
                                .rev()
                                .find(|h| h.pitch == pitch && h.end.is_none())
                            {
                                let normalized = if let Some(st) = self.start_time {
                                    (time.duration_since(st).as_secs_f32()) / measure_secs
                                } else {
                                    hit.start
                                };
                                hit.end = Some(normalized);
                            }
                        }
                    }
                }
            }

            let entry = format!(
                "{} [{} ms] {}",
                time.format("%H:%M:%S"),
                ts.elapsed().as_millis(),
                parsed
            );
            self.log.push(entry);
            if self.log.len() > 1000 {
                self.log.drain(0..200);
            }
            if self.log_auto_scroll {
                // request a forced scroll to bottom on next UI update
                self.log_pending_scroll = true;
            }
        }
    }

    fn ensure_chords_up_to(&mut self, up_to: u32) {
        // `self.chords` stores predicted chords per measure (1-based).
        // The initial chord is treated as measure 1. At the end of measure n,
        // we summarize notes in measure n and generate the chord for measure n+1.
        let mut next = self.chords.last().map(|(m, _, _)| m + 1).unwrap_or(2);
        while next <= up_to {
            // Prefer the explicit capability check so we don't rely only on raw fn pointers.
            // This also makes use of `has_sample_next_chord()` so the helper is exercised.
            let sample_fn = self.selected_algo_idx.and_then(|idx| {
                self.algos.get(idx).and_then(|a| {
                    if a.has_sample_next_chord() {
                        a.sample_next_chord_fn()
                    } else {
                        None
                    }
                })
            });
            let last_chord = self
                .chords
                .last()
                .map(|(_, c, _)| *c)
                .unwrap_or(PitchOrderedSet::new());
            let start = Instant::now();
            // Build normalized NoteData for the *previous* measure (`next - 1`).
            let notes_for_measure: Vec<crate::algo_load::NoteData> = {
                let mut notes = Vec::new();
                if self.start_time.is_some() {
                    // Seconds per quarter note, then convert quarter -> beat using denominator `b` (b=4 => beat=quarter)
                    let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                    let beat_secs = quarter_secs * (4.0_f32 / self.time_sig_b as f32);
                    let beats_per_measure: usize = self.time_sig_a as usize;
                    let measure_secs = beat_secs * beats_per_measure as f32;
                    // We are predicting chord for measure `next` using notes from measure `src_measure`.
                    // Measures are 1-based: measure 1 starts at t=0.
                    let src_measure = next - 1;
                    let src_idx0 = (src_measure - 1) as f32;
                    let measure_start = src_idx0 * measure_secs;
                    let measure_end = (src_idx0 + 1.0) * measure_secs;

                    for hit in &self.note_hits {
                        // convert normalized measure times to seconds relative to start_time
                        let s_elapsed = hit.start * measure_secs;

                        // Exclude if note starts at/after measure end.
                        if s_elapsed >= measure_end {
                            continue;
                        }

                        // Exclude if note ends at/before measure start.
                        if let Some(e) = hit.end {
                            let e_elapsed = e * measure_secs;
                            if e_elapsed <= measure_start {
                                continue;
                            }
                        }

                        // Normalize start/end within the source measure.
                        // Notes started in a previous measure => start=0.0.
                        let s_norm = if s_elapsed <= measure_start {
                            0.0
                        } else {
                            ((s_elapsed - measure_start) / measure_secs).clamp(0.0, 1.0)
                        };

                        // Notes still active at the measure boundary => end=1.0.
                        let e_norm = match hit.end {
                            Some(e) => {
                                let e_elapsed = e * measure_secs;
                                if e_elapsed >= measure_end {
                                    1.0
                                } else {
                                    ((e_elapsed - measure_start) / measure_secs).clamp(0.0, 1.0)
                                }
                            }
                            None => 1.0,
                        };

                        // Normalize velocity to [0,1] (MIDI velocities are 0..127)
                        let v_norm = (hit.velocity as f32) / 127.0;

                        notes.push(crate::algo_load::NoteData {
                            pitch: hit.pitch as i32,
                            start: s_norm,
                            end: e_norm,
                            velocity: v_norm,
                        });
                    }
                }
                notes
            };
            for note in &notes_for_measure {
                println!(
                    "Note in measure {}: pitch={} start={:.3} end={:.3} vel={:.3}",
                    next - 1,
                    note.pitch,
                    note.start,
                    note.end,
                    note.velocity
                );
            }
            let chord = generate_chord_for_measure(
                last_chord,
                sample_fn,
                &notes_for_measure,
                self.time_sig_a as u32,
            );
            let elapsed = start.elapsed();
            println!(
                "Generated chord for measure {}:\t{:032b} [{} ns]",
                next,
                chord.get_data(),
                elapsed.as_nanos()
            );
            self.chords.push((next, chord, elapsed));
            if self.chords_auto_scroll {
                self.chord_pending_scroll_index = Some(self.chords.len() - 1);
            }
            next += 1;
        }
    }

    fn save_piano_to_file(&self, path: &std::path::Path) -> Result<(), String> {
        // Save recorded piano notes to a TSV file.
        // Compute time units (seconds per quarter, beat, measure).
        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
        let measure_secs = beat_secs * (self.time_sig_a as f32);

        // Pick anchors for start/end: prefer explicit values, otherwise derive from note bounds
        let start_anchor = if let Some(s) = self.start_time {
            s
        } else {
            let earliest_meas = self
                .note_hits
                .iter()
                .map(|h| h.start)
                .fold(std::f32::INFINITY, |m, v| m.min(v));
            Instant::now() - std::time::Duration::from_secs_f32(earliest_meas * measure_secs)
        };

        let end_anchor = if let Some(ea) = self.recording_ended_at {
            ea
        } else {
            let latest_meas = self
                .note_hits
                .iter()
                .map(|h| h.end.unwrap_or(h.start))
                .fold(0.0_f32, |m, v| m.max(v));
            start_anchor + std::time::Duration::from_secs_f32(latest_meas * measure_secs)
        };

        let mut file = File::create(path).map_err(|e| e.to_string())?;

        // Write metadata header lines (prefixed with '#')
        let header = format!("# sig: {}/{}\n", self.time_sig_a, self.time_sig_b);
        file.write_all(header.as_bytes())
            .map_err(|e| e.to_string())?;

        // Write notes between anchors as TSV lines: <pitch>\t<start_meas_pos>\t<end_meas_pos>\t<velocity>\n
        for hit in &self.note_hits {
            // Convert normalized measure times into seconds since start_anchor
            let hit_start_secs = hit.start * measure_secs;
            let hit_end_secs = hit.end.unwrap_or(hit.start) * measure_secs;

            // Skip notes fully outside the anchor range
            if hit_start_secs > end_anchor.duration_since(start_anchor).as_secs_f32() {
                continue;
            }
            if hit_end_secs < 0.0 {
                continue;
            }

            let s_elapsed = hit_start_secs;
            let s_measure_idx = (s_elapsed / measure_secs).floor() as i64;
            let s_in_measure = ((s_elapsed - (s_measure_idx as f32) * measure_secs) / measure_secs)
                .clamp(0.0_f32, 1.0_f32);
            // measures are 1-based
            let s_pos = (s_measure_idx as f64) + s_in_measure as f64 + 1.0;

            let e_elapsed = hit_end_secs;
            let e_measure_idx = (e_elapsed / measure_secs).floor() as i64;
            let e_in_measure = ((e_elapsed - (e_measure_idx as f32) * measure_secs) / measure_secs)
                .clamp(0.0_f32, 1.0_f32);
            let e_pos = (e_measure_idx as f64) + e_in_measure as f64 + 1.0;

            let velocity = hit.velocity;

            let line = format!("{}\t{:.6}\t{:.6}\t{}\n", hit.pitch, s_pos, e_pos, velocity);
            file.write_all(line.as_bytes()).map_err(|e| e.to_string())?;
        }

        Ok(())
    }

    fn load_piano_from_file(&mut self, path: &std::path::Path) -> Result<(), String> {
        // Read TSV and populate self.note_hits for display in piano roll.
        let content = std::fs::read_to_string(path).map_err(|e| e.to_string())?;
        // Allow optional metadata header lines (prefixed with '#')
        // Example: # sig: 3/4
        let mut sig_a = self.time_sig_a;
        let mut sig_b = self.time_sig_b;

        // First pass: parse header lines only
        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if let Some(rest) = line.strip_prefix('#') {
                let rest = rest.trim();
                if let Some(sig) = rest.strip_prefix("sig:") {
                    let sig = sig.trim();
                    let parts: Vec<&str> = sig.split('/').collect();
                    if parts.len() == 2 {
                        let a: u8 = parts[0]
                            .trim()
                            .parse()
                            .map_err(|e| format!("Invalid sig numerator: {}", e))?;
                        let b: u32 = parts[1]
                            .trim()
                            .parse()
                            .map_err(|e| format!("Invalid sig denominator: {}", e))?;
                        sig_a = a;
                        sig_b = b;
                    }
                }
            }
        }

        // Apply parsed time signature (if provided)
        self.time_sig_a = sig_a;
        self.time_sig_b = sig_b;

        // Compute time units (seconds per quarter, beat, measure) with (possibly) updated time signature
        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
        let measure_secs = beat_secs * (self.time_sig_a as f32);

        let start_anchor = Instant::now();
        let mut hits: Vec<crate::midi::NoteHit> = Vec::new();
        let mut max_end_secs: f32 = 0.0;

        for (lineno, line) in content.lines().enumerate() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if line.starts_with('#') {
                continue;
            }
            let parts: Vec<&str> = line.split('\t').collect();
            if parts.len() < 4 {
                return Err(format!("Invalid TSV at line {}: '{}'", lineno + 1, line));
            }
            let pitch: u8 = parts[0]
                .parse()
                .map_err(|e| format!("Invalid pitch on line {}: {}", lineno + 1, e))?;
            let s_pos: f64 = parts[1]
                .parse()
                .map_err(|e| format!("Invalid start pos on line {}: {}", lineno + 1, e))?;
            let e_pos: f64 = parts[2]
                .parse()
                .map_err(|e| format!("Invalid end pos on line {}: {}", lineno + 1, e))?;
            let velocity: u8 = parts[3]
                .parse()
                .map_err(|e| format!("Invalid velocity on line {}: {}", lineno + 1, e))?;

            // convert measure-based positions to seconds relative to start_anchor
            let e_sec = ((e_pos - 1.0) as f32) * measure_secs;
            if e_sec > max_end_secs {
                max_end_secs = e_sec;
            }

            // store normalized measure positions (1 measure == 1.0)
            let start_meas = (s_pos - 1.0) as f32;
            let end_meas = (e_pos - 1.0) as f32;

            hits.push(crate::midi::NoteHit {
                pitch,
                start: start_meas,
                end: Some(end_meas),
                velocity,
            });
        }

        // Apply loaded hits to state
        self.note_hits = hits;
        self.start_time = Some(start_anchor);
        self.scrolling_active = false;
        self.recording_enabled = false;
        let frozen_end = start_anchor + std::time::Duration::from_secs_f32(max_end_secs.max(0.0));
        self.frozen_view_end = Some(frozen_end);
        self.recording_ended_at = Some(frozen_end);
        self.chords.clear();
        self.chords
            .push((1, PitchOrderedSet::new(), std::time::Duration::ZERO));
        self.save_path = Some(path.to_path_buf());

        Ok(())
    }
}

impl MidiApp {
    fn handle_new(&mut self) {
        // Reset piano state to a new, empty file
        self.note_hits.clear();
        self.start_time = None;
        self.scrolling_active = false;
        self.recording_enabled = false;
        self.frozen_view_end = None;
        self.recording_ended_at = None;
        self.chords.clear();
        self.chords
            .push((1, PitchOrderedSet::new(), std::time::Duration::ZERO));
        self.save_path = None;
        self.status = "New file".to_string();
    }

    fn handle_open_file(&mut self) {
        if let Some(p) = FileDialog::new()
            .set_title("Open piano TSV")
            .add_filter("TSV", &["tsv"])
            .pick_file()
        {
            match self.load_piano_from_file(&p) {
                Ok(()) => {
                    self.save_path = Some(p.clone());
                    self.status = format!("Loaded {}", p.display());
                }
                Err(e) => self.status = format!("Load failed: {}", e),
            }
        }
    }

    fn handle_save(&mut self) {
        if let Some(path) = &self.save_path {
            match self.save_piano_to_file(path) {
                Ok(()) => self.status = format!("Saved to {}", path.display()),
                Err(e) => self.status = format!("Save failed: {}", e),
            }
        } else {
            self.handle_save_as();
        }
    }

    fn handle_save_as(&mut self) {
        if let Some(p) = FileDialog::new()
            .set_title("Save piano as")
            .add_filter("TSV", &["tsv"])
            .save_file()
        {
            match self.save_piano_to_file(&p) {
                Ok(()) => {
                    self.save_path = Some(p.clone());
                    self.status = format!("Saved to {}", p.display());
                }
                Err(e) => self.status = format!("Save failed: {}", e),
            }
        }
    }
}

impl eframe::App for MidiApp {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        let state = AppState {
            tempo_bpm: self.tempo_bpm,
            scroll_mode: self.scroll_mode,
            log_width_frac: self.log_width_frac,
            measures: self.measures,
            time_sig_a: self.time_sig_a,
            time_sig_b: self.time_sig_b,
            metronome_enabled: self.metronome_enabled,
            power_save_mode: self.power_save_mode,
            selected_algo_idx: self.selected_algo_idx,
        };
        eframe::set_value(storage, eframe::APP_KEY, &state);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.drain_messages();

        // Manage metronome lifecycle and keep parameters in sync
        if self.metronome_enabled && self.metronome.is_none() {
            let enabled =
                self.metronome_enabled && self.start_time.is_some() && self.scrolling_active;
            let m = Metronome::start(
                self.tempo_bpm,
                self.time_sig_a as usize,
                self.time_sig_b,
                enabled,
            );
            if let Some(st) = self.start_time {
                m.set_anchor(st);
            }
            self.metronome = Some(m);
        }
        if !self.metronome_enabled && self.metronome.is_some() {
            // dropping the Option will stop the metronome thread (Drop impl)
            self.metronome = None;
        }
        if let Some(m) = &self.metronome {
            let enabled =
                self.metronome_enabled && self.start_time.is_some() && self.scrolling_active;
            m.set_params(
                self.tempo_bpm,
                self.time_sig_a as usize,
                self.time_sig_b,
                enabled,
            );
        }

        // Spacebar toggles recording (only when a port is open). Use event-based detection for compatibility across egui versions.
        let space_pressed = ctx.input(|i| i.key_pressed(egui::Key::Space));

        // Keyboard shortcuts (use `command` so Ctrl on Win/Linux and Cmd on macOS both work)
        let new_pressed = ctx.input(|i| i.key_pressed(egui::Key::N) && i.modifiers.command);
        let open_pressed = ctx.input(|i| i.key_pressed(egui::Key::O) && i.modifiers.command);
        let save_pressed =
            ctx.input(|i| i.key_pressed(egui::Key::S) && i.modifiers.command && !i.modifiers.shift);
        let save_as_pressed =
            ctx.input(|i| i.key_pressed(egui::Key::S) && i.modifiers.command && i.modifiers.shift);

        if new_pressed {
            self.handle_new();
            ctx.request_repaint();
        }
        if open_pressed {
            self.handle_open_file();
            ctx.request_repaint();
        }
        if save_pressed {
            self.handle_save();
            ctx.request_repaint();
        }
        if save_as_pressed {
            self.handle_save_as();
            ctx.request_repaint();
        }

        if space_pressed && self.connection.is_some() {
            let is_recording = self.start_time.is_some() && self.scrolling_active;
            if is_recording {
                self.stop_recording();
            } else {
                self.start_recording(Instant::now());
            }
            ctx.request_repaint();
        }

        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // Menu bar for settings
            let mut pending_algo_set: Option<(usize, String, String)> = None;
        egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    // Platform-sensitive shortcut labels (use symbols on macOS)
                    let new_shortcut = if cfg!(target_os = "macos") { "⌘N" } else { "Ctrl+N" };
                    let open_shortcut = if cfg!(target_os = "macos") { "⌘O" } else { "Ctrl+O" };
                    let save_shortcut = if cfg!(target_os = "macos") { "⌘S" } else { "Ctrl+S" };
                    let save_as_shortcut = if cfg!(target_os = "macos") { "⌘⇧S" } else { "Ctrl+Shift+S" };

                    // Compute an approximate width based on character counts and font sizes so the menu width
                    // matches the widest item (approximation). Adjust constants below if you want tighter/looser sizing.
                    let label_font = egui::TextStyle::Button.resolve(ui.style());
                    let shortcut_font_size = if cfg!(target_os = "macos") {
                        label_font.size * 0.95
                    } else {
                        label_font.size * 0.90
                    };
                    let approx_char_width = |ch_count: usize, font_size: f32| -> f32 {
                        ch_count as f32 * font_size * 0.55
                    };
                    let gap = 24.0; // space between label and shortcut
                    let padding = 12.0 * 2.0; // left + right padding estimate
                    let w_new = approx_char_width("New".chars().count(), label_font.size)
                        + approx_char_width(new_shortcut.chars().count(), shortcut_font_size)
                        + gap
                        + padding;
                    let w_open = approx_char_width("Open".chars().count(), label_font.size)
                        + approx_char_width(open_shortcut.chars().count(), shortcut_font_size)
                        + gap
                        + padding;
                    let w_save = approx_char_width("Save".chars().count(), label_font.size)
                        + approx_char_width(save_shortcut.chars().count(), shortcut_font_size)
                        + gap
                        + padding;
                    let w_save_as = approx_char_width("Save as".chars().count(), label_font.size)
                        + approx_char_width(save_as_shortcut.chars().count(), shortcut_font_size)
                        + gap
                        + padding;
                    let menu_width = w_open.max(w_save).max(w_save_as).max(w_new).max(120.0).min(420.0);

                    if menu_item_with_shortcut(ui, "New", new_shortcut, Some(menu_width)).clicked() {
                        self.handle_new();
                    }
                    if menu_item_with_shortcut(ui, "Open", open_shortcut, Some(menu_width)).clicked() {
                        self.handle_open_file();
                    }
                    ui.separator();
                    if menu_item_with_shortcut(ui, "Save", save_shortcut, Some(menu_width)).clicked() {
                        self.handle_save();
                    }
                    if menu_item_with_shortcut(
                        ui,
                        "Save as",
                        save_as_shortcut,
                        Some(menu_width),
                    )
                    .clicked()
                    {
                        self.handle_save_as();
                    }
                });
                ui.menu_button("Algorithm", |ui| {
                    for (i, algo) in self.algos.iter().enumerate() {
                        let algoname = algo.file_stem().unwrap_or_else(|| algo.name.clone());
                        ui.menu_button(&algoname, |ui| {
                            if let Some(opt_names) = algo.get_cached_option_names() {
                                if opt_names.is_empty() {
                                    ui.label("(no options)");
                                } else {
                                    for key in opt_names {
                                        ui.menu_button(&key, |ui| {
                                            if let Some(vals) = algo.get_cached_options_for(&key) {
                                                // current value from plugin; if empty and plugin provided possible values,
                                                // pre-select the first value and schedule setting it so the UI reflects a sensible initial state.
                                                let mut cur = algo.get_cached_option_value(&key).unwrap_or_default();
                                                if cur.is_empty() && !vals.is_empty() {
                                                    let default = vals[0].clone();
                                                    pending_algo_set = Some((i, key.clone(), default.clone()));
                                                    cur = default;
                                                }
                                                for val in vals {
                                                    if ui.selectable_label(cur == val, &val).clicked() {
                                                        pending_algo_set = Some((i, key.clone(), val.clone()));
                                                    }
                                                }
                                            } else {
                                                ui.label("(no values)");
                                            }
                                        });
                                    }
                                }
                            } else {
                                ui.label("(no options)");
                            }
                        });
                    }
                });

                ui.menu_button("Settings", |ui| {
                    ui.separator();
                    ui.label("Scroll Mode:");
                    ui.radio_value(&mut self.scroll_mode, ScrollMode::Smooth, "Smooth");
                    ui.radio_value(&mut self.scroll_mode, ScrollMode::Bar, "Bar (per measure)");
                    ui.separator();
                    // Hover hint (1s delay) explains BPM semantics
                    ui.scope(|ui| {
                        let mut style = ui.ctx().style().as_ref().clone();
                        style.interaction.tooltip_delay = 1.0;
                        ui.set_style(style);
                        ui.add(egui::Slider::new(&mut self.tempo_bpm, 40..=240).text("BPM"))
                            .on_hover_text(
                                "BPM counts quarter notes. Example: 6/8 @100 -> 200 eighths/min",
                            );
                    });
                    ui.add(egui::Slider::new(&mut self.measures, 1..=4).text("Measures"));

                    ui.separator();
                    ui.horizontal(|ui| {
                        ui.label("Time signature");
                        ui.add(
                            egui::DragValue::new(&mut self.time_sig_a)
                                .range(1..=16)
                                .speed(0.25),
                        );
                        ui.label("/");
                        let prev_b = self.time_sig_b;
                        ui.add(
                            egui::DragValue::new(&mut self.time_sig_b)
                                .range(2..=16)
                                .speed(0.25),
                        );
                        if self.time_sig_b != prev_b && !matches!(self.time_sig_b, 2 | 4 | 8 | 16) {
                            // snap denominator to the nearest allowed value
                            let allowed = [2_u32, 4, 8, 16];
                            if let Some(&closest) = allowed
                                .iter()
                                .min_by_key(|v| (self.time_sig_b as i32 - **v as i32).abs())
                            {
                                self.time_sig_b = closest;
                            }
                        }
                    });

                    ui.checkbox(&mut self.power_save_mode, "Power Save Mode")
                        .on_hover_text("When enabled, pause repaint when no notes are active to save CPU (default: off).");
                    ui.checkbox(&mut self.metronome_enabled, "Metronome")
                        .on_hover_text("When enabled, play a click sound on every beat of each measure (n beats per measure).");

                });
            });

            if let Some((idx, key, val)) = pending_algo_set.take() {
                if let Some(algo) = self.algos.get(idx) {
                    match algo.set_option_value(&key, &val) {
                        Ok(()) => self.status = format!("{}: {} = {}", algo.file_stem().unwrap_or_else(|| algo.name.clone()), key, val),
                        Err(e) => self.status = format!("Failed to set {}: {}", key, e),
                    }
                }
            }

            ui.horizontal(|ui| {
                ui.label("MIDI Input:");
                ComboBox::from_label("")
                    .selected_text(
                        self.selected
                            .and_then(|i| self.ports.get(i))
                            .cloned()
                            .unwrap_or("(none)".to_owned()),
                    )
                    .show_ui(ui, |ui| {
                        for (i, name) in self.ports.iter().enumerate() {
                            ui.selectable_value(&mut self.selected, Some(i), name);
                        }
                    });
                if ui.button("Refresh").clicked() {
                    self.refresh_ports();
                }

                if self.connection.is_some() {
                    if ui.button("Close").clicked() {
                        self.close_connection();
                    }
                } else {
                    if ui.button("Open").clicked() {
                        self.open_selected(ctx);
                    }
                }
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            let bottom_padding: f32 = 36.0; // leave space so the bottom panel doesn't cover the last line

            // Two independent panes with a draggable vertical splitter
            // We'll implement the splitter manually so it behaves like a normal IDE splitter.
            // Left = log, handle = draggable, Right = piano roll
            let total_width = ui.available_width();
            let row_h: f32 = 22.0;
            let height = 12.0 * row_h + 24.0; // piano roll vertical size (12 rows + header)
            let handle_w: f32 = 6.0;
            // ensure fraction exists and stays within min..max
            self.log_width_frac = self.log_width_frac.clamp(0.12_f32, 0.88_f32);

            let left_w = (total_width - handle_w) * self.log_width_frac;
            let right_w = (total_width - handle_w) - left_w;

            // Layout horizontally so panes are side-by-side
            ui.horizontal(|ui_row| {
                // LEFT PANE (Log)
                ui_row.allocate_ui_with_layout(
                    egui::vec2(left_w, height),
                    egui::Layout::top_down(egui::Align::Min),
                    |ui_left| {
                        ui_left.horizontal(|ui| {
                            ui.heading("MIDI Event History");
                            // push the toggle to the right but keep it inside the available area
                            let sz: f32 = 26.0;
                            let rem = ui.available_width();
                            let push = (rem - sz - 6.0 - 6.0).max(0.0); // left margin 6.0, right margin 6.0
                            ui.add_space(push);

                            let (rect, resp) =
                                ui.allocate_exact_size(egui::vec2(sz, sz), egui::Sense::click());
                            let resp = resp.on_hover_text("Auto-scroll log");
                            // draw background indicating state
                            let painter = ui.painter_at(rect);
                            let bg = if self.log_auto_scroll {
                                egui::Color32::from_rgb(60, 145, 60)
                            } else {
                                egui::Color32::from_rgb(70, 70, 70)
                            };
                            painter.rect_filled(rect, 4.0, bg);
                            // icon
                            painter.text(
                                rect.center(),
                                egui::Align2::CENTER_CENTER,
                                "🔁",
                                egui::FontId::proportional(14.0),
                                egui::Color32::WHITE,
                            );

                            if resp.clicked() {
                                self.log_auto_scroll = !self.log_auto_scroll;
                                if self.log_auto_scroll && !self.log.is_empty() {
                                    self.log_pending_scroll = true;
                                    ui.ctx().request_repaint();
                                }
                            }
                        });
                        ui_left.separator();
                        let log_scroll_output = egui::ScrollArea::vertical()
                            .stick_to_bottom(self.log_auto_scroll)
                            .auto_shrink([false; 2])
                            .show(ui_left, |ui_left| {
                                let mut line_rects: Vec<egui::Rect> = Vec::new();
                                for line in &self.log {
                                    let resp = ui_left.label(line);
                                    line_rects.push(resp.rect);
                                }
                                ui_left.add_space(bottom_padding);

                                // If a forced scroll was requested (new entries while auto-scroll enabled), scroll to the last line
                                if self.log_pending_scroll {
                                    if let Some(last_rect) = line_rects.last() {
                                        ui_left.scroll_to_rect(*last_rect, Some(egui::Align::Max));
                                        ui_left.ctx().request_repaint();
                                    }
                                    self.log_pending_scroll = false;
                                }
                            });

                        // Detect user manual scrolling (scrolling up) and disable auto-scroll
                        if self.log_auto_scroll {
                            let current_offset = log_scroll_output.state.offset.y;
                            if let Some(last_offset) = self.last_log_scroll_offset {
                                // If user scrolled up (offset decreased) and it's not due to pending scroll, disable auto-scroll
                                if current_offset < last_offset - 1.0 {
                                    self.log_auto_scroll = false;
                                }
                            }
                            self.last_log_scroll_offset = Some(current_offset);
                        } else {
                            // When auto-scroll is off, still track offset for when it's re-enabled
                            self.last_log_scroll_offset = Some(log_scroll_output.state.offset.y);
                        }
                    },
                );

                // Handle (draggable)
                let (handle_rect, handle_resp) =
                    ui_row.allocate_exact_size(egui::vec2(handle_w, height), egui::Sense::drag());

                // change cursor when hovering / dragging
                if handle_resp.hovered() || handle_resp.dragged() {
                    ui_row
                        .ctx()
                        .output_mut(|o| o.cursor_icon = egui::CursorIcon::ResizeHorizontal);
                }

                if handle_resp.dragged() {
                    let delta = handle_resp.drag_delta().x;
                    // update fraction using full area width available to panes (exclude handle)
                    self.log_width_frac += delta / (total_width - handle_w);
                    self.log_width_frac = self.log_width_frac.clamp(0.12_f32, 0.88_f32);
                }

                // draw handle visuals
                {
                    let handle_painter = ui_row.painter();
                    handle_painter.rect_filled(handle_rect, 0.0, egui::Color32::from_gray(38));
                    let mid_x = handle_rect.center().x;
                    let top = handle_rect.top() + 8.0;
                    let bottom = handle_rect.bottom() - 8.0;
                    handle_painter.line_segment(
                        [egui::pos2(mid_x, top), egui::pos2(mid_x, bottom)],
                        egui::Stroke::new(1.0, egui::Color32::from_gray(110)),
                    );
                }

                // RIGHT PANE (Piano roll)
                ui_row.allocate_ui_with_layout(
                    egui::vec2(right_w, height),
                    egui::Layout::top_down(egui::Align::Min),
                    |ui_right| {
                        // Heading with record button on the right
                        ui_right.horizontal(|ui_h| {
                            ui_h.heading("Piano Roll");

                            // Circular record button to the right of the title
                            // Match chord auto-scroll sizing/margins: 26px button, 12px right margin
                            let sz: f32 = 26.0;
                            let rem = ui_h.available_width();
                            let right_margin: f32 = 12.0;
                            let push = (rem - (sz + right_margin)).max(0.0);
                            ui_h.add_space(push);

                            let (rect, resp) =
                                ui_h.allocate_exact_size(egui::vec2(sz, sz), egui::Sense::click());
                            let resp = resp.on_hover_text("Record (Space)");

                            // determine state and colors
                            let is_open = self.connection.is_some();
                            let is_recording = self.start_time.is_some() && self.scrolling_active;

                            let painter = ui_h.painter_at(rect);
                            let center = rect.center();
                            let radius = sz * 0.5;

                            if !is_open {
                                // disabled gray circle
                                painter.circle_filled(center, radius, egui::Color32::from_gray(70));
                                painter.text(
                                    center,
                                    egui::Align2::CENTER_CENTER,
                                    "●",
                                    egui::FontId::proportional(14.0),
                                    egui::Color32::from_gray(140),
                                );
                            } else if is_recording {
                                // active: red circle with white stop square
                                painter.circle_filled(
                                    center,
                                    radius,
                                    egui::Color32::from_rgb(200, 40, 40),
                                );
                                let sq = sz * 0.45;
                                let sq_rect =
                                    egui::Rect::from_center_size(center, egui::vec2(sq, sq));
                                painter.rect_filled(sq_rect, 3.0, egui::Color32::WHITE);
                            } else {
                                // armed/ready: dark red circle with white dot
                                painter.circle_filled(
                                    center,
                                    radius,
                                    egui::Color32::from_rgb(170, 50, 50),
                                );
                                painter.circle_filled(center, radius * 0.42, egui::Color32::WHITE);
                            }

                            if is_open && resp.clicked() {
                                if is_recording {
                                    self.stop_recording();
                                } else {
                                    self.start_recording(Instant::now());
                                }
                                ctx.request_repaint();
                            }
                        });

                        ui_right.separator();

                        let (r, resp) = ui_right
                            .allocate_exact_size(egui::vec2(right_w, height), egui::Sense::drag());
                        let painter = ui_right.painter_at(r);

                        // background
                        painter.rect_filled(r, 0.0, egui::Color32::from_gray(30));

                        let grid_left = r.left();
                        let grid_right = r.right();
                        let grid_top = r.top() + 20.0; // leave header space
                        let grid_bottom = grid_top + row_h * 12.0;

                        // draw horizontal rows and labels (bottom to top C..B)
                        let labels = [
                            "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
                        ];
                        for row in 0..12 {
                            let y = grid_bottom - (row as f32 + 1.0) * row_h;

                            // row background: darker for black keys, lighter for white keys
                            let row_rect = egui::Rect::from_min_max(
                                egui::pos2(grid_left, y),
                                egui::pos2(grid_right, y + row_h),
                            );
                            let is_black = matches!(row, 1 | 3 | 6 | 8 | 10);
                            let bg = if is_black {
                                egui::Color32::from_rgb(36, 40, 45) // darker
                            } else {
                                egui::Color32::from_rgb(52, 56, 60) // lighter
                            };
                            painter.rect_filled(row_rect, 0.0, bg);

                            // separator line
                            painter.line_segment(
                                [egui::pos2(grid_left, y), egui::pos2(grid_right, y)],
                                egui::Stroke::new(1.0, egui::Color32::from_gray(80)),
                            );

                            // label on left
                            let text_pos = egui::pos2(grid_left + 6.0, y + row_h * 0.18);
                            painter.text(
                                text_pos,
                                egui::Align2::LEFT_TOP,
                                labels[row as usize],
                                egui::FontId::monospace(12.0),
                                egui::Color32::WHITE,
                            );
                        }

                        // draw vertical beat lines according to the current time signature (a/b)
                        let now = Instant::now();
                        let beats_per_measure: usize = self.time_sig_a as usize; // a
                        let total_beats: usize = (self.measures as usize) * beats_per_measure;
                        let window_beats = total_beats as f32;
                        // BPM is always per quarter note: seconds per quarter
                        let quarter_secs = 60.0_f32 / (self.tempo_bpm as f32);
                        // seconds per beat unit (denominator b)
                        let beat_secs = quarter_secs * (4.0_f32 / self.time_sig_b as f32);
                        let measure_secs = beat_secs * beats_per_measure as f32;
                        let window_secs = measure_secs * (self.measures as f32);

                        if resp.dragged() {
                            let delta_x = resp.drag_delta().x;
                            // dragging right (positive) moves view window to the left (earlier time)
                            let px_per_sec = right_w / window_secs;
                            let dt = delta_x / px_per_sec;
                            if let Some(fve) = self.frozen_view_end {
                                let mut new_fve = if dt >= 0.0 {
                                    fve.checked_sub(std::time::Duration::from_secs_f32(dt))
                                        .unwrap_or(fve)
                                } else {
                                    fve + std::time::Duration::from_secs_f32(-dt)
                                };

                                // Constrain dragging: don't show area before start_time (at left edge)
                                if let Some(start) = self.start_time {
                                    let min_view_end =
                                        start + std::time::Duration::from_secs_f32(window_secs);
                                    if new_fve < min_view_end {
                                        new_fve = min_view_end;
                                    }
                                    let max_view_end =
                                        self.recording_ended_at.unwrap_or_else(Instant::now);
                                    if new_fve > max_view_end {
                                        new_fve = max_view_end;
                                    }
                                }
                                self.frozen_view_end = Some(new_fve);
                                // Ensure we stay in frozen mode (though dragged() implies it)
                                self.scrolling_active = false;
                            }
                        }

                        // Map time signature to hierarchical beats for rhythm weighting.
                        // Special-case common compound time 6/8 -> [2,3], otherwise use top-level beats = a
                        let beats_vec: Vec<i32> = if self.time_sig_a == 4 {
                            vec![2, 2, 2]
                        } else if self.time_sig_a == 9 {
                            vec![3, 3]
                        } else if self.time_sig_a == 6 && self.time_sig_b == 8 {
                            vec![2, 3]
                        } else {
                            vec![beats_per_measure as i32]
                        };

                        // Determine view_end: if scrolling is active compute normally, otherwise freeze to `frozen_view_end` (or now)
                        let view_end = if self.scrolling_active {
                            match self.scroll_mode {
                                ScrollMode::Smooth => now,
                                ScrollMode::Bar => {
                                    if let Some(start) = self.start_time {
                                        let elapsed = now.duration_since(start).as_secs_f32();
                                        let measure_idx = (elapsed / measure_secs).floor();
                                        start
                                            + std::time::Duration::from_secs_f32(
                                                (measure_idx + 1.0) * measure_secs,
                                            )
                                    } else {
                                        // before the first note: keep a static window (no quantized jump)
                                        now
                                    }
                                }
                            }
                        } else {
                            self.frozen_view_end.unwrap_or(now)
                        };

                        match self.scroll_mode {
                            ScrollMode::Smooth => {
                                if let Some(start) = self.start_time {
                                    // Align beat lines to the global beat grid anchored at start_time and
                                    // position them by their exact timestamp so they move smoothly.
                                    let elapsed = view_end.duration_since(start).as_secs_f32();
                                    let last_beat_idx = (elapsed / beat_secs).floor() as isize;
                                    let first_beat_idx =
                                        ((elapsed - window_secs) / beat_secs).floor() as isize;

                                    for beat_idx in first_beat_idx..=last_beat_idx {
                                        // beat_age = how long ago this beat occurred relative to view_end
                                        let beat_age = elapsed - (beat_idx as f32) * beat_secs;
                                        if beat_age < 0.0 || beat_age > window_secs {
                                            continue;
                                        }
                                        let t = (1.0 - (beat_age / window_secs))
                                            .clamp(0.0_f32, 1.0_f32);
                                        let x = grid_left + t * (grid_right - grid_left);

                                        let is_measure =
                                            (beat_idx as isize) % (beats_per_measure as isize) == 0;
                                        if is_measure {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    2.0,
                                                    egui::Color32::from_rgb(180, 200, 255),
                                                ),
                                            );

                                            // measure number relative to start_time (1-indexed)
                                            let measure_num = (beat_idx as isize
                                                / beats_per_measure as isize)
                                                + 1;
                                            painter.text(
                                                egui::pos2(x + 4.0, grid_top - 16.0),
                                                egui::Align2::LEFT_TOP,
                                                format!("{}", measure_num),
                                                egui::FontId::monospace(12.0),
                                                egui::Color32::from_rgb(200, 220, 255),
                                            );
                                        } else {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    1.0,
                                                    egui::Color32::from_gray(100),
                                                ),
                                            );
                                        }
                                    }
                                } else {
                                    // before the first note: static grid like Bar-mode fallback
                                    for b in 0..=total_beats {
                                        let x = grid_left
                                            + (b as f32 / window_beats) * (grid_right - grid_left);
                                        let is_measure = b % beats_per_measure == 0;
                                        if is_measure {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    2.0,
                                                    egui::Color32::from_rgb(180, 200, 255),
                                                ),
                                            );

                                            let measure_num = b / beats_per_measure + 1;
                                            painter.text(
                                                egui::pos2(x + 4.0, grid_top - 16.0),
                                                egui::Align2::LEFT_TOP,
                                                format!("{}", measure_num),
                                                egui::FontId::monospace(12.0),
                                                egui::Color32::from_rgb(200, 220, 255),
                                            );
                                        } else {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    1.0,
                                                    egui::Color32::from_gray(100),
                                                ),
                                            );
                                        }
                                    }
                                }
                            }
                            ScrollMode::Bar => {
                                if let Some(start) = self.start_time {
                                    // when started, quantize view_end and draw beat lines accordingly so the grid jumps per measure
                                    let elapsed = view_end.duration_since(start).as_secs_f32();
                                    let last_beat_idx = (elapsed / beat_secs).floor() as isize;
                                    let first_beat_idx =
                                        ((elapsed - window_secs) / beat_secs).floor() as isize;

                                    for beat_idx in first_beat_idx..=last_beat_idx {
                                        let beat_age = elapsed - (beat_idx as f32) * beat_secs;
                                        if beat_age < 0.0 || beat_age > window_secs {
                                            continue;
                                        }
                                        let t = (1.0 - (beat_age / window_secs))
                                            .clamp(0.0_f32, 1.0_f32);
                                        let x = grid_left + t * (grid_right - grid_left);

                                        let is_measure =
                                            (beat_idx as isize) % (beats_per_measure as isize) == 0;
                                        if is_measure {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    2.0,
                                                    egui::Color32::from_rgb(180, 200, 255),
                                                ),
                                            );

                                            let measure_num = (beat_idx as isize
                                                / beats_per_measure as isize)
                                                + 1;
                                            painter.text(
                                                egui::pos2(x + 4.0, grid_top - 16.0),
                                                egui::Align2::LEFT_TOP,
                                                format!("{}", measure_num),
                                                egui::FontId::monospace(12.0),
                                                egui::Color32::from_rgb(200, 220, 255),
                                            );
                                        } else {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    1.0,
                                                    egui::Color32::from_gray(100),
                                                ),
                                            );
                                        }
                                    }
                                } else {
                                    // before the first note: static window grid
                                    for b in 0..=total_beats {
                                        let x = grid_left
                                            + (b as f32 / window_beats) * (grid_right - grid_left);
                                        let is_measure = b % beats_per_measure == 0;
                                        if is_measure {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    2.0,
                                                    egui::Color32::from_rgb(180, 200, 255),
                                                ),
                                            );

                                            let measure_num = b / beats_per_measure + 1;
                                            painter.text(
                                                egui::pos2(x + 4.0, grid_top - 16.0),
                                                egui::Align2::LEFT_TOP,
                                                format!("{}", measure_num),
                                                egui::FontId::monospace(12.0),
                                                egui::Color32::from_rgb(200, 220, 255),
                                            );
                                        } else {
                                            painter.line_segment(
                                                [
                                                    egui::pos2(x, grid_top),
                                                    egui::pos2(x, grid_bottom),
                                                ],
                                                egui::Stroke::new(
                                                    1.0,
                                                    egui::Color32::from_gray(100),
                                                ),
                                            );
                                        }
                                    }
                                }
                            }
                        }

                        // chord generation per measure
                        if let Some(start) = self.start_time.filter(|_| self.scrolling_active) {
                            // Use actual current time (now) to determine which measures are active.
                            // Using `view_end` (quantized in Bar mode) can cause an off-by-one where the view
                            // is already snapped to the end of the first measure and we end up generating
                            // both measure 1 and 2 at the same time. Using `now` avoids that.
                            let elapsed = now.duration_since(start).as_secs_f32();
                            // Convert to 1-based measure index so the first active measure is measure 1
                            let current_measure_idx = (elapsed / measure_secs).floor() as u32 + 1;
                            self.ensure_chords_up_to(current_measure_idx);
                        }

                        // draw hits as rectangles spanning start..end (or start..view_end if active)
                        for hit in &self.note_hits {
                            // Need start anchor to convert normalized measure times to Instants
                            let start_anchor = if let Some(sa) = self.start_time {
                                sa
                            } else {
                                continue;
                            };
                            let hit_start_inst = start_anchor
                                + std::time::Duration::from_secs_f32(hit.start * measure_secs);
                            if hit_start_inst > view_end {
                                continue;
                            }
                            // determine ages relative to view_end
                            let start_age = view_end.duration_since(hit_start_inst).as_secs_f32();
                            let end_age = if let Some(e) = hit.end {
                                let hit_end_inst = start_anchor
                                    + std::time::Duration::from_secs_f32(e * measure_secs);
                                if hit_end_inst > view_end {
                                    0.0
                                } else {
                                    view_end.duration_since(hit_end_inst).as_secs_f32()
                                }
                            } else {
                                0.0
                            };

                            // skip completely out-of-window elements
                            let end_age_for_check = if let Some(e) = hit.end {
                                let hit_end_inst = start_anchor
                                    + std::time::Duration::from_secs_f32(e * measure_secs);
                                view_end.duration_since(hit_end_inst).as_secs_f32()
                            } else {
                                start_age
                            };

                            if start_age > window_secs && end_age_for_check > window_secs {
                                continue;
                            }

                            // normalized positions in [0..1]
                            let t_start = (1.0 - (start_age / window_secs)).clamp(0.0_f32, 1.0_f32);
                            let t_end = (1.0 - (end_age / window_secs)).clamp(0.0_f32, 1.0_f32);
                            let x_start = grid_left + t_start * (grid_right - grid_left);
                            let x_end = grid_left + t_end * (grid_right - grid_left);
                            let x0 = x_start.min(x_end);
                            let x1 = x_start.max(x_end);

                            let pitch_class = (hit.pitch % 12) as usize;
                            let row = pitch_class;
                            let y = grid_bottom - (row as f32 + 1.0) * row_h;

                            let rect = egui::Rect::from_min_max(
                                egui::pos2(x0, y + 2.0),
                                egui::pos2(x1.max(x0 + 4.0), y + row_h - 2.0),
                            );

                            // alpha fades based on end age (active notes show fully opaque)
                            let age_ref = end_age;
                            let alpha = ((1.0 - (age_ref / window_secs)).clamp(0.0_f32, 1.0_f32)
                                * 220.0) as u8;
                            let col = egui::Color32::from_rgba_premultiplied(
                                (80 + hit.velocity) as u8,
                                200,
                                80,
                                alpha,
                            );

                            painter.rect_filled(rect, 2.0, col);

                            // Compute rhythm weight in-measure and draw it at the left edge of the note.
                            if let Some(start_anchor) = self.start_time {
                                // start position within its measure (seconds)
                                let s_elapsed = hit.start * measure_secs;
                                let s_measure_idx = (s_elapsed / measure_secs).floor() as i64;
                                let s_in_measure = ((s_elapsed
                                    - (s_measure_idx as f32) * measure_secs)
                                    / measure_secs)
                                    .clamp(0.0_f32, 1.0_f32);

                                // end position: either explicit end, or current time's in-measure position
                                let (e_in_measure, e_measure_idx) = if let Some(e) = hit.end {
                                    let e_elapsed = e * measure_secs;
                                    let e_measure_idx = (e_elapsed / measure_secs).floor() as i64;
                                    let e_in_measure = ((e_elapsed
                                        - (e_measure_idx as f32) * measure_secs)
                                        / measure_secs)
                                        .clamp(0.0_f32, 1.0_f32);
                                    (e_in_measure, e_measure_idx)
                                } else {
                                    let now_elapsed =
                                        now.duration_since(start_anchor).as_secs_f32();
                                    let now_measure_idx =
                                        (now_elapsed / measure_secs).floor() as i64;
                                    let now_in_measure = ((now_elapsed
                                        - (now_measure_idx as f32) * measure_secs)
                                        / measure_secs)
                                        .clamp(0.0_f32, 1.0_f32);
                                    (now_in_measure, now_measure_idx)
                                };

                                let a = s_in_measure as f64;
                                let b = if e_measure_idx != s_measure_idx {
                                    1.0
                                } else {
                                    e_in_measure as f64
                                };

                                let weight = if b > a {
                                    crate::rhythm::weight::rhythm_weight(a, b, &beats_vec)
                                } else {
                                    0.0
                                };

                                // Draw weight text at the left side with fully transparent background
                                let label = format!("{:.3}", weight);
                                let label_pos = egui::pos2(x0 + 4.0, y + 4.0);
                                painter.text(
                                    label_pos,
                                    egui::Align2::LEFT_TOP,
                                    label,
                                    egui::FontId::monospace(12.0),
                                    egui::Color32::WHITE,
                                );
                            }
                        }
                    },
                );
            }); // end ui.horizontal

            // Request periodic repaints for animation. When Power Save Mode is enabled, only repaint when there are active or recent hits.
            if !self.power_save_mode || !self.note_hits.is_empty() {
                ctx.request_repaint_after(std::time::Duration::from_millis(40));
            }

            // Chord window (full width) — displays horizontally scrollable chord cards
            ui.separator();
            ui.horizontal(|ui| {
                ui.heading("Chord");
                // place the algorithm selector immediately to the right of the title
                ui.add_space(8.0);

                ui.label("Algorithm:");
                let selected_text = self
                    .selected_algo_idx
                    .and_then(|idx| self.algos.get(idx).and_then(|a| a.file_stem()))
                    .unwrap_or("(none)".to_string());

                egui::ComboBox::from_label("")
                    .selected_text(selected_text)
                    .show_ui(ui, |ui| {
                        ui.selectable_value(&mut self.selected_algo_idx, None, "(none)");
                        for (i, algo) in self.algos.iter().enumerate() {
                            let name = algo.file_stem().unwrap_or_else(|| algo.name.clone());
                            ui.selectable_value(&mut self.selected_algo_idx, Some(i), name);
                        }
                    });

                if ui.button("Refresh").clicked() {
                    self.refresh_algos();
                }

                // Regenerate: rebuild all chord cards from piano notes (re-run generation using same input)
                if ui.button("Regenerate").clicked() {
                    // If no notes, do nothing meaningful
                    if self.note_hits.is_empty() {
                        self.status = "Regenerate: no piano notes available".to_string();
                    } else {
                        // If the selected algorithm provides reset_state(), call it before regenerating
                        if let Some(idx) = self.selected_algo_idx {
                            if let Some(algo) = self.algos.get(idx) {
                                if algo.has_reset_state() {
                                    match algo.reset_state() {
                                        Ok(()) => {
                                            self.status = format!(
                                                "{}: reset_state()",
                                                algo.file_stem()
                                                    .unwrap_or_else(|| algo.name.clone())
                                            );
                                        }
                                        Err(e) => {
                                            self.status = format!(
                                                "Failed to reset {}: {}",
                                                algo.file_stem()
                                                    .unwrap_or_else(|| algo.name.clone()),
                                                e
                                            );
                                        }
                                    }
                                }
                            }
                        }

                        // Reset chords to initial state
                        self.chords.clear();
                        self.chords
                            .push((1, PitchOrderedSet::new(), std::time::Duration::ZERO));

                        // Compute anchor start/end similar to save_piano_to_file
                        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
                        let measure_secs = beat_secs * (self.time_sig_a as f32);

                        // Determine anchor instants for regeneration. If we don't have a start_time, derive one
                        // such that the earliest normalized hit maps to a time in the past (preserving relative offsets).

                        let start_anchor = if let Some(s) = self.start_time {
                            s
                        } else {
                            let earliest_meas = self
                                .note_hits
                                .iter()
                                .map(|h| h.start)
                                .fold(std::f32::INFINITY, |m, v| m.min(v));
                            Instant::now()
                                - std::time::Duration::from_secs_f32(earliest_meas * measure_secs)
                        };

                        let end_anchor = if let Some(ea) = self.recording_ended_at {
                            ea
                        } else {
                            let latest_meas = self
                                .note_hits
                                .iter()
                                .map(|h| h.end.unwrap_or(h.start))
                                .fold(0.0_f32, |m, v| m.max(v));
                            start_anchor
                                + std::time::Duration::from_secs_f32(latest_meas * measure_secs)
                        };

                        let total_secs = if end_anchor > start_anchor {
                            (end_anchor - start_anchor).as_secs_f32()
                        } else {
                            0.0
                        };

                        // Determine how many source measures of notes exist
                        let measures_present = if total_secs <= 0.0 {
                            1u32
                        } else {
                            (total_secs / measure_secs).ceil() as u32
                        };

                        // We want to generate chords at each measure end; ensure_chords_up_to expects a target measure index
                        // and generates chords up to that measure. Since chord for measure (n+1) is generated from notes in measure n,
                        // to produce chords for ends of measures 1..measures_present we call up_to = measures_present + 1
                        let target = measures_present.saturating_add(1);

                        // Generate
                        self.ensure_chords_up_to(target);
                        ui.ctx().request_repaint();

                        self.status = format!("Regenerated chords up to measure {}", target);
                    }
                }

                // push the auto-scroll toggle to the far right of the header, leaving a small right margin
                let sz: f32 = 26.0;
                let rem_after = ui.available_width();
                let right_margin: f32 = 12.0; // px of spacing to keep from the right edge
                let push = (rem_after - (sz + right_margin)).max(0.0);
                ui.add_space(push);

                let (rect, resp) = ui.allocate_exact_size(egui::vec2(sz, sz), egui::Sense::click());
                let resp = resp.on_hover_text("Auto-scroll chords");
                // draw background indicating state
                let painter = ui.painter_at(rect);
                let bg = if self.chords_auto_scroll {
                    egui::Color32::from_rgb(60, 145, 60)
                } else {
                    egui::Color32::from_rgb(70, 70, 70)
                };
                painter.rect_filled(rect, 4.0, bg);
                // icon
                painter.text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    "🔁",
                    egui::FontId::proportional(14.0),
                    egui::Color32::WHITE,
                );

                if resp.clicked() {
                    self.chords_auto_scroll = !self.chords_auto_scroll;
                    if self.chords_auto_scroll && !self.chords.is_empty() {
                        self.chord_pending_scroll_index = Some(self.chords.len() - 1);
                        ui.ctx().request_repaint();
                    }
                }
            });
            let card_h: f32 = 72.0;
            let card_w: f32 = 140.0;
            let chord_scroll_output = egui::ScrollArea::horizontal()
                .stick_to_right(self.chords_auto_scroll)
                .max_height(card_h + 16.0)
                .show(ui, |ui| {
                    ui.horizontal(|ui| {
                        let mut card_rects: Vec<egui::Rect> = Vec::new();
                        for (_i, (measure, chord, dur)) in self.chords.iter().enumerate() {
                            let (rect, _resp) = ui.allocate_exact_size(
                                egui::vec2(card_w, card_h),
                                egui::Sense::hover(),
                            );
                            let painter = ui.painter_at(rect);
                            painter.rect_filled(
                                rect.shrink(4.0),
                                6.0,
                                egui::Color32::from_rgb(60, 60, 70),
                            );
                            painter.text(
                                rect.left_top() + egui::vec2(8.0, 6.0),
                                egui::Align2::LEFT_TOP,
                                format!("{}.", measure),
                                egui::FontId::monospace(10.0),
                                egui::Color32::from_gray(200),
                            );
                            painter.text(
                                rect.center(),
                                egui::Align2::CENTER_CENTER,
                                chord.to_string(),
                                egui::FontId::proportional(18.0),
                                egui::Color32::WHITE,
                            );
                            // Draw the measured duration in bottom-right with adaptive units
                            painter.text(
                                rect.right_bottom() - egui::vec2(6.0, 6.0),
                                egui::Align2::RIGHT_BOTTOM,
                                format_duration_adaptive(*dur),
                                egui::FontId::monospace(10.0),
                                egui::Color32::from_gray(180),
                            );
                            card_rects.push(rect);
                        }
                        if let Some(idx) = self.chord_pending_scroll_index.take() {
                            if idx < card_rects.len() {
                                ui.scroll_to_rect(card_rects[idx], Some(egui::Align::Max));
                                ui.ctx().request_repaint();
                            }
                        }
                    });
                });

            // Detect user manual scrolling (scrolling left) and disable auto-scroll
            if self.chords_auto_scroll {
                let current_offset = chord_scroll_output.state.offset.x;
                if let Some(last_offset) = self.last_chord_scroll_offset {
                    // If user scrolled left (offset decreased), disable auto-scroll
                    if current_offset < last_offset - 1.0 {
                        self.chords_auto_scroll = false;
                    }
                }
                self.last_chord_scroll_offset = Some(current_offset);
            } else {
                // When auto-scroll is off, still track offset for when it's re-enabled
                self.last_chord_scroll_offset = Some(chord_scroll_output.state.offset.x);
            }

            egui::TopBottomPanel::bottom("bottom_panel").show(ctx, |ui| {
                ui.horizontal(|ui| {
                    // Status on the left
                    ui.label(format!("Status: {}", self.status));
                    ui.separator();
                    ui.label(format!("Ports: {}", self.ports.len()));
                    ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                        ui.label("verichord — MIDI monitor");
                    });
                });
            });
        });
    }
}
