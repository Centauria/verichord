use chrono::Local;
use eframe::{egui, egui::ComboBox};
use midir::{Ignore, MidiInput, MidiInputConnection, MidiOutput, MidiOutputConnection};
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

mod helpers;
mod state;
mod widgets;

use crate::ui::state::{AppState, ChordScrollDirection, ChordUpdateFrequency, ScrollMode};
use crate::ui::widgets::ButtonWithShortcut;

pub struct MidiApp {
    midi_in: MidiInput,
    ports: Vec<String>,
    selected: Option<usize>,
    connection: Option<MidiInputConnection<()>>,
    midi_out: MidiOutput,
    out_ports: Vec<String>,
    out_selected: Option<usize>,
    out_connection: Option<MidiOutputConnection>,
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
    // `chords` stores generated chords per-beat as (measure, beat_index, chord, gen_duration).
    // Each measure now yields `time_sig_a` entries (one per beat).
    chords: Vec<(u32, u32, PitchOrderedSet, Duration)>,
    chords_auto_scroll: bool,
    chords_scroll_direction: ChordScrollDirection,
    chord_pending_scroll_index: Option<usize>,
    save_path: Option<PathBuf>,
    scrolling_active: bool,
    frozen_view_end: Option<Instant>,
    view_frozen_by_user: bool,
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

    // lookahead: delay automatic chord prediction slightly after new measure start
    lookahead_enabled: bool,
    // pending automatic chord generation: (target_measure, ready_at)
    chord_gen_pending: Option<(u32, Instant)>,
    // When true, display an initial N.C. chord card at measure 0
    show_initial_chord: bool,
    // Update frequency for chord generation (Beat = every beat, Measure = once per measure)
    chord_update_frequency: ChordUpdateFrequency,
    // Last window title that was requested to the OS. Used to avoid redundant title updates.
    last_window_title: Option<String>,
    // Playback state
    playback_active: bool,
    playback_start_time: Option<Instant>,
    pub cursor_pos: f32,
    playback_start_cursor: f32,
    playback_viewport_fraction: f32,
}

impl Default for MidiApp {
    fn default() -> Self {
        let midi_in = MidiInput::new("verichord")
            .unwrap_or_else(|_| MidiInput::new("verichord-fallback").unwrap());
        let midi_out = MidiOutput::new("verichord-out")
            .unwrap_or_else(|_| MidiOutput::new("verichord-out-fallback").unwrap());
        let (tx, rx) = mpsc::channel();
        let mut app = MidiApp {
            midi_in,
            midi_out,
            ports: Vec::new(),
            selected: None,
            connection: None,
            out_ports: Vec::new(),
            out_selected: None,
            out_connection: None,
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
            chords: Vec::new(),
            chords_auto_scroll: true,
            chords_scroll_direction: ChordScrollDirection::Horizontal,
            chord_pending_scroll_index: None,
            save_path: None,
            scrolling_active: false,
            frozen_view_end: None,
            view_frozen_by_user: false,
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

            lookahead_enabled: true,
            chord_gen_pending: None,
            show_initial_chord: false,
            chord_update_frequency: ChordUpdateFrequency::Beat,
            last_window_title: None,
            playback_active: false,
            playback_start_time: None,
            cursor_pos: 0.0,
            playback_start_cursor: 0.0,
            playback_viewport_fraction: 0.5,
        };
        app.midi_in.ignore(Ignore::None);
        app.refresh_ports();
        app.refresh_algos();
        app.refresh_output_ports();
        app
    }
}

impl MidiApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut app = Self::default();

        // Temporarily capture saved MIDI names so we can restore selection after we refresh port lists
        let mut saved_midi_in: Option<String> = None;
        let mut saved_midi_out: Option<String> = None;
        if let Some(storage) = cc.storage {
            if let Some(state) = eframe::get_value::<AppState>(storage, eframe::APP_KEY) {
                app.tempo_bpm = state.tempo_bpm;
                app.scroll_mode = state.scroll_mode;
                app.chords_scroll_direction = state.chords_scroll_direction;
                app.log_width_frac = state.log_width_frac;
                app.measures = state.measures;
                app.time_sig_a = state.time_sig_a;
                app.time_sig_b = state.time_sig_b;
                app.metronome_enabled = state.metronome_enabled;
                app.power_save_mode = state.power_save_mode;
                app.lookahead_enabled = state.lookahead_enabled;
                app.selected_algo_idx = state.selected_algo_idx;
                app.show_initial_chord = state.show_initial_chord;
                app.chord_update_frequency = state.chord_update_frequency;

                saved_midi_in = state.midi_in_name.clone();
                saved_midi_out = state.midi_out_name.clone();
            }
        }

        // Refresh ports/algos to ensure valid state even if loaded config is stale
        app.refresh_ports();
        app.refresh_algos();
        app.refresh_output_ports();

        // Restore saved selections by device name if the device is present; otherwise leave selection unchanged
        if let Some(name) = saved_midi_in {
            if let Some(idx) = app.ports.iter().position(|n| n == &name) {
                app.selected = Some(idx);
            }
        }
        if let Some(name) = saved_midi_out {
            if let Some(idx) = app.out_ports.iter().position(|n| n == &name) {
                app.out_selected = Some(idx);
            }
        }

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

    fn refresh_output_ports(&mut self) {
        self.out_ports.clear();
        let list = self.midi_out.ports();
        for p in list {
            match self.midi_out.port_name(&p) {
                Ok(name) => self.out_ports.push(name),
                Err(_) => self.out_ports.push("Unnamed port".to_string()),
            }
        }
        if self.out_selected.is_none() && !self.out_ports.is_empty() {
            self.out_selected = Some(0);
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
                    self.chord_pending_scroll_index = None;
                    self.cursor_pos = 0.0;
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
            // clear any pending automatic chord generation
            self.chord_gen_pending = None;
        } else {
            self.status = "No open connection".to_string();
        }
    }

    pub fn open_selected_output(&mut self) {
        if self.out_connection.is_some() {
            self.status = "Output already open".to_string();
            return;
        }
        if let Some(idx) = self.out_selected {
            let ports = self.midi_out.ports();
            if idx >= ports.len() {
                self.status = "Invalid output selected".to_string();
                return;
            }
            let port = &ports[idx];
            let port_name = self
                .midi_out
                .port_name(port)
                .unwrap_or_else(|_| "Unnamed".to_string());

            let connector = MidiOutput::new("verichord-connection")
                .unwrap_or_else(|_| MidiOutput::new("verichord-conn-fallback").unwrap());
            match connector.connect(port, "verichord-output") {
                Ok(conn) => {
                    self.status = format!("Output Open: {}", port_name);
                    self.out_connection = Some(conn);
                }
                Err(e) => {
                    self.status = format!("Failed to open output: {}", e);
                }
            }
        } else {
            self.status = "No output selected".to_string();
        }
    }

    pub fn close_output_connection(&mut self) {
        if let Some(conn) = self.out_connection.take() {
            drop(conn);
            self.status = "Output closed".to_string();
        } else {
            self.status = "No open output".to_string();
        }
    }

    fn start_recording(&mut self, _ignored_anchor: Instant) {
        // Calculate anchor based on cursor_pos so that 'now' corresponds to cursor_pos
        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
        let measure_secs = beat_secs * (self.time_sig_a as f32);
        let offset = Duration::from_secs_f32(self.cursor_pos * measure_secs);
        let anchor = Instant::now() - offset;

        self.recording_enabled = true;
        self.start_time = Some(anchor);
        self.scrolling_active = true;
        self.frozen_view_end = None;
        self.view_frozen_by_user = false;
        self.chord_gen_pending = None;

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

        // Overwrite mode: Remove/Truncate future notes.
        // 1. Remove notes that start at or after the cursor.
        self.note_hits.retain(|h| h.start < self.cursor_pos);
        // 2. Truncate notes that obtained before but end after cursor.
        for h in self.note_hits.iter_mut() {
            if let Some(end) = h.end {
                if end > self.cursor_pos {
                    h.end = Some(self.cursor_pos);
                    // If we had a way to send NoteOff for this pitch now, we should,
                    // but these consist of recorded data, not necessarily live sound,
                    // unless we are playing back.
                }
            } else {
                // Open-ended note (shouldn't happen in stored hits usually, but for safety)
                h.end = Some(self.cursor_pos);
            }
        }

        // We do typically clear chords/log in a new take, but with punch-in,
        // maybe we should keep previous chords?
        // For simplicity and matching "replace" semantics, we clear `chords`
        // because generation depends on the new context potentially.
        // Actually, if we overwrite, we might invalid old chords.
        // Let's clear chords for now to trigger regeneration.
        self.chords.clear();
        self.chord_pending_scroll_index = None;

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

    fn start_playback(&mut self) {
        if self.playback_active {
            return;
        }
        // Ensure connection is appropriate?
        // User said: "send to MIDI Out. If not specified, do nothing".
        // We allow starting playback even without MIDI Out, just to move cursor.

        // Check if cursor is already at or past the end of the recording.
        // If so, do not start playback (per user request "光标不应该再往前走").
        // We consider the end to be the later of recording_ended_at or the max note end.
        let end_meas_from_notes = self
            .note_hits
            .iter()
            .map(|h| h.end.unwrap_or(h.start))
            .fold(0.0_f32, f32::max);

        // Calculate max measure duration from recording_ended_at (if available and relevant)
        // Since we are about to re-anchor start_time, let's calculate relative to whatever start_time exists now
        // or just use measures.
        // Actually simpler: if we have `recording_ended_at` and `start_time`, we can compute duration.
        // If not, rely on notes.
        let mut max_duration_meas = end_meas_from_notes;

        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
        let measure_secs = beat_secs * (self.time_sig_a as f32);

        if let (Some(s), Some(e)) = (self.start_time, self.recording_ended_at) {
            if e > s {
                let dur_secs = e.duration_since(s).as_secs_f32();
                let dur_meas = dur_secs / measure_secs;
                if dur_meas > max_duration_meas {
                    max_duration_meas = dur_meas;
                }
            }
        }

        // Tolerance: if within small epsilon of end, or past it, don't start.
        if self.cursor_pos >= max_duration_meas - 0.01 {
            return;
        }

        // 1. Calculate and lock the current visual cursor position fraction [0.0 (left) .. 1.0 (right)]
        // This ensures the cursor visually stays where it is while the grid moves underneath.
        let fraction = if let Some(start) = self.start_time {
            let quarter_secs = 60.0 / (self.tempo_bpm as f32);
            let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
            let measure_secs = beat_secs * (self.time_sig_a as f32);
            let window_secs = measure_secs * (self.measures as f32);

            let cursor_time_secs = self.cursor_pos * measure_secs;

            if let Some(frozen_end) = self.frozen_view_end {
                let view_end_secs = frozen_end.duration_since(start).as_secs_f32();
                let view_start_secs = view_end_secs - window_secs;
                if window_secs > 0.0 {
                    (cursor_time_secs - view_start_secs) / window_secs
                } else {
                    0.5
                }
            } else {
                // Live view (recording)? Usually cursor is at right (1.0).
                // Or we haven't frozen yet. Assumes right edge for now.
                1.0
            }
        } else {
            // Static view [0 .. measures]
            if self.measures > 0 {
                self.cursor_pos / (self.measures as f32)
            } else {
                0.0
            }
        };
        self.playback_viewport_fraction = fraction.clamp(0.0, 1.0);

        // Relocate start_time so that current cursor position aligns with "now".
        // This ensures the view (which follows "now" when scrolling_active=true) shows the cursor
        // and that "elapsed" calculations are correct relative to the new start time.
        let quarter_secs = 60.0 / (self.tempo_bpm as f32);
        let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
        let measure_secs = beat_secs * (self.time_sig_a as f32);
        let cursor_offset = Duration::from_secs_f32(self.cursor_pos * measure_secs);
        let now = Instant::now();

        let new_start = now.checked_sub(cursor_offset).unwrap_or(now);

        // Adjust recording_ended_at to maintain song duration relative to new start
        if let Some(old_start) = self.start_time {
            if let Some(old_end) = self.recording_ended_at {
                // if strictly later than start
                if old_end > old_start {
                    let dur = old_end - old_start;
                    self.recording_ended_at = Some(new_start + dur);
                } else {
                    self.recording_ended_at = Some(new_start);
                }
            }
        }
        self.start_time = Some(new_start);

        self.playback_active = true;
        self.playback_start_time = Some(now);
        self.playback_start_cursor = self.cursor_pos;
        self.scrolling_active = true;
        self.frozen_view_end = None; // Unfreeze view to follow cursor
        self.view_frozen_by_user = false;
        self.status = "Playback started".to_string();

        // Update metronome anchor if active
        if self.metronome_enabled {
            if let Some(m) = &self.metronome {
                m.set_anchor(new_start);
            }
        }
    }

    fn stop_playback(&mut self) {
        if !self.playback_active {
            return;
        }
        self.playback_active = false;
        self.playback_start_time = None;
        self.scrolling_active = false;
        self.status = "Playback stopped".to_string();

        // Freeze view at current cursor, respecting the viewport fraction.
        // We want the cursor (at cursor_secs) to remain at `playback_viewport_fraction` of the window.
        // Window = [view_end - window_secs, view_end]
        // Cursor = view_start + fraction * window = view_end - window + fraction * window = view_end - (1-fraction)*window
        // => view_end = Cursor + (1 - fraction) * window
        if let Some(start) = self.start_time {
            let quarter_secs = 60.0 / (self.tempo_bpm as f32);
            let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
            let measure_secs = beat_secs * (self.time_sig_a as f32);
            let window_secs = measure_secs * (self.measures as f32);

            let cursor_secs = self.cursor_pos * measure_secs;
            let extra_secs = (1.0 - self.playback_viewport_fraction) * window_secs;

            self.frozen_view_end = Some(start + Duration::from_secs_f32(cursor_secs + extra_secs));
        }

        // Send All Notes Off (CC 123) to MIDI output to silence any hanging notes
        if let Some(conn) = &mut self.out_connection {
            // 0xB0 = Control Change on Channel 1, 123 = All Notes Off, 0 = value
            // Repeat for channels 1-16 if we want to be thorough, but Ch1 is default usually.
            // Let's just do Ch1 (0) for now.
            let _ = conn.send(&[0xB0, 123, 0]);
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
        self.chord_gen_pending = None;
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
                                self.chord_pending_scroll_index = None;

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
                        // Only record note hits when recording is enabled and recording has started (start_time set)
                        if self.recording_enabled && self.start_time.is_some() {
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
                        if self.recording_enabled && self.start_time.is_some() {
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

            // Forward raw MIDI messages to opened MIDI output, if any.
            if let Some(conn) = self.out_connection.as_mut() {
                if let Err(e) = conn.send(&bytes) {
                    self.status = format!("MIDI send failed: {}", e);
                }
            }
        }
    }

    fn ensure_chords_accumulated(&mut self, target_len: usize) {
        // `self.chords` stores predicted chords per-beat as (measure, beat_index, chord, duration).
        // Iterate beat-by-beat until we reach `target_len`.
        while self.chords.len() < target_len {
            let next_idx = self.chords.len();
            let time_sig_a = self.time_sig_a as u32;
            let next_measure = (next_idx as u32) / time_sig_a;
            let next_beat = (next_idx as u32) % time_sig_a;

            // Handle M0.B0 specifically as N.C.
            if next_idx == 0 {
                self.chords
                    .push((0, 0, PitchOrderedSet::new(), Duration::ZERO));
                if self.chords_auto_scroll {
                    self.chord_pending_scroll_index = Some(0);
                }
                continue;
            }

            // If we are in Measure update mode and this is not the first beat of the measure,
            // simply duplicate the chord we already generated at measure start and record zero
            // extra generation time for these extra beats.
            if self.chord_update_frequency == ChordUpdateFrequency::Measure && next_beat != 0 {
                if let Some((lm, _lb, prev_chord, _d)) = self.chords.last() {
                    if *lm == next_measure {
                        self.chords
                            .push((next_measure, next_beat, *prev_chord, Duration::ZERO));
                        if self.chords_auto_scroll {
                            self.chord_pending_scroll_index = Some(self.chords.len() - 1);
                        }
                        continue;
                    }
                }
            }

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
                .map(|(_, _, c, _)| *c)
                .unwrap_or(PitchOrderedSet::new());
            let start = Instant::now();

            // Notes are gathered from `src_measure`.
            // For M0, src=0. For M>0, src=M-1.
            // If updating per-beat, use the previous beat's measure as the source.
            let src_measure = if self.chord_update_frequency == ChordUpdateFrequency::Beat {
                let prev_global = if next_idx == 0 { 0 } else { next_idx - 1 };
                (prev_global as u32) / time_sig_a
            } else {
                next_measure.saturating_sub(1)
            };

            // Build absolute-measure normalized NoteData for the measure (or previous beat when in Beat mode).
            // start/end are absolute measure units (e.g., 4.25 = measure 4 + 0.25).
            let notes_for_measure: Vec<crate::algo_load::NoteData> = {
                let mut notes = Vec::new();
                // Seconds per quarter note, then convert quarter -> beat using denominator `b` (b=4 => beat=quarter)
                let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                let beat_secs = quarter_secs * (4.0_f32 / self.time_sig_b as f32);
                let beats_per_measure: usize = self.time_sig_a as usize;
                let measure_secs = beat_secs * beats_per_measure as f32;

                // Small lookahead (in measure-fraction units) if enabled
                let mut lookahead_extra = 0.0f32;
                let mut lookahead_secs = 0.0f32;
                if self.lookahead_enabled {
                    let lookahead = std::cmp::min(
                        std::time::Duration::from_millis(50),
                        std::time::Duration::from_secs_f32(beat_secs / 3.0),
                    );
                    lookahead_secs = lookahead.as_secs_f32();
                    lookahead_extra = lookahead_secs / measure_secs;
                }

                if self.chord_update_frequency == ChordUpdateFrequency::Beat {
                    // Use notes from the just-past beat only.
                    let prev_global = if next_idx == 0 { 0 } else { next_idx - 1 };
                    let prev_measure = (prev_global as u32) / self.time_sig_a as u32;
                    let prev_beat = (prev_global as u32) % self.time_sig_a as u32;

                    let src_idx0 = prev_measure as f32;
                    let measure_start = src_idx0 * measure_secs;
                    let beat_start_secs = measure_start + (prev_beat as f32) * beat_secs;
                    let beat_end_secs = beat_start_secs + beat_secs + lookahead_secs;

                    for hit in &self.note_hits {
                        let s_elapsed = hit.start * measure_secs;

                        // Exclude notes that start at/after the end of the beat window
                        if s_elapsed >= beat_end_secs {
                            continue;
                        }

                        // Exclude notes that end at/before the start of the beat window
                        if let Some(e) = hit.end {
                            let e_elapsed = e * measure_secs;
                            if e_elapsed <= beat_start_secs {
                                continue;
                            }
                        }

                        // Base measure for this beat-source
                        let base_measure = prev_measure as f32;

                        // Beat-end in measure units
                        let beat_end_measure = (beat_end_secs / measure_secs) as f32;

                        // Compute absolute start/end directly from hit.* (already in measure units), but clamp to the source measure window
                        let abs_start = hit
                            .start
                            .clamp(base_measure, base_measure + 1.0 + lookahead_extra);

                        let raw_end = hit.end.unwrap_or(beat_end_measure);
                        let raw_end = raw_end.min(beat_end_measure);
                        let abs_end =
                            raw_end.clamp(base_measure, base_measure + 1.0 + lookahead_extra);

                        let v_norm = (hit.velocity as f32) / 127.0;

                        notes.push(crate::algo_load::NoteData {
                            pitch: hit.pitch as i32,
                            start: abs_start,
                            end: abs_end,
                            velocity: v_norm,
                        });
                    }
                } else {
                    // Existing behaviour: collect notes from the whole source measure.
                    let src_idx0 = src_measure as f32;
                    let measure_start = src_idx0 * measure_secs;
                    // lookahead: measure_end may be extended
                    let mut measure_end = (src_idx0 + 1.0) * measure_secs;
                    if self.lookahead_enabled {
                        measure_end += lookahead_secs;
                    }

                    for hit in &self.note_hits {
                        let s_elapsed = hit.start * measure_secs;

                        // Exclude if note starts at/after measure end (with lookahead)
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

                        // Base measure for this source
                        let base_measure = src_measure as f32;

                        // Compute absolute start/end directly and clamp to the measure window
                        let abs_start = hit
                            .start
                            .clamp(base_measure, base_measure + 1.0 + lookahead_extra);

                        let raw_end = hit.end.unwrap_or(base_measure + 1.0 + lookahead_extra);
                        let abs_end =
                            raw_end.clamp(base_measure, base_measure + 1.0 + lookahead_extra);

                        // Normalize velocity to [0,1] (MIDI velocities are 0..127)
                        let v_norm = (hit.velocity as f32) / 127.0;

                        notes.push(crate::algo_load::NoteData {
                            pitch: hit.pitch as i32,
                            start: abs_start,
                            end: abs_end,
                            velocity: v_norm,
                        });
                    }
                }

                notes
            };
            for note in &notes_for_measure {
                println!(
                    "Note in measure {} beat {}: pitch={} start={:.3} end={:.3} vel={:.3}",
                    if next_beat == 0 {
                        next_measure - 1
                    } else {
                        next_measure
                    },
                    (next_beat + self.time_sig_a as u32 - 1) % self.time_sig_a as u32,
                    note.pitch,
                    note.start,
                    note.end,
                    note.velocity
                );
            }
            // Generate ONE chord for the current beat (or the whole measure when in Measure mode)
            let num_beats = if self.chord_update_frequency == ChordUpdateFrequency::Beat {
                1u32
            } else {
                self.time_sig_a as u32
            };
            let beat_start = next_idx as u32;
            let beat_end = beat_start + num_beats;
            let chord = generate_chord_for_measure(
                last_chord,
                sample_fn,
                &notes_for_measure,
                self.time_sig_a as u32,
                beat_start,
                beat_end,
            );
            let elapsed = start.elapsed();
            println!(
                "Generated chord for measure {} beat {} (beats {}..{}):\t{:032b} [{} ns]",
                next_measure,
                next_beat,
                beat_start,
                beat_end,
                chord.get_data(),
                elapsed.as_nanos()
            );
            self.chords.push((next_measure, next_beat, chord, elapsed));

            if self.chords_auto_scroll {
                // scroll to newly-added chord
                self.chord_pending_scroll_index = Some(self.chords.len() - 1);
            }
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
        let header = format!(
            "# sig: {}/{}\n# pos: 0-based\n",
            self.time_sig_a, self.time_sig_b
        );
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
            // measures are 0-based now
            let s_pos = (s_measure_idx as f64) + s_in_measure as f64;

            let e_elapsed = hit_end_secs;
            let e_measure_idx = (e_elapsed / measure_secs).floor() as i64;
            let e_in_measure = ((e_elapsed - (e_measure_idx as f32) * measure_secs) / measure_secs)
                .clamp(0.0_f32, 1.0_f32);
            let e_pos = (e_measure_idx as f64) + e_in_measure as f64;

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
        let mut header_pos_offset: Option<f32> = None; // Some(0.0) or Some(1.0) if header specifies
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
                if let Some(poshdr) = rest.strip_prefix("pos:") {
                    let poshdr = poshdr.trim();
                    if poshdr.starts_with('0') {
                        header_pos_offset = Some(0.0_f32);
                    } else if poshdr.starts_with('1') {
                        header_pos_offset = Some(1.0_f32);
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
        let mut parsed_rows: Vec<(u8, f64, f64, u8)> = Vec::new();

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

            parsed_rows.push((pitch, s_pos, e_pos, velocity));
        }

        // Determine whether file uses 0-based or 1-based positions. Preference: explicit header, otherwise auto-detect by min position.
        let mut min_pos: f64 = std::f64::INFINITY;
        for (_p, s_pos, e_pos, _v) in &parsed_rows {
            if *s_pos < min_pos {
                min_pos = *s_pos;
            }
            if *e_pos < min_pos {
                min_pos = *e_pos;
            }
        }
        let offset = if let Some(h) = header_pos_offset {
            h
        } else if min_pos >= 1.0 {
            1.0_f32
        } else {
            0.0_f32
        };

        let mut hits: Vec<crate::midi::NoteHit> = Vec::new();
        let mut max_end_secs: f32 = 0.0;

        for (pitch, s_pos, e_pos, velocity) in parsed_rows {
            // convert measure-based positions to seconds relative to start_anchor
            let e_sec = ((e_pos as f32 - offset) as f32) * measure_secs;
            if e_sec > max_end_secs {
                max_end_secs = e_sec;
            }

            // store normalized measure positions (1 measure == 1.0) using 0-based positions
            let start_meas = s_pos as f32 - offset;
            let end_meas = e_pos as f32 - offset;

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
        self.save_path = Some(path.to_path_buf());
        self.chord_gen_pending = None;
        self.cursor_pos = 0.0;

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
        self.save_path = None;
        self.chord_gen_pending = None;
        self.cursor_pos = 0.0;
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
            chords_scroll_direction: self.chords_scroll_direction,
            log_width_frac: self.log_width_frac,
            measures: self.measures,
            time_sig_a: self.time_sig_a,
            time_sig_b: self.time_sig_b,
            metronome_enabled: self.metronome_enabled,
            power_save_mode: self.power_save_mode,
            lookahead_enabled: self.lookahead_enabled,
            selected_algo_idx: self.selected_algo_idx,
            show_initial_chord: self.show_initial_chord,
            chord_update_frequency: self.chord_update_frequency,
            midi_in_name: self.selected.and_then(|i| self.ports.get(i).cloned()),
            midi_out_name: self
                .out_selected
                .and_then(|i| self.out_ports.get(i).cloned()),
        };
        eframe::set_value(storage, eframe::APP_KEY, &state);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Playback logic
        if self.playback_active {
            if let Some(start) = self.playback_start_time {
                let now = Instant::now();
                let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
                let measure_secs = beat_secs * (self.time_sig_a as f32);

                let elapsed_measures = now.duration_since(start).as_secs_f32() / measure_secs;
                let new_cursor = self.playback_start_cursor + elapsed_measures;

                // Play MIDI notes between self.cursor_pos and new_cursor
                if let Some(conn) = &mut self.out_connection {
                    for hit in &self.note_hits {
                        // Note On
                        if hit.start > self.cursor_pos && hit.start <= new_cursor {
                            let _ = conn.send(&[0x90, hit.pitch, hit.velocity]);
                        }
                        // Note Off
                        if let Some(end) = hit.end {
                            if end > self.cursor_pos && end <= new_cursor {
                                let _ = conn.send(&[0x80, hit.pitch, 0]);
                            }
                        }
                    }
                }

                self.cursor_pos = new_cursor;

                // Auto-stop if we passed the end
                // We used to wait for max_end + 1.0 but that causes the "short distance" overrun issue.
                // We should stop strictly at the max_end (or recording_ended_at) to avoid overshooting.

                let note_max_end = self
                    .note_hits
                    .iter()
                    .map(|h| h.end.unwrap_or(h.start))
                    .fold(0.0, f32::max);

                let mut overall_max_meas = note_max_end;

                if let (Some(s), Some(e)) = (self.start_time, self.recording_ended_at) {
                    if e > s {
                        let dur_secs = e.duration_since(s).as_secs_f32();
                        let dur_meas = dur_secs / measure_secs;
                        if dur_meas > overall_max_meas {
                            overall_max_meas = dur_meas;
                        }
                    }
                }

                if self.cursor_pos >= overall_max_meas {
                    self.cursor_pos = overall_max_meas; // Clamp
                    self.stop_playback();
                }
                ctx.request_repaint();
            }
        }

        // Update recording cursor visual
        if self.recording_enabled {
            if let Some(start) = self.start_time {
                let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                let beat_secs = quarter_secs * (4.0 / self.time_sig_b as f32);
                let measure_secs = beat_secs * (self.time_sig_a as f32);

                let elapsed = Instant::now().duration_since(start).as_secs_f32();
                let current_measures = elapsed / measure_secs;
                self.cursor_pos = current_measures.floor();
                ctx.request_repaint();
            }
        }

        self.drain_messages();

        // Update window title to include opened file name (if any).
        // Keep base title in sync with initial run_native title in main.rs.
        let base_title = "VeriChord";
        let title = if let Some(p) = &self.save_path {
            format!(
                "{} - {}",
                base_title,
                if let Some(name) = p.file_name().and_then(|s| s.to_str()) {
                    name
                } else {
                    p.to_str().unwrap()
                }
            )
        } else {
            base_title.to_string()
        };

        // Only send the viewport title command if it changed since last frame.
        if self.last_window_title.as_deref() != Some(title.as_str()) {
            ctx.send_viewport_cmd(egui::ViewportCommand::Title(title.clone()));
            self.last_window_title = Some(title);
        }

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

        // Keyboard shortcuts

        // 1. Shift+Space for Recording (priority)
        let record_shortcut_pressed =
            ctx.input_mut(|i| i.consume_key(egui::Modifiers::SHIFT, egui::Key::Space));

        // 2. Space for Playback (if not consumed by recording)
        let play_shortcut_pressed = !record_shortcut_pressed
            && ctx.input_mut(|i| i.consume_key(egui::Modifiers::NONE, egui::Key::Space));

        // Keyboard shortcuts (use `command` so Ctrl on Win/Linux and Cmd on macOS both work).
        // Use `consume_key` so handled shortcuts don't leak to other UI elements.
        let new_pressed = ctx.input_mut(|i| i.consume_key(egui::Modifiers::COMMAND, egui::Key::N));
        let open_pressed = ctx.input_mut(|i| i.consume_key(egui::Modifiers::COMMAND, egui::Key::O));
        // Save / Save as: prefer Save as when Shift is held
        let save_as_pressed = ctx.input_mut(|i| {
            let mut mods = egui::Modifiers::default();
            mods.command = true;
            mods.shift = true;
            i.consume_key(mods, egui::Key::S)
        });
        let save_pressed = !save_as_pressed
            && ctx.input_mut(|i| i.consume_key(egui::Modifiers::COMMAND, egui::Key::S));

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

        if record_shortcut_pressed && self.connection.is_some() && !self.playback_active {
            let is_recording = self.start_time.is_some() && self.scrolling_active;
            if is_recording {
                self.stop_recording();
            } else {
                self.start_recording(Instant::now());
            }
            ctx.request_repaint();
        }

        if play_shortcut_pressed && !self.recording_enabled {
            if self.playback_active {
                self.stop_playback();
            } else {
                self.start_playback();
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

                    // Use the Ui extension method to draw menu items with right-aligned shortcuts.
                    if ui.button_with_shortcut("New", new_shortcut).clicked() {
                        self.handle_new();
                        ui.close();
                    }
                    if ui.button_with_shortcut("Open", open_shortcut).clicked() {
                        self.handle_open_file();
                        ui.close();
                    }
                    ui.separator();
                    if ui.button_with_shortcut("Save", save_shortcut).clicked() {
                        self.handle_save();
                        ui.close();
                    }
                    if ui.button_with_shortcut("Save as", save_as_shortcut).clicked() {
                        self.handle_save_as();
                        ui.close();
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
                    ui.label("Chord update frequency:");
                    ui.radio_value(&mut self.chord_update_frequency, ChordUpdateFrequency::Beat, "Beat");
                    ui.radio_value(&mut self.chord_update_frequency, ChordUpdateFrequency::Measure, "Measure");
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
                    ui.checkbox(&mut self.lookahead_enabled, "Lookahead")
                        .on_hover_text("When enabled, delay automatic chord prediction until a short lookahead time after the next measure starts (default: on). t = min(50ms, 1/3 beat).");
                    ui.checkbox(&mut self.show_initial_chord, "Show initial chord")
                        .on_hover_text("When checked, show an initial N.C. card at measure 0 before any generated chords.");

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
                ComboBox::from_id_salt("midi_input")
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

            // MIDI Output selector (similar controls to MIDI Input)
            ui.horizontal(|ui| {
                ui.label("MIDI Output:");
                ComboBox::from_id_salt("midi_output")
                    .selected_text(
                        self.out_selected
                            .and_then(|i| self.out_ports.get(i))
                            .cloned()
                            .unwrap_or("(none)".to_owned()),
                    )
                    .show_ui(ui, |ui| {
                        for (i, name) in self.out_ports.iter().enumerate() {
                            ui.selectable_value(&mut self.out_selected, Some(i), name);
                        }
                    });
                if ui.button("Refresh").clicked() {
                    self.refresh_output_ports();
                }

                if self.out_connection.is_some() {
                    if ui.button("Close").clicked() {
                        self.close_output_connection();
                    }
                } else {
                    if ui.button("Open").clicked() {
                        self.open_selected_output();
                    }
                }
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
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
                            let sz: f32 = 26.0;
                            let right_margin: f32 = 6.0;

                            // Right-align the auto-scroll control to keep layout consistent with other panes.
                            ui.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    // Keep a small margin from the right edge
                                    ui.add_space(right_margin);

                                    let (rect, resp) = ui.allocate_exact_size(
                                        egui::vec2(sz, sz),
                                        egui::Sense::click(),
                                    );
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
                                },
                            );
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
                            let right_margin: f32 = 12.0;

                            // Right-align the record button so it is consistently flush from the right edge.
                            ui_h.with_layout(
                                egui::Layout::right_to_left(egui::Align::Center),
                                |ui| {
                                    // Keep a margin from the right edge
                                    ui.add_space(right_margin);

                                    // Record Button
                                    let (rect, resp) = ui.allocate_exact_size(
                                        egui::vec2(sz, sz),
                                        egui::Sense::click(),
                                    );
                                    let resp = resp.on_hover_text("Record (Shift+Space)");

                                    // determine state and colors
                                    let is_open = self.connection.is_some();
                                    let is_recording =
                                        self.start_time.is_some() && self.scrolling_active;

                                    // Disable recording if playback is active
                                    let recording_disabled = self.playback_active;

                                    let painter = ui.painter_at(rect);
                                    let center = rect.center();
                                    let radius = sz * 0.5;

                                    if !is_open || recording_disabled {
                                        // disabled gray circle
                                        painter.circle_filled(
                                            center,
                                            radius,
                                            egui::Color32::from_gray(70),
                                        );
                                        painter.circle_filled(
                                            center,
                                            radius * 0.4,
                                            egui::Color32::from_gray(140),
                                        );
                                    } else if is_recording {
                                        // recording visuals: red center/outer
                                        painter.circle_filled(
                                            center,
                                            radius,
                                            egui::Color32::from_rgb(120, 40, 40),
                                        );
                                        let square_size = radius * 0.8;
                                        let square_top_left = center
                                            - egui::vec2(square_size / 2.0, square_size / 2.0);
                                        let square_rect = egui::Rect::from_min_size(
                                            square_top_left,
                                            egui::vec2(square_size, square_size),
                                        );
                                        painter.rect_filled(
                                            square_rect,
                                            0.0,
                                            egui::Color32::from_rgb(200, 80, 80),
                                        );
                                    } else {
                                        // ready-to-record visuals: green background with circular white center
                                        painter.circle_filled(
                                            center,
                                            radius,
                                            egui::Color32::from_rgb(60, 145, 60),
                                        );
                                        painter.circle_filled(
                                            center,
                                            radius * 0.4,
                                            egui::Color32::WHITE,
                                        );
                                    }

                                    if resp.clicked() && !recording_disabled {
                                        if is_open {
                                            if is_recording {
                                                // stop recording
                                                self.stop_recording();
                                                ctx.request_repaint();
                                            } else {
                                                // start recording
                                                self.start_recording(Instant::now());
                                                ctx.request_repaint();
                                            }
                                        } else {
                                            self.open_selected(ctx);
                                        }
                                    }

                                    ui.add_space(8.0); // gap between buttons

                                    // Play Button
                                    let (rect, resp) = ui.allocate_exact_size(
                                        egui::vec2(sz, sz),
                                        egui::Sense::click(),
                                    );
                                    let resp = resp.on_hover_text("Play / Stop (Space)");
                                    let painter = ui.painter_at(rect);
                                    let center = rect.center();
                                    let radius = sz * 0.5;

                                    // Disable playback if recording is active
                                    let playback_disabled = self.recording_enabled;

                                    let bg = if playback_disabled {
                                        egui::Color32::from_gray(70)
                                    } else if self.playback_active {
                                        egui::Color32::from_rgb(180, 160, 60) // Yellowish for pause/stop
                                    } else {
                                        egui::Color32::from_rgb(60, 160, 60) // Green for play
                                    };
                                    painter.circle_filled(center, radius, bg);

                                    if self.playback_active {
                                        // Square (Stop)
                                        let s = radius * 0.7;
                                        painter.rect_filled(
                                            egui::Rect::from_center_size(center, egui::vec2(s, s)),
                                            1.0,
                                            egui::Color32::WHITE,
                                        );
                                        if resp.clicked() {
                                            self.stop_playback();
                                            ctx.request_repaint();
                                        }
                                    } else {
                                        // Triangle (Play)
                                        let s = radius * 0.5;
                                        // Points: (center-s, center-s), (center-s, center+s), (center+s, center)
                                        // Rotate 90 deg -> pointing right
                                        let p1 = center + egui::vec2(-s * 0.5, -s * 0.8);
                                        let p2 = center + egui::vec2(-s * 0.5, s * 0.8);
                                        let p3 = center + egui::vec2(s * 0.9, 0.0);

                                        let triangle_color = if playback_disabled {
                                            egui::Color32::from_gray(140)
                                        } else {
                                            egui::Color32::WHITE
                                        };

                                        painter.add(egui::Shape::convex_polygon(
                                            vec![p1, p2, p3],
                                            triangle_color,
                                            egui::Stroke::NONE,
                                        ));

                                        if resp.clicked() && !playback_disabled {
                                            self.start_playback();
                                            ctx.request_repaint();
                                        }
                                    }
                                },
                            );
                        });

                        ui_right.separator();

                        let (r, resp) = ui_right.allocate_exact_size(
                            egui::vec2(right_w, height),
                            egui::Sense::click_and_drag(),
                        );
                        // Hint for users: indicate that the piano roll can be panned by dragging or using the mouse wheel
                        let hint = if self.recording_enabled {
                            "Panning disabled while recording"
                        } else {
                            "Pan timeline (drag or mouse wheel)"
                        };
                        let resp = resp.on_hover_text(hint);
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

                        if resp.dragged() && !self.recording_enabled {
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
                                // Mark view as user-frozen (keeps metronome & generation running)
                                self.view_frozen_by_user = true;
                            }
                        }

                        // wheel scrolling handler moved to after view_end

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
                        // `view_end` is mutable because mouse-wheel horizontal scrolling can switch the view into
                        // a frozen/manual mode by setting `frozen_view_end` and disabling `scrolling_active`.
                        let mut view_end = if self.scrolling_active && !self.view_frozen_by_user {
                            if self.playback_active {
                                // If playing back, enforce lookahead so cursor stays at the same relative screen position
                                let offset = (1.0 - self.playback_viewport_fraction) * window_secs;
                                now + Duration::from_secs_f32(offset)
                            } else {
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
                            }
                        } else {
                            self.frozen_view_end.unwrap_or(now)
                        };

                        // Handle mouse wheel horizontal scrolling while hovering the piano roll.
                        // Use `smooth_scroll_delta` (smoothed over frames) for a nicer, less jumpy experience.
                        // Interpret horizontal scroll (preferred) or vertical wheel as a time pan.
                        // Disabled while recording to avoid interrupting the live recording view.
                        if resp.hovered() && !self.recording_enabled {
                            let scroll = ui_right.ctx().input(|i| i.smooth_scroll_delta);

                            // Prefer horizontal scroll; otherwise use the vertical scroll value.
                            let scroll_px = if scroll.x.abs() > scroll.y.abs() {
                                scroll.x
                            } else {
                                scroll.y
                            };
                            if scroll_px.abs() > 0.0 {
                                // Convert pixels to seconds using the same px_per_sec metric as dragging.
                                let px_per_sec = right_w / window_secs;
                                let dt = scroll_px / px_per_sec;

                                // Base the adjustment on the current frozen view end (if any),
                                // otherwise start from the current computed view_end.
                                let base_fve = if let Some(fve) = self.frozen_view_end {
                                    fve
                                } else {
                                    view_end
                                };

                                let mut new_fve = if dt >= 0.0 {
                                    base_fve
                                        .checked_sub(std::time::Duration::from_secs_f32(dt))
                                        .unwrap_or(base_fve)
                                } else {
                                    base_fve + std::time::Duration::from_secs_f32(-dt)
                                };

                                // Constrain panning: don't show area before start_time (at left edge)
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
                                // Mark view as user-frozen (keep metronome running)
                                self.view_frozen_by_user = true; // switch to manual/frozen mode
                                // Recompute view_end to reflect the new frozen value
                                view_end = self.frozen_view_end.unwrap_or(now);
                                ui_right.ctx().request_repaint();
                            }
                        }

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

                                            // measure number relative to start_time (0-indexed)
                                            let measure_num =
                                                beat_idx as isize / beats_per_measure as isize;
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

                                            let measure_num = b / beats_per_measure; // 0-indexed
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

                                            let measure_num =
                                                beat_idx as isize / beats_per_measure as isize; // 0-indexed
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

                                            let measure_num = b / beats_per_measure; // 0-indexed
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

                        // chord generation per beat
                        if let Some(start) = self.start_time.filter(|_| self.scrolling_active) {
                            // Don't generate chords during playback
                            if !self.playback_active {
                                let elapsed = now.duration_since(start).as_secs_f32();
                                // Generate chords up to including the current beat
                                let quarter_secs = 60.0 / (self.tempo_bpm as f32);
                                let beat_secs = quarter_secs * (4.0_f32 / self.time_sig_b as f32);
                                let target_len = (elapsed / beat_secs).floor() as usize + 1;
                                self.ensure_chords_accumulated(target_len);
                            }
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

                        // Draw Playback Cursor "I" and handle clicks
                        // Do this last so it overlays the notes and grid
                        {
                            let cursor_color = egui::Color32::from_rgb(255, 200, 0); // Gold/Orange
                            let mut cursor_x_opt = None;

                            // Map cursor_pos (measures) to screen X
                            if let Some(start) = self.start_time {
                                // Dynamic window logic: [view_end - window_secs, view_end]
                                let elapsed = view_end.duration_since(start).as_secs_f32();
                                let view_start_secs = elapsed - window_secs;
                                let cursor_secs = self.cursor_pos * measure_secs;

                                if cursor_secs >= view_start_secs && cursor_secs <= elapsed {
                                    let fraction = (cursor_secs - view_start_secs) / window_secs;
                                    cursor_x_opt =
                                        Some(grid_left + fraction * (grid_right - grid_left));
                                }
                            } else {
                                // Static view logic: [0, measures]
                                if self.cursor_pos >= 0.0 && self.cursor_pos <= self.measures as f32
                                {
                                    let fraction = self.cursor_pos / (self.measures as f32);
                                    cursor_x_opt =
                                        Some(grid_left + fraction * (grid_right - grid_left));
                                }
                            }

                            if let Some(cx) = cursor_x_opt {
                                painter.line_segment(
                                    [egui::pos2(cx, grid_top), egui::pos2(cx, grid_bottom)],
                                    egui::Stroke::new(2.0, cursor_color),
                                );
                                // I-beam caps
                                painter.line_segment(
                                    [
                                        egui::pos2(cx - 3.0, grid_top),
                                        egui::pos2(cx + 3.0, grid_top),
                                    ],
                                    egui::Stroke::new(2.0, cursor_color),
                                );
                                painter.line_segment(
                                    [
                                        egui::pos2(cx - 3.0, grid_bottom),
                                        egui::pos2(cx + 3.0, grid_bottom),
                                    ],
                                    egui::Stroke::new(2.0, cursor_color),
                                );
                            }

                            // Handle Clicks to set cursor
                            if resp.clicked_by(egui::PointerButton::Primary)
                                && !resp.dragged()
                                && !self.recording_enabled
                                && !self.playback_active
                            {
                                if let Some(pos) = resp.interact_pointer_pos() {
                                    let x = pos.x;
                                    let fraction = (x - grid_left) / (grid_right - grid_left);
                                    let clamped_frac = fraction.clamp(0.0, 1.0);

                                    let new_cursor_measures = if let Some(start) = self.start_time {
                                        let elapsed = view_end.duration_since(start).as_secs_f32();
                                        let view_start_secs = elapsed - window_secs;
                                        let time_secs =
                                            view_start_secs + clamped_frac * window_secs;
                                        time_secs / measure_secs
                                    } else {
                                        clamped_frac * (self.measures as f32)
                                    };

                                    self.cursor_pos = new_cursor_measures.max(0.0);
                                    ctx.request_repaint();
                                }
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

                        // Reset chords to initial state (no predicted chords until measures pass)
                        self.chords.clear();

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
                        // To produce chords corresponding to notes in measures 0..(measures_present-1), call up_to = measures_present
                        let target = measures_present;

                        // Generate
                        let total_beats = (target as usize) * (self.time_sig_a as usize);
                        self.ensure_chords_accumulated(total_beats);
                        ui.ctx().request_repaint();

                        self.status = format!("Regenerated chords up to measure {}", target);
                    }
                }

                // push the controls (direction + auto-scroll) to the far right of the header, leaving a small right margin
                let sz: f32 = 26.0;
                let gap: f32 = 6.0;
                let right_margin: f32 = 12.0; // px of spacing to keep from the right edge
                // Place controls using a right-to-left layout so the rightmost control sits comfortably away from the right edge
                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    // Keep a margin from the right edge
                    ui.add_space(right_margin);

                    // Auto-scroll (rightmost)
                    let (rect, resp) =
                        ui.allocate_exact_size(egui::vec2(sz, sz), egui::Sense::click());
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
                        if self.chords_auto_scroll {
                            if !self.chords.is_empty() {
                                // latest chord index in display (account for optional initial card)
                                self.chord_pending_scroll_index = Some(
                                    self.chords.len() - 1
                                        + if self.show_initial_chord {
                                            self.time_sig_a as usize
                                        } else {
                                            0
                                        },
                                );
                                ui.ctx().request_repaint();
                            } else if self.show_initial_chord {
                                // No generated chords yet: scroll to the initial N.C. card
                                self.chord_pending_scroll_index = Some(0);
                                ui.ctx().request_repaint();
                            }
                        }
                    }
                    ui.add_space(gap);

                    // Scroll direction toggle (to the left of auto-scroll)
                    let (dir_rect, dir_resp) =
                        ui.allocate_exact_size(egui::vec2(sz, sz), egui::Sense::click());
                    let dir_resp = dir_resp.on_hover_text(match self.chords_scroll_direction {
                        ChordScrollDirection::Horizontal => "Scroll: Left/Right",
                        ChordScrollDirection::Vertical => "Scroll: Up/Down",
                    });
                    let painter = ui.painter_at(dir_rect);
                    let bg = egui::Color32::from_rgb(70, 70, 70);
                    painter.rect_filled(dir_rect, 4.0, bg);
                    let dir_icon = match self.chords_scroll_direction {
                        ChordScrollDirection::Horizontal => "↔",
                        ChordScrollDirection::Vertical => "↕",
                    };
                    painter.text(
                        dir_rect.center(),
                        egui::Align2::CENTER_CENTER,
                        dir_icon,
                        egui::FontId::proportional(14.0),
                        egui::Color32::WHITE,
                    );
                    if dir_resp.clicked() {
                        self.chords_scroll_direction = match self.chords_scroll_direction {
                            ChordScrollDirection::Horizontal => ChordScrollDirection::Vertical,
                            ChordScrollDirection::Vertical => ChordScrollDirection::Horizontal,
                        };
                        // if auto-scroll is on, ensure we move to the latest chord
                        if self.chords_auto_scroll {
                            if !self.chords.is_empty() {
                                let target_idx = if self.show_initial_chord {
                                    self.chords.len() - 1
                                } else {
                                    // if hiding initial, index is offset by how many items we hid?
                                    // Actually, if we are scrolling to the last rect, we just need the index in display_chords.
                                    // But chord_pending_scroll_index is used to scroll to rect at index.
                                    // The widgets render display_chords.
                                    // If show_initial=false, we hide M0 chords.
                                    // self.chords contains all.
                                    // We need to count how many are valid.
                                    self.chords
                                        .iter()
                                        .filter(|c| c.0 > 0)
                                        .count()
                                        .saturating_sub(1)
                                };
                                self.chord_pending_scroll_index = Some(target_idx);
                            } else {
                                self.chord_pending_scroll_index = None;
                            }
                        }
                        ui.ctx().request_repaint();
                    }
                });
            });
            let card_h: f32 = 72.0;
            let card_w: f32 = 140.0;
            let current_offset = if self.chords_scroll_direction == ChordScrollDirection::Horizontal
            {
                crate::ui::widgets::chord_cards_horizontal(
                    ui,
                    &self.chords,
                    self.time_sig_a as u32,
                    false, // show_initial
                    card_w,
                    card_h,
                    self.chords_auto_scroll,
                    &mut self.chord_pending_scroll_index,
                )
            } else {
                crate::ui::widgets::chord_cards_vertical(
                    ui,
                    &self.chords,
                    self.time_sig_a as u32,
                    self.show_initial_chord,
                    card_w,
                    card_h,
                    self.chords_auto_scroll,
                    &mut self.chord_pending_scroll_index,
                )
            };

            // Detect user manual scrolling and disable auto-scroll
            if self.chords_auto_scroll {
                // If chords are empty (e.g. just cleared via Ctrl+N or start recording),
                // the offset naturally drops to near-zero. We ignore this drop to keep scroll lock.
                if !self.chords.is_empty() {
                    if let Some(last_offset) = self.last_chord_scroll_offset {
                        // If user scrolled away from the end (offset decreased), disable auto-scroll
                        if current_offset < last_offset - 1.0 {
                            self.chords_auto_scroll = false;
                        }
                    }
                }
                self.last_chord_scroll_offset = Some(current_offset);
            } else {
                // When auto-scroll is off, still track offset for when it's re-enabled
                self.last_chord_scroll_offset = Some(current_offset);
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
