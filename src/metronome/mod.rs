use rodio::{OutputStream, Sink, buffer::SamplesBuffer};
use spin_sleep::SpinSleeper;
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread;
use std::time::{Duration, Instant};

/// Minimal metronome that schedules clicks on a dedicated high-precision thread.
///
/// - Uses `spin_sleep` to reduce scheduling jitter.
/// - Uses `rodio` to play short generated click waveforms.
///
/// The metronome runs until dropped. Use `set_params` to update tempo/time-signature
/// while running.
pub struct Metronome {
    cmd_tx: Sender<Command>,
}

enum Command {
    Update {
        bpm: u32,
        beats_per_measure: usize,
        denom: u32,
        enabled: bool,
    },
    SetAnchor(Instant),
    Stop,
}

impl Metronome {
    /// Start the metronome thread.
    ///
    /// `bpm` - beats per minute (quarter notes)
    /// `beats_per_measure` - number of beats per measure (e.g. 4 for 4/4)
    pub fn start(bpm: u32, beats_per_measure: usize, denom: u32, enabled: bool) -> Self {
        let (tx, rx): (Sender<Command>, Receiver<Command>) = mpsc::channel();

        // Spawn thread
        thread::spawn(move || {
            // Try to open default audio output once for the thread lifetime
            let (_stream, stream_handle) = match OutputStream::try_default() {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("Metronome: failed to open audio output: {}", e);
                    return;
                }
            };

            // Configure sleeper: try to spin for small windows to reduce jitter
            let sleeper = SpinSleeper::new(1_000);

            let mut cur_bpm = bpm;
            let mut cur_beats = beats_per_measure;
            let mut cur_denom = 4_u32; // default assume quarter beats unless updated
            // If caller supplied denom via initial Update this will be overwritten when processed
            let mut enabled = enabled;

            let mut anchor: Option<Instant> = None;
            let mut next_instant = Instant::now();
            let mut beat_idx: usize = 0;

            'outer: loop {
                // Non-blocking command poll (drain all)
                while let Ok(cmd) = rx.try_recv() {
                    match cmd {
                        Command::Update {
                            bpm,
                            beats_per_measure,
                            denom,
                            enabled: en,
                        } => {
                            // Adopt new values
                            cur_bpm = bpm;
                            cur_beats = beats_per_measure;
                            cur_denom = denom;
                            enabled = en;

                            // If we have an anchor and are (now) enabled, recompute next_instant aligned to anchor
                            if enabled {
                                if let Some(a) = anchor {
                                    let now = Instant::now();
                                    let beat_secs =
                                        60.0f64 / (cur_bpm as f64) * (4.0_f64 / cur_denom as f64);
                                    if a > now {
                                        // If anchor is in the future, schedule the first beat at the anchor
                                        next_instant = a;
                                        beat_idx = 0;
                                    } else {
                                        // Compute number of beats elapsed since anchor and schedule next beat aligned to grid
                                        let elapsed = now.duration_since(a).as_secs_f64();
                                        let beats_elapsed = (elapsed / beat_secs).floor();
                                        beat_idx = (beats_elapsed as usize) + 1;
                                        next_instant = a + Duration::from_secs_f64(
                                            (beats_elapsed + 1.0) * beat_secs,
                                        );
                                    }
                                } else {
                                    // no anchor -> schedule relative to now
                                    next_instant = Instant::now();
                                }
                            }
                        }
                        Command::SetAnchor(a) => {
                            anchor = Some(a);
                            // compute next_instant aligned to anchor based on current BPM
                            let now = Instant::now();
                            let beat_secs = 60.0f64 / (cur_bpm as f64);
                            if a > now {
                                next_instant = a;
                                beat_idx = 0;
                            } else {
                                let elapsed = now.duration_since(a).as_secs_f64();
                                let beats_elapsed = (elapsed / beat_secs).floor();
                                beat_idx = (beats_elapsed as usize) + 1;
                                next_instant =
                                    a + Duration::from_secs_f64((beats_elapsed + 1.0) * beat_secs);
                            }
                        }
                        Command::Stop => break 'outer,
                    }
                }

                // compute current beat duration
                // compute current beat duration using denominator (e.g. 8 -> eighth-note beats)
                let beat_secs = 60.0f64 / (cur_bpm as f64) * (4.0_f64 / cur_denom as f64);
                let beat_dur = Duration::from_secs_f64(beat_secs);

                // Sleep until the scheduled instant (spin-sleep for accuracy)
                sleeper.sleep_until(next_instant);

                if enabled {
                    let is_downbeat = if cur_beats == 0 {
                        true
                    } else {
                        beat_idx % cur_beats == 0
                    };
                    // generate a short click waveform
                    let samples = generate_click_waveform(is_downbeat, 44_100);
                    let source = SamplesBuffer::new(1, 44_100, samples);

                    // create a sink and play the sample. `detach` allows it to keep playing without
                    // holding the sink handle in this thread.
                    if let Ok(sink) = Sink::try_new(&stream_handle) {
                        sink.append(source);
                        sink.detach();
                    }
                }

                // advance by one beat
                next_instant += beat_dur;
                beat_idx = beat_idx.wrapping_add(1);
            }
        });

        // seed initial params so thread can pick them up immediately
        let _ = tx.send(Command::Update {
            bpm,
            beats_per_measure,
            denom,
            enabled,
        });

        Metronome { cmd_tx: tx }
    }

    /// Update running parameters.
    pub fn set_params(&self, bpm: u32, beats_per_measure: usize, denom: u32, enabled: bool) {
        // best-effort, ignore send errors (thread may have exited)
        let _ = self.cmd_tx.send(Command::Update {
            bpm,
            beats_per_measure,
            denom,
            enabled,
        });
    }

    /// Stop the metronome thread (and drop audio resources).
    #[allow(dead_code)]
    pub fn stop(&self) {
        let _ = self.cmd_tx.send(Command::Stop);
    }

    /// Set the global anchor (start_time) used to align the beat grid.
    pub fn set_anchor(&self, anchor: Instant) {
        let _ = self.cmd_tx.send(Command::SetAnchor(anchor));
    }
}

impl Drop for Metronome {
    fn drop(&mut self) {
        let _ = self.cmd_tx.send(Command::Stop);
    }
}

/// Generate a short decaying square-wave "click".
///
/// - `downbeat`: use a higher pitch for the downbeat.
/// - `sample_rate`: sample rate in Hz.
///
/// Returns mono samples in [-1.0, 1.0].
pub fn generate_click_waveform(downbeat: bool, sample_rate: u32) -> Vec<f32> {
    let dur_s = 0.06_f32; // 60ms click
    let n_samples = (dur_s * sample_rate as f32) as usize;
    let freq = if downbeat { 1000.0 } else { 700.0 };
    let tau = 0.010; // decay time constant (seconds)
    let mut out = Vec::with_capacity(n_samples);

    for n in 0..n_samples {
        let t = n as f32 / sample_rate as f32;
        // square wave via sign(sin())
        let s = ((2.0 * std::f32::consts::PI * freq * t).sin()).signum();
        // exponential decay envelope
        let env = (-(t as f64) / tau).exp() as f32;
        // small initial ramp to avoid clicks at 0
        let ramp = (t / 0.002).clamp(0.0, 1.0);
        let amplitude = 0.8 * env * ramp;
        out.push(s * amplitude as f32);
    }

    out
}
