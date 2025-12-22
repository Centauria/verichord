use std::time::Instant;

/// Pitch-class note representation and sustain tracking
#[derive(Clone, Debug)]
pub struct NoteHit {
    pub pitch_class: u8, // 0=C ... 11=B
    pub start: Instant,
    pub end: Option<Instant>,
    pub velocity: u8,
}

/// Discrete actions parsed from raw MIDI bytes relevant to the piano roll
#[derive(Clone, Debug)]
pub enum NoteAction {
    On { pc: u8, vel: u8, time: Instant },
    Off { pc: u8, time: Instant },
}

/// Parse raw MIDI bytes into a NoteAction if it's a Note On/Off message.
/// - Note On with velocity==0 is treated as Note Off per the MIDI spec.
pub fn parse_note_action(bytes: &[u8], ts: Instant) -> Option<NoteAction> {
    if bytes.len() >= 3 {
        let status = bytes[0];
        let data1 = bytes[1];
        let data2 = bytes[2];
        let kind = status & 0xF0;
        match kind {
            0x90 => {
                let pc = data1 % 12;
                if data2 != 0 {
                    Some(NoteAction::On { pc, vel: data2, time: ts })
                } else {
                    Some(NoteAction::Off { pc, time: ts })
                }
            }
            0x80 => Some(NoteAction::Off { pc: data1 % 12, time: ts }),
            _ => None,
        }
    } else {
        None
    }
}

pub fn generate_chord_for_measure(_measure_idx: u32) -> String {
    // Placeholder: always return No Chord
    "N.C.".to_string()
}

