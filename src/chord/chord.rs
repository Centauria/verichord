use std::fmt;

pub const DIV: usize = 12;

/// C++ `code` function counterpart
pub fn code(data: u32) -> u32 {
    data & 0xFFFFFF
}

/// C++ `shift` function counterpart
pub fn shift(data: u32) -> u32 {
    (data >> (2 * DIV)) & 0xF
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum Pitch {
    C,
    Cs,
    Db,
    D,
    Ds,
    Eb,
    E,
    F,
    Fs,
    Gb,
    G,
    Gs,
    Ab,
    A,
    As,
    Bb,
    B,
}

impl PartialEq for Pitch {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl Eq for Pitch {}

impl Pitch {
    pub fn value(&self) -> u8 {
        match self {
            Pitch::C => 0,
            Pitch::Cs | Pitch::Db => 1,
            Pitch::D => 2,
            Pitch::Ds | Pitch::Eb => 3,
            Pitch::E => 4,
            Pitch::F => 5,
            Pitch::Fs | Pitch::Gb => 6,
            Pitch::G => 7,
            Pitch::Gs | Pitch::Ab => 8,
            Pitch::A => 9,
            Pitch::As | Pitch::Bb => 10,
            Pitch::B => 11,
        }
    }
}

pub const PITCH_NAMES: [&str; 12] = [
    "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B",
];

// suppress unused warnings for chord definitions
#[allow(dead_code)]
pub mod chord {
    // major
    pub const MAJ: &[i32] = &[4, 3];
    pub const MAJ7: &[i32] = &[4, 3, 4];
    pub const MAJ9: &[i32] = &[4, 3, 4, 3];
    pub const MAJ911S: &[i32] = &[4, 3, 4, 3, 4];
    pub const MAJS9: &[i32] = &[4, 3, 4, 4];
    pub const MAJS911S: &[i32] = &[4, 3, 4, 4, 3];
    pub const MAJ11: &[i32] = &[4, 3, 4, 3, 3];
    pub const MAJ13: &[i32] = &[4, 3, 4, 3, 3, 4];
    pub const ADD9: &[i32] = &[4, 3, 7];
    pub const ADD11: &[i32] = &[4, 3, 10];
    pub const ADD13: &[i32] = &[4, 3, 14];
    pub const MAJ1311S: &[i32] = &[4, 3, 4, 3, 4, 3];

    // minor
    pub const MIN: &[i32] = &[3, 4];
    pub const MIN7: &[i32] = &[3, 4, 3];
    pub const MIN9: &[i32] = &[3, 4, 3, 4];
    pub const MIN11: &[i32] = &[3, 4, 3, 4, 3];
    pub const MIN1113B: &[i32] = &[3, 4, 3, 4, 3, 3];
    pub const MIN13: &[i32] = &[3, 4, 3, 4, 3, 4];
    pub const MINADD9: &[i32] = &[3, 4, 7];
    pub const MINADD11: &[i32] = &[3, 4, 10];
    pub const MINADD13: &[i32] = &[3, 4, 14];
    pub const MINB9: &[i32] = &[3, 4, 3, 3];
    pub const MINMAJ11: &[i32] = &[3, 4, 4, 3, 3];
    pub const MINMAJ13: &[i32] = &[3, 4, 4, 3, 3, 4];
    pub const MINMAJ7: &[i32] = &[3, 4, 4];
    pub const MINMAJ9: &[i32] = &[3, 4, 4, 3];

    // augmented
    pub const AUG: &[i32] = &[4, 4];
    pub const AUGMAJ11: &[i32] = &[4, 4, 3, 3, 3];
    pub const AUGMAJ7: &[i32] = &[4, 4, 3];
    pub const AUGMAJ9: &[i32] = &[4, 4, 3, 3];

    // diminished
    pub const DIM: &[i32] = &[3, 3];
    pub const DIM7: &[i32] = &[3, 3, 3];
    pub const DIM9: &[i32] = &[3, 3, 3, 5];
    pub const DIM11: &[i32] = &[3, 3, 3, 5, 3];
    pub const DIM11B9: &[i32] = &[3, 3, 3, 4, 4];
    pub const DIM13B9: &[i32] = &[3, 3, 3, 4, 8];
    pub const DIMADD11: &[i32] = &[3, 3, 11];
    pub const DIMADD13: &[i32] = &[3, 3, 15];
    pub const DIMB7: &[i32] = &[3, 3, 4];
    pub const DIMB9: &[i32] = &[3, 3, 3, 4];

    // suspended
    pub const SUS2: &[i32] = &[2, 5];
    pub const SUS4: &[i32] = &[5, 2];
    pub const MAJ7SUS2: &[i32] = &[2, 5, 4];
    pub const MAJ7SUS4: &[i32] = &[5, 2, 4];

    // dominant
    pub const DOM7: &[i32] = &[4, 3, 3];
    pub const DOM7B9: &[i32] = &[4, 3, 3, 3];
    pub const DOM7SUS2: &[i32] = &[2, 5, 3];
    pub const DOM7SUS4: &[i32] = &[5, 2, 3];
    pub const DOM9: &[i32] = &[4, 3, 3, 4];
    pub const DOM11: &[i32] = &[4, 3, 3, 4, 3];
    pub const DOM11B9: &[i32] = &[4, 3, 3, 3, 4];
    pub const DOM11S: &[i32] = &[4, 3, 3, 4, 4];
    pub const DOM13: &[i32] = &[4, 3, 3, 4, 3, 4];
    pub const DOM13B: &[i32] = &[4, 3, 3, 4, 6];
    pub const DOM13B9: &[i32] = &[4, 3, 3, 3, 8];

    // no third
    pub const NO3D: &[i32] = &[7];

    pub const ALL_CHORD_TYPES: &[&[i32]] = &[
        MAJ, MAJ7, MAJ9, MAJ911S, MAJS9, MAJS911S, MAJ11, MAJ13, ADD9, ADD11, ADD13, MAJ1311S, MIN,
        MIN7, MIN9, MIN11, MIN1113B, MIN13, MINADD9, MINADD11, MINADD13, MINB9, MINMAJ11, MINMAJ13,
        MINMAJ7, MINMAJ9, AUG, AUGMAJ11, AUGMAJ7, AUGMAJ9, DIM, DIM7, DIM9, DIM11, DIM11B9,
        DIM13B9, DIMADD11, DIMADD13, DIMB7, DIMB9, SUS2, SUS4, MAJ7SUS2, MAJ7SUS4, DOM7, DOM7B9,
        DOM7SUS2, DOM7SUS4, DOM9, DOM11, DOM11B9, DOM11S, DOM13, DOM13B, DOM13B9, NO3D,
    ];
}

#[allow(dead_code)]
pub mod scale {
    use super::DIV;

    pub const MAJOR: &[i32] = &[2, 2, 1, 2, 2, 2, 1];
    pub const HARMONIC_MINOR: &[i32] = &[2, 1, 2, 2, 1, 3, 1];
    pub const MELODIC_MINOR: &[i32] = &[2, 1, 2, 2, 2, 2, 1];
    pub const PENTATONIC: &[i32] = &[2, 2, 3, 2, 3];
    pub const BLUES: &[i32] = &[3, 2, 1, 1, 3, 2];
    pub const WHOLE_TONE: &[i32] = &[2, 2, 2, 2, 2, 2];
    pub const DIMINISHED: &[i32] = &[2, 1, 2, 1, 2, 1, 2, 1];
    pub const CHROMATIC: &[i32] = &[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];

    pub fn is_scale(intervals: &[i32]) -> bool {
        intervals.iter().sum::<i32>() == DIV as i32
    }
}

/// Store ordered pitch set in a single 32-bit integer.
///
/// Can be used to express chords and scales.
///
/// Value 1 for active pitch, 0 for inactive.
/// Bits in position [0, 12) is for chord notes, multi-hot;
/// bits in position [12, 24) is for bass note, one-hot;
/// bits in position [24, 28) is a 4-bit unsigned integer, root note index;
/// bits in position [28, 32) is preserved.
///
/// ```text  
/// 32                             0  
/// ....iiiibbbbbbbbbbbbcccccccccccc  
///         B A G FE D CB A G FE D C  
/// ```
///
/// If root note index is 0, the chord is in root C, which means no transposition is applied.
/// If root note index is not 0, the notes in the chord is not the bits in position [0, 12),
/// but the bits rotated left by root note index.
/// The bass note is in the same way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PitchOrderedSet {
    data: u32,
}

// Macro to generate consuming `with_*` wrappers that delegate to existing `&mut` methods.
macro_rules! impl_with {
    ($with_name:ident, $set_name:ident, $arg_ty:ty) => {
        pub fn $with_name(mut self, arg: $arg_ty) -> Self {
            self.$set_name(arg);
            self
        }
    };
}

macro_rules! impl_with_noarg {
    ($with_name:ident, $set_name:ident) => {
        pub fn $with_name(mut self) -> Self {
            self.$set_name();
            self
        }
    };
}

#[allow(dead_code)]
impl PitchOrderedSet {
    const CHORD_MASK: u32 = (1u32 << DIV) - 1;
    const BASE_MASK: u32 = Self::CHORD_MASK << DIV;

    pub fn new() -> Self {
        Self { data: 0 }
    }

    pub fn from_data(data: u32) -> Self {
        Self { data }
    }

    /// Constructs a PitchOrderedSet.
    /// Corresponds to `PitchOrderedSet(Pitch root, const std::vector<int> &intervals)` in C++.
    /// The base defaults to the root.
    pub fn from_intervals(root: Pitch, intervals: &[i32]) -> Self {
        let data = Self::gen_data(root, intervals, root);
        Self { data }
    }

    /// Constructs a PitchOrderedSet with an explicit base.
    /// Corresponds to `PitchOrderedSet(Pitch root, const std::vector<int> &intervals, Pitch base)` in C++.
    pub fn from_intervals_with_base(root: Pitch, intervals: &[i32], base: Pitch) -> Self {
        let data = Self::gen_data(root, intervals, base);
        Self { data }
    }

    pub fn to_string(&self) -> String {
        let chordbase = self.chordbase();
        if let Some((name, base)) = crate::chord::get_chord_info(chordbase) {
            let root = PITCH_NAMES[self.root() as usize];
            if base == 0 {
                format!("{}{}", root, name)
            } else {
                let base_name = PITCH_NAMES[(self.root() + base) as usize % DIV];
                format!("{}{}/{}", root, name, base_name)
            }
        } else {
            if chordbase == 0 {
                return "N.C.".to_string();
            }
            let detail = format!("{:032b}", self.data);
            let chord_bits = &detail[20..32];
            let base_bits = &detail[8..20];
            let root_bits = &detail[0..8];
            format!(
                "Unknown(chord bits: {}\n base bits: {}\n root bits: {})",
                chord_bits, base_bits, root_bits
            )
        }
    }

    pub fn get_data(&self) -> u32 {
        self.data
    }

    pub fn chord(&self) -> u32 {
        self.data & Self::CHORD_MASK
    }

    pub fn base(&self) -> u32 {
        (self.data & Self::BASE_MASK) >> DIV
    }

    pub fn chordbase(&self) -> u32 {
        self.data & (Self::CHORD_MASK | Self::BASE_MASK)
    }

    pub fn root(&self) -> u32 {
        (self.data >> (DIV * 2)) & 0xF
    }

    pub fn set_note(&mut self, note: Pitch) -> &mut Self {
        self.data |= 1 << ((note.value() as u32) % DIV as u32);
        self
    }

    pub fn reset_note(&mut self, note: Pitch) -> &mut Self {
        self.data &= !(1 << ((note.value() as u32) % DIV as u32));
        self
    }

    pub fn set_base_empty(&mut self) -> &mut Self {
        self.data &= !Self::BASE_MASK;
        self
    }

    pub fn set_base(&mut self, note: Pitch) -> &mut Self {
        self.data &= !Self::BASE_MASK;
        self.data |= 1 << ((note.value() as u32) % DIV as u32 + DIV as u32);
        self
    }

    pub fn merge_base(&mut self) -> &mut Self {
        self.data = self.chord() | self.base();
        self
    }

    impl_with!(with_note, set_note, Pitch);
    impl_with!(without_note, reset_note, Pitch);
    impl_with_noarg!(with_base_empty, set_base_empty);
    impl_with!(with_base, set_base, Pitch);
    impl_with_noarg!(merged_base, merge_base);

    /// Get actual chord notes
    pub fn normalized(&self) -> Self {
        let r = self.root();
        let chord = self.chord();
        let base = self.base();

        if r == 0 {
            return Self::from_data(self.data);
        }

        let norm_chord = self.rotr_12bit(chord, r as i32);
        let norm_base = self.rotr_12bit(base, r as i32);
        let norm_data = norm_chord | (norm_base << DIV);
        Self::from_data(norm_data)
    }

    pub fn transposed(&self, semitones: i32) -> Self {
        let r = self.root();
        // Calculation logic from C++:
        // auto r_new = static_cast<uint32_t>(static_cast<int32_t>(r) + semitones + DIV) % DIV;
        let r_new = ((r as i32 + semitones + DIV as i32) as u32) % DIV as u32;

        let data_new = code(self.data) | (r_new << (DIV * 2));
        Self::from_data(data_new)
    }

    /// Return true if data.chord contains others.data.chord
    pub fn contains_chord(&self, other: &PitchOrderedSet) -> bool {
        let a = other.data & Self::CHORD_MASK;
        let b = self.data & Self::CHORD_MASK;
        (a & b) == a
    }

    /// Return true if data.chord contains others.data.(chord|base)
    pub fn contains_chord_with_base(&self, other: &PitchOrderedSet) -> bool {
        let a = other.data & Self::CHORD_MASK;
        let b = (other.data & Self::BASE_MASK) >> DIV;
        let ab = a | b;
        let c = self.data & Self::CHORD_MASK;
        (ab & c) == ab
    }

    /// Return true if data.chord contains others.data.chord
    /// and if data.base contains others.data.base
    pub fn contains(&self, other: &PitchOrderedSet) -> bool {
        const MASK: u32 = PitchOrderedSet::CHORD_MASK | PitchOrderedSet::BASE_MASK;
        let a = other.data & MASK;
        let b = self.data & MASK;
        (a & b) == a
    }

    pub fn size(&self) -> u32 {
        let chord = self.data & Self::CHORD_MASK;
        chord.count_ones()
    }

    fn gen_data(root: Pitch, intervals: &[i32], base: Pitch) -> u32 {
        let mut data = 0u32;
        let mut p = root.value() as i32;
        data |= 1u32 << ((p % DIV as i32) as u32);

        for &step in intervals {
            p += step;
            data |= 1u32 << ((p % DIV as i32) as u32);
        }

        data |= 1u32 << ((base.value() as u32) % DIV as u32 + DIV as u32);
        data |= (root.value() as u32 % DIV as u32) << (DIV * 2);

        data
    }

    fn rotl_12bit(&self, x: u32, n: i32) -> u32 {
        ((x << n) | (x >> (12 - n))) & Self::CHORD_MASK
    }

    fn rotr_12bit(&self, x: u32, n: i32) -> u32 {
        ((x >> n) | (x << (12 - n))) & Self::CHORD_MASK
    }
}

impl Default for PitchOrderedSet {
    fn default() -> Self {
        Self::new()
    }
}

impl std::ops::BitOr for PitchOrderedSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            data: self.chord() | self.base() | rhs.chord() | rhs.base(),
        }
    }
}

impl std::ops::BitOrAssign for PitchOrderedSet {
    fn bitor_assign(&mut self, rhs: Self) {
        self.data = self.chord() | self.base() | rhs.chord() | rhs.base();
    }
}

impl fmt::Display for PitchOrderedSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
