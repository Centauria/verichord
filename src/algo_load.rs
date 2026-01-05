//! Dynamic algorithm plugin loader — scans and loads .dll / .so / .dylib files
//! located in the same directory as the running executable.
//!
//! The plugin is expected to export the symbol with C linkage:
//! ```cpp
//! extern "C" uint32_t sample_next_chord(uint32_t input);
//! ```
//!
//! Example usage in Rust:
//! ```ignore
//! let algos = algo_load::find_algos_in_exe_dir().unwrap_or_default();
//! for a in &algos {
//!     if a.has_sample_next_chord() {
//!         let out = a.sample_next_chord(42).unwrap();
//!         println!("{} -> {}", a.name, out);
//!     }
//! }
//! ```

use std::{
    env,
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
};

use libloading::Library;

/// C function signature: uint32_t sample_next_chord(uint32_t input);
pub type SampleNextChordFn = unsafe extern "C" fn(u32) -> u32;

/// Represents a loaded algorithm library. Keeps the `Library` handle alive
/// to ensure any obtained function pointers remain valid while the struct lives.
///
/// Fields:
/// - `name`: filename of the shared library.
/// - `path`: full path to the shared library file.
/// - `_lib`: the `Library` handle (kept private to keep it alive).
/// - `sample_next_chord`: optional function pointer for the exposed symbol.
#[derive(Debug)]
pub struct AlgoLib {
    pub name: String,
    pub path: PathBuf,
    // Keep the library handle so that function pointers remain valid.
    _lib: Library,
    sample_next_chord: Option<SampleNextChordFn>,
}

impl AlgoLib {
    /// Load the library from `path`. If the library opens but does not export
    /// `sample_next_chord`, the method still returns `Ok(AlgoLib)` with
    /// `has_sample_next_chord()` == false.
    pub fn load(path: impl AsRef<Path>) -> Result<Self, libloading::Error> {
        let path = path.as_ref().to_path_buf();
        unsafe {
            let lib = Library::new(&path)?;
            // Attempt to fetch the symbol; we require exact symbol name with null terminator:
            let symbol = lib.get::<SampleNextChordFn>(b"sample_next_chord\0");
            let sample_next_chord = match symbol {
                Ok(sym) => Some(*sym), // copy function pointer
                Err(_) => None,
            };
            let name = path
                .file_name()
                .and_then(OsStr::to_str)
                .map(|s| s.to_string())
                .unwrap_or_else(|| "<unknown>".into());
            Ok(AlgoLib {
                name,
                path,
                _lib: lib,
                sample_next_chord,
            })
        }
    }

    /// Returns true if the library exported `sample_next_chord`.
    pub fn has_sample_next_chord(&self) -> bool {
        self.sample_next_chord.is_some()
    }

    /// If exported, return the raw function pointer for `sample_next_chord`.
    /// The returned function pointer is safe to copy; the underlying library
    /// handle is kept alive by this `AlgoLib`, so the pointer remains valid
    /// while this `AlgoLib` instance lives.
    pub fn sample_next_chord_fn(&self) -> Option<SampleNextChordFn> {
        self.sample_next_chord
    }

    /// Call `sample_next_chord`. Returns `Some(result)` if available, otherwise `None`.
    #[allow(dead_code)]
    pub fn sample_next_chord(&self, input: u32) -> Option<u32> {
        self.sample_next_chord.map(|f| unsafe { f(input) })
    }

    /// Return the filename without extension (file stem) if available.
    pub fn file_stem(&self) -> Option<String> {
        self.path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
    }
}

/// Check whether a path looks like a shared library by its extension.
fn is_shared_lib(path: &Path) -> bool {
    path.extension()
        .and_then(OsStr::to_str)
        .map(|ext| {
            let ext = ext.to_lowercase();
            ext == "dll" || ext == "so" || ext == "dylib"
        })
        .unwrap_or(false)
}

/// Scan `dir` for shared libraries and attempt to load them.
/// Libraries that fail to open are ignored (but could be logged).
pub fn find_algos_in_dir(dir: impl AsRef<Path>) -> io::Result<Vec<AlgoLib>> {
    let mut found = Vec::new();
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        if !is_shared_lib(&path) {
            continue;
        }
        match AlgoLib::load(&path) {
            Ok(algo) => found.push(algo),
            Err(_err) => {
                // Ignore libraries that fail to load (e.g. missing dependencies).
                // Consider logging `_err` for debugging.
            }
        }
    }
    Ok(found)
}

/// Find and load algorithm libraries located in the executable's directory.
pub fn find_algos_in_exe_dir() -> io::Result<Vec<AlgoLib>> {
    let exe = env::current_exe()?;
    let dir = exe
        .parent()
        .ok_or_else(|| io::Error::new(io::ErrorKind::Other, "can't get executable directory"))?;
    find_algos_in_dir(dir)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_shared_lib() {
        assert!(is_shared_lib(Path::new("a.dll")));
        assert!(is_shared_lib(Path::new("libm.so")));
        assert!(is_shared_lib(Path::new("libm.dylib")));
        assert!(!is_shared_lib(Path::new("text.txt")));
        assert!(!is_shared_lib(Path::new("noext")));
    }
}
