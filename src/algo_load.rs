//! Dynamic algorithm plugin loader — scans and loads .dll / .so / .dylib files
//! located in the same directory as the running executable.
//!
//! The plugin is expected to export the symbol with C linkage:
//! ```cpp
//! extern "C" uint32_t sample_next_chord(uint32_t input, const NoteData *input_notes, size_t note_count, uint32_t beats_per_bar);
//! ```
//!
//! Example usage in Rust:
//! ```ignore
//! let algos = algo_load::find_algos_in_exe_dir().unwrap_or_default();
//! for a in &algos {
//!     if a.has_sample_next_chord() {
//!         // example: input=42, no notes, 4 beats per bar
//!         let out = a.sample_next_chord(42, &[], 4).unwrap();
//!         println!("{} -> {}", a.name, out);
//!     }
//! }
//! ```

use std::os::raw::c_char;
use std::{
    env,
    ffi::{CStr, CString, OsStr},
    fs, io,
    path::{Path, PathBuf},
};

use libloading::Library;

/// C function signature: uint32_t sample_next_chord(uint32_t input, const NoteData *input_notes, size_t note_count, uint32_t beats_per_bar);
///
/// The `NoteData` structure (C layout):
/// ```c
/// struct NoteData {
///     int pitch;
///     float start;   // normalized to [0,1] within the measure
///     float end;     // normalized to [0,1] within the measure
///     float velocity;// normalized to [0,1]
/// };
/// ```
#[repr(C)]
#[derive(Copy, Clone)]
pub struct NoteData {
    pub pitch: i32,
    pub start: f32,
    pub end: f32,
    pub velocity: f32,
}

/// C function signature: uint32_t sample_next_chord(uint32_t input, const NoteData *input_notes, size_t note_count, uint32_t beats_per_bar);
pub type SampleNextChordFn = unsafe extern "C" fn(u32, *const NoteData, usize, u32) -> u32;

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
    // Optional config API exposed by plugins (see example in request):
    // const char **get_option_names(size_t *out_count)
    get_option_names: Option<unsafe extern "C" fn(*mut usize) -> *const *const c_char>,
    // const char **get_options(const char *key, size_t *out_count)
    get_options: Option<unsafe extern "C" fn(*const c_char, *mut usize) -> *const *const c_char>,
    // const char *get_option(const char *key)
    get_option: Option<unsafe extern "C" fn(*const c_char) -> *const c_char>,
    // void set_option(const char *key, const char *value)
    set_option: Option<unsafe extern "C" fn(*const c_char, *const c_char)>,
    // Internal cache for options to avoid calling into plugin on every frame
    option_cache: std::cell::RefCell<Option<AlgoOptions>>,
}

#[derive(Debug, Clone)]
struct AlgoOptions {
    names: Vec<String>,
    choices: std::collections::HashMap<String, Vec<String>>,
    values: std::collections::HashMap<String, String>,
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
            // Optional config API symbols (may be missing)
            let get_option_names = lib
                .get::<unsafe extern "C" fn(*mut usize) -> *const *const c_char>(
                    b"get_option_names\0",
                )
                .map(|s| *s)
                .ok();
            let get_options = lib
                .get::<unsafe extern "C" fn(*const c_char, *mut usize) -> *const *const c_char>(
                    b"get_options\0",
                )
                .map(|s| *s)
                .ok();
            let get_option = lib
                .get::<unsafe extern "C" fn(*const c_char) -> *const c_char>(b"get_option\0")
                .map(|s| *s)
                .ok();
            let set_option = lib
                .get::<unsafe extern "C" fn(*const c_char, *const c_char)>(b"set_option\0")
                .map(|s| *s)
                .ok();

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
                get_option_names,
                get_options,
                get_option,
                set_option,
                option_cache: std::cell::RefCell::new(None),
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

    /// Call `sample_next_chord` with normalized note data and the number of beats per bar. Returns `Some(result)` if available.
    #[allow(dead_code)]
    pub fn sample_next_chord(
        &self,
        input: u32,
        notes: &[NoteData],
        beats_per_bar: u32,
    ) -> Option<u32> {
        let ptr = if notes.is_empty() {
            std::ptr::null()
        } else {
            notes.as_ptr()
        };
        self.sample_next_chord
            .map(|f| unsafe { f(input, ptr, notes.len(), beats_per_bar) })
    }

    /// Return the filename without extension (file stem) if available.
    pub fn file_stem(&self) -> Option<String> {
        self.path
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| s.to_string())
    }

    /// Returns true if the plugin exposes the option API.
    pub fn has_option_api(&self) -> bool {
        self.get_option_names.is_some() && self.get_options.is_some()
    }

    /// Low-level fetch of option names (no caching). Returns None if function missing.
    fn option_names(&self) -> Option<Vec<String>> {
        let f = self.get_option_names?;
        unsafe {
            let mut count: usize = 0;
            let ptr = f(&mut count as *mut usize);
            if ptr.is_null() || count == 0 {
                return Some(Vec::new());
            }
            let slice = std::slice::from_raw_parts(ptr, count);
            let mut out = Vec::new();
            for &p in slice {
                if p.is_null() {
                    continue;
                }
                let s = CStr::from_ptr(p).to_string_lossy().into_owned();
                out.push(s);
            }
            Some(out)
        }
    }

    /// Low-level fetch of possible values for a key (no caching). Returns None if function missing.
    fn options_for(&self, key: &str) -> Option<Vec<String>> {
        let f = self.get_options?;
        let ckey = match CString::new(key) {
            Ok(c) => c,
            Err(_) => return Some(Vec::new()),
        };
        unsafe {
            let mut count: usize = 0;
            let ptr = f(ckey.as_ptr(), &mut count as *mut usize);
            if ptr.is_null() || count == 0 {
                return Some(Vec::new());
            }
            let slice = std::slice::from_raw_parts(ptr, count);
            let mut out = Vec::new();
            for &p in slice {
                if p.is_null() {
                    continue;
                }
                let s = CStr::from_ptr(p).to_string_lossy().into_owned();
                out.push(s);
            }
            Some(out)
        }
    }

    /// Low-level get current option value (no caching). Returns None if function missing or value null.
    fn get_option_value(&self, key: &str) -> Option<String> {
        let f = self.get_option?;
        let ckey = CString::new(key).ok()?;
        unsafe {
            let ptr = f(ckey.as_ptr());
            if ptr.is_null() {
                return None;
            }
            Some(CStr::from_ptr(ptr).to_string_lossy().into_owned())
        }
    }

    /// Refresh and cache option names, their possible values, and current values.
    pub fn refresh_option_cache(&self) {
        if !self.has_option_api() {
            self.option_cache.replace(None);
            return;
        }
        let names = match self.option_names() {
            Some(n) => n,
            None => {
                self.option_cache.replace(None);
                return;
            }
        };
        let mut choices = std::collections::HashMap::new();
        let mut values = std::collections::HashMap::new();
        for k in &names {
            if let Some(opts) = self.options_for(k) {
                choices.insert(k.clone(), opts.clone());
            }
            if let Some(v) = self.get_option_value(k) {
                values.insert(k.clone(), v);
            }
        }
        let ao = AlgoOptions {
            names,
            choices,
            values,
        };
        self.option_cache.replace(Some(ao));
    }

    /// Get cached option names; refresh cache on first access.
    pub fn get_cached_option_names(&self) -> Option<Vec<String>> {
        if self.option_cache.borrow().is_none() {
            self.refresh_option_cache();
        }
        self.option_cache.borrow().as_ref().map(|a| a.names.clone())
    }

    /// Get cached choices for a key (refresh cache if needed).
    pub fn get_cached_options_for(&self, key: &str) -> Option<Vec<String>> {
        if self.option_cache.borrow().is_none() {
            self.refresh_option_cache();
        }
        self.option_cache
            .borrow()
            .as_ref()
            .and_then(|a| a.choices.get(key).cloned())
            .or(Some(Vec::new()))
    }

    /// Get cached current value for a key (refresh cache if needed).
    pub fn get_cached_option_value(&self, key: &str) -> Option<String> {
        if self.option_cache.borrow().is_none() {
            self.refresh_option_cache();
        }
        self.option_cache
            .borrow()
            .as_ref()
            .and_then(|a| a.values.get(key).cloned())
    }

    /// Set an option (if `set_option` is exported). Updates cache accordingly.
    pub fn set_option_value(&self, key: &str, value: &str) -> Result<(), String> {
        let f = self
            .set_option
            .ok_or_else(|| "set_option not exported".to_string())?;
        let ckey = CString::new(key).map_err(|_| "invalid key".to_string())?;
        let cval = CString::new(value).map_err(|_| "invalid value".to_string())?;
        unsafe {
            f(ckey.as_ptr(), cval.as_ptr());
        }
        // update cache if present
        if let Some(opt) = self.option_cache.borrow_mut().as_mut() {
            opt.values.insert(key.to_string(), value.to_string());
        }
        Ok(())
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
