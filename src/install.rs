use std::env;
use std::fs::{self, File};
use std::io::{BufWriter, Cursor};
use std::path::PathBuf;
use std::process::{Command, Stdio};

use resvg::tiny_skia;
use resvg::usvg;

/// Create an app, optionally installing to `dest`:
/// - macOS: `dest` may be an absolute `.app` bundle path or a directory (the `.app` will be created
///   inside it). If `None`, installs to `~/Applications/VeriChord.app`.
/// - Linux: `dest` may be an absolute `.desktop` file path, or a directory (the `.desktop` will be
///   created inside it). If `None`, installs to `~/.local/share/applications/verichord.desktop`.
/// - Windows: `dest` may be an absolute `.bat`/`.exe`/`.lnk` path, or a directory (a `.bat` will
///   be created inside it). If `None`, installs to the Desktop.
pub fn create_app_at(dest: Option<PathBuf>) -> Result<PathBuf, Box<dyn std::error::Error>> {
    if cfg!(target_os = "macos") {
        create_macos_app_at(dest)
    } else if cfg!(target_os = "linux") {
        create_linux_desktop_entry_at(dest)
    } else if cfg!(target_os = "windows") {
        create_windows_shortcut_at(dest)
    } else {
        Err("Unsupported target OS for create_app".into())
    }
}

fn create_macos_app_at(dest: Option<PathBuf>) -> Result<PathBuf, Box<dyn std::error::Error>> {
    let app_display = display_name_from_package();
    let app_path = match dest {
        Some(p) => {
            if p.extension().and_then(|s| s.to_str()) == Some("app") {
                p
            } else {
                p.join(format!("{}.app", app_display))
            }
        }
        None => {
            let user_dirs = directories::UserDirs::new()
                .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
            user_dirs
                .home_dir()
                .join("Applications")
                .join(format!("{}.app", app_display))
        }
    };

    let contents = app_path.join("Contents");
    let macos_dir = contents.join("MacOS");
    let resources_dir = contents.join("Resources");
    fs::create_dir_all(&macos_dir)?;
    fs::create_dir_all(&resources_dir)?;

    // Create icns in resources
    create_icns_from_svg(resources_dir.join("logo.icns"))?;

    // Copy current executable into the bundle
    let exe = std::env::current_exe()?;
    let dest_exe = macos_dir.join(&app_display);
    fs::copy(&exe, &dest_exe)?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&dest_exe)?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&dest_exe, perms)?;
    }

    // Write Info.plist
    let plist = format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN"
 "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>{name}</string>
  <key>CFBundleDisplayName</key>
  <string>{name}</string>
  <key>CFBundleIdentifier</key>
  <string>com.example.{pkg}</string>
  <key>CFBundleVersion</key>
  <string>{version}</string>
  <key>CFBundleExecutable</key>
  <string>{name}</string>
  <key>CFBundleIconFile</key>
  <string>logo.icns</string>
</dict>
</plist>
"#,
        name = app_display,
        pkg = env!("CARGO_PKG_NAME"),
        version = env!("CARGO_PKG_VERSION"),
    );
    fs::write(contents.join("Info.plist"), plist.as_bytes())?;

    Ok(app_path)
}

fn create_linux_desktop_entry_at(
    dest: Option<PathBuf>,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    if dest.is_none() {
        return create_linux_desktop_entry();
    }

    let p = dest.unwrap();
    let desktop_path = if p.extension().and_then(|s| s.to_str()) == Some("desktop") {
        p
    } else {
        p.join(format!("{}.desktop", env!("CARGO_PKG_NAME")))
    };

    if let Some(parent) = desktop_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let icon_path = desktop_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .join(format!("{}.png", env!("CARGO_PKG_NAME")));
    if let Some(parent) = icon_path.parent() {
        fs::create_dir_all(parent)?;
    }

    // Generate PNG icon next to the desktop file (or in custom dir)
    create_png_from_svg(&icon_path, 256)?;

    let exe = std::env::current_exe()?;
    let exec = exe
        .to_str()
        .ok_or_else(|| "executable path is not valid Unicode")?;
    let desktop_contents = format!(
        "[Desktop Entry]\nName={name}\nComment=VeriChord\nExec={exec}\nIcon={icon}\nType=Application\nCategories=Audio;Music;\nTerminal=false\n",
        name = display_name_from_package(),
        exec = exec,
        icon = icon_path.display(),
    );

    fs::write(&desktop_path, desktop_contents.as_bytes())?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&desktop_path)?.permissions();
        perms.set_mode(0o644);
        fs::set_permissions(&desktop_path, perms)?;
    }

    Ok(desktop_path)
}

fn create_windows_shortcut_at(
    dest: Option<PathBuf>,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    if dest.is_none() {
        return create_windows_shortcut();
    }

    let p = dest.unwrap();
    let exe = std::env::current_exe()?;
    let exe_str = exe
        .to_str()
        .ok_or_else(|| "executable path is not valid Unicode")?;

    let target = if p
        .extension()
        .and_then(|s| s.to_str())
        .map(|ext| {
            let ext = ext.to_ascii_lowercase();
            ext == "bat" || ext == "exe" || ext == "lnk"
        })
        .unwrap_or(false)
    {
        p
    } else {
        p.join(format!("{}.bat", display_name_from_package()))
    };

    if target
        .extension()
        .and_then(|s| s.to_str())
        .map(|e| e.eq_ignore_ascii_case("exe"))
        .unwrap_or(false)
    {
        fs::copy(&exe, &target)?;
    } else {
        let bat_contents = format!("@echo off\r\nstart \"\" \"{}\" %*\r\n", exe_str);
        fs::write(&target, bat_contents.as_bytes())?;
    }

    Ok(target)
}

/// Create an application artifact directly from a given executable path.
/// On macOS: creates `<dest or ~/Applications>/<AppName>.app` and copies `exe_path` into Contents/MacOS.
/// On Linux: writes a `.desktop` file (Exec set to `exe_path`) and a PNG icon near it.
/// On Windows: creates a `.bat` launcher (or copies exe if `.exe` target) on the Desktop or `dest`.
pub fn create_app_from_exe(
    exe_path: &std::path::Path,
    dest: Option<PathBuf>,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    // macOS bundle creation
    #[cfg(target_os = "macos")]
    {
        let app_display = display_name_from_package();
        let app_path = match dest {
            Some(p) => {
                if p.extension().and_then(|s| s.to_str()) == Some("app") {
                    p
                } else {
                    p.join(format!("{}.app", app_display))
                }
            }
            None => {
                let user_dirs = directories::UserDirs::new()
                    .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
                user_dirs
                    .home_dir()
                    .join("Applications")
                    .join(format!("{}.app", app_display))
            }
        };

        let contents = app_path.join("Contents");
        let macos_dir = contents.join("MacOS");
        let resources_dir = contents.join("Resources");
        fs::create_dir_all(&macos_dir)?;
        fs::create_dir_all(&resources_dir)?;

        // Create icns
        create_icns_from_svg(resources_dir.join("logo.icns"))?;

        // Copy provided executable into the bundle
        let dest_exe = macos_dir.join(&app_display);
        fs::copy(exe_path, &dest_exe)?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&dest_exe)?.permissions();
            perms.set_mode(0o755);
            fs::set_permissions(&dest_exe, perms)?;
        }

        // Write Info.plist
        let plist = format!(
            r#"<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN"
 "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleName</key>
  <string>{name}</string>
  <key>CFBundleDisplayName</key>
  <string>{name}</string>
  <key>CFBundleIdentifier</key>
  <string>com.example.{pkg}</string>
  <key>CFBundleVersion</key>
  <string>{version}</string>
  <key>CFBundleExecutable</key>
  <string>{name}</string>
  <key>CFBundleIconFile</key>
  <string>logo.icns</string>
</dict>
</plist>
"#,
            name = app_display,
            pkg = env!("CARGO_PKG_NAME"),
            version = env!("CARGO_PKG_VERSION"),
        );
        fs::write(contents.join("Info.plist"), plist.as_bytes())?;
        Ok(app_path)
    }

    // Linux desktop entry creation from specific exe
    #[cfg(target_os = "linux")]
    {
        let p = dest.unwrap_or_else(|| {
            let user_dirs = directories::UserDirs::new().expect("user dirs");
            user_dirs.home_dir().join(".local/share/applications")
        });
        let desktop_path = if p.extension().and_then(|s| s.to_str()) == Some("desktop") {
            p
        } else {
            p.join(format!("{}.desktop", env!("CARGO_PKG_NAME")))
        };

        if let Some(parent) = desktop_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let icon_path = desktop_path
            .parent()
            .unwrap_or_else(|| std::path::Path::new("."))
            .join(format!("{}.png", env!("CARGO_PKG_NAME")));
        if let Some(parent) = icon_path.parent() {
            fs::create_dir_all(parent)?;
        }
        create_png_from_svg(&icon_path, 256)?;

        let exec = exe_path
            .to_str()
            .ok_or_else(|| "executable path is not valid Unicode")?;
        let desktop_contents = format!(
            "[Desktop Entry]\nName={name}\nComment=VeriChord\nExec={exec}\nIcon={icon}\nType=Application\nCategories=Audio;Music;\nTerminal=false\n",
            name = display_name_from_package(),
            exec = exec,
            icon = icon_path.display(),
        );
        fs::write(&desktop_path, desktop_contents.as_bytes())?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(&desktop_path)?.permissions();
            perms.set_mode(0o644);
            fs::set_permissions(&desktop_path, perms)?;
        }
        Ok(desktop_path)
    }

    // Windows: simple .bat or copy behavior
    #[cfg(target_os = "windows")]
    {
        let user_dirs =
            directories::UserDirs::new().ok_or_else(|| "failed to determine user dirs")?;
        let desktop = user_dirs
            .desktop_dir()
            .ok_or_else(|| "couldn't find Desktop directory")?;

        let target = match dest {
            Some(p) => {
                if p.is_dir() {
                    p.join(format!("{}.bat", display_name_from_package()))
                } else {
                    p
                }
            }
            None => desktop.join(format!("{}.bat", display_name_from_package())),
        };

        if target
            .extension()
            .and_then(|s| s.to_str())
            .map(|e| e.eq_ignore_ascii_case("exe"))
            .unwrap_or(false)
        {
            fs::copy(exe_path, &target)?;
        } else {
            let exe_str = exe_path
                .to_str()
                .ok_or_else(|| "executable path is not valid Unicode")?;
            let bat_contents = format!("@echo off\r\nstart \"\" \"{}\" %*\r\n", exe_str);
            fs::write(&target, bat_contents.as_bytes())?;
        }
        Ok(target)
    }

    #[cfg(not(any(target_os = "macos", target_os = "linux", target_os = "windows")))]
    {
        Err("Unsupported target OS for create_app_from_exe".into())
    }
}

const LOGO_SVG: &[u8] = include_bytes!("../assets/logo.svg");

const INSTALLER_USAGE: &str = r#"verichord-installer

Usage:
  verichord-installer [--path <PATH>] [--no-build] [--install]

Options:
  --path <PATH>   Custom installation target (e.g. .app path on macOS, .desktop on Linux)
  --no-build      Skip `cargo build --release` (use existing `target/release/verichord`)
  --install       Run `cargo install --path .` and then create app from installed binary
  --help          Show this help
"#;

fn rasterize_png_bytes(svg_bytes: &[u8], size: u32) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    // Parse SVG
    let options = usvg::Options::default();
    let rtree = usvg::Tree::from_data(svg_bytes, &options)
        .map_err(|e| format!("failed to parse svg: {}", e))?;

    // Render to pixmap
    let width = size;
    let height = size;
    let mut pixmap =
        tiny_skia::Pixmap::new(width, height).ok_or_else(|| "failed to create pixmap")?;

    let tree_size = rtree.size();
    let scale_x = (width as f32) / tree_size.width();
    let scale_y = (height as f32) / tree_size.height();
    let scale = scale_x.min(scale_y);
    let transform = tiny_skia::Transform::from_scale(scale, scale);
    let mut pixmap_mut = pixmap.as_mut();
    resvg::render(&rtree, transform, &mut pixmap_mut);

    // Convert premultiplied alpha -> straight RGBA
    let mut data = pixmap.data().to_vec();
    for px in data.chunks_mut(4) {
        let a = px[3] as u32;
        if a == 0 {
            px[0] = 0;
            px[1] = 0;
            px[2] = 0;
        } else if a < 255 {
            px[0] = ((px[0] as u32) * 255 / a) as u8;
            px[1] = ((px[1] as u32) * 255 / a) as u8;
            px[2] = ((px[2] as u32) * 255 / a) as u8;
        }
    }

    // Encode PNG
    let mut png_buf: Vec<u8> = Vec::new();
    {
        let mut encoder = png::Encoder::new(&mut png_buf, width, height);
        encoder.set_color(png::ColorType::Rgba);
        encoder.set_depth(png::BitDepth::Eight);
        let mut writer = encoder
            .write_header()
            .map_err(|e| format!("failed to start png encoder: {}", e))?;
        writer
            .write_image_data(&data)
            .map_err(|e| format!("failed to write png data: {}", e))?;
    }
    Ok(png_buf)
}

fn display_name_from_package() -> String {
    let pkg = env!("CARGO_PKG_NAME");
    // Smart-ish capitalization: "verichord" -> "Verichord"
    let mut chars = pkg.chars();
    match chars.next() {
        None => pkg.to_string(),
        Some(first) => {
            let rest: String = chars.collect();
            format!("{}{}", first.to_uppercase(), rest)
        }
    }
}

fn create_icns_from_svg<P: AsRef<std::path::Path>>(
    out_path: P,
) -> Result<(), Box<dyn std::error::Error>> {
    let svg_bytes: &[u8] = LOGO_SVG;

    // rtree parsing isn't needed here; rasterize_png_bytes handles SVG parsing.

    let sizes = [16u32, 32, 64, 128, 256, 512, 1024];
    let mut icon_family = icns::IconFamily::new();
    for &size in &sizes {
        let png_bytes = rasterize_png_bytes(svg_bytes, size)?;
        let img = icns::Image::read_png(Cursor::new(&png_bytes))
            .map_err(|e| format!("failed to decode png for icns: {}", e))?;
        let _ = icon_family.add_icon(&img);
    }

    let file = File::create(out_path)?;
    let mut writer = BufWriter::new(file);
    icon_family
        .write(&mut writer)
        .map_err(|e| format!("failed to write icns file: {}", e))?;
    Ok(())
}

fn create_png_from_svg<P: AsRef<std::path::Path>>(
    out_path: P,
    size: u32,
) -> Result<(), Box<dyn std::error::Error>> {
    let png = rasterize_png_bytes(LOGO_SVG, size)?;
    if let Some(parent) = out_path.as_ref().parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(out_path, &png)?;
    Ok(())
}

#[cfg(target_os = "linux")]
fn create_linux_desktop_entry() -> Result<PathBuf, Box<dyn std::error::Error>> {
    let user_dirs = directories::UserDirs::new()
        .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
    let home = user_dirs.home_dir();

    let applications_dir = home.join(".local/share/applications");
    let icons_dir = home.join(".local/share/icons/hicolor/256x256/apps");
    fs::create_dir_all(&applications_dir)?;
    fs::create_dir_all(&icons_dir)?;

    let app_display = display_name_from_package();
    let icon_path = icons_dir.join(format!("{}.png", env!("CARGO_PKG_NAME")));
    // Generate 256x256 PNG icon
    let png_bytes = rasterize_png_bytes(LOGO_SVG, 256)?;
    fs::write(&icon_path, png_bytes)?;

    let exe = std::env::current_exe()?;
    let exec = exe
        .to_str()
        .ok_or_else(|| "executable path is not valid Unicode")?;
    let desktop_contents = format!(
        "[Desktop Entry]\nName={name}\nComment=VeriChord\nExec={exec}\nIcon={icon}\nType=Application\nCategories=Audio;Music;\nTerminal=false\n",
        name = app_display,
        exec = exec,
        icon = env!("CARGO_PKG_NAME"),
    );

    let desktop_path = applications_dir.join(format!("{}.desktop", env!("CARGO_PKG_NAME")));
    fs::write(&desktop_path, desktop_contents.as_bytes())?;

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&desktop_path)?.permissions();
        perms.set_mode(0o644);
        fs::set_permissions(&desktop_path, perms)?;
    }

    Ok(desktop_path)
}

#[cfg(not(target_os = "linux"))]
fn create_linux_desktop_entry() -> Result<PathBuf, Box<dyn std::error::Error>> {
    Err("Linux desktop entry creation is only supported on Linux builds".into())
}

#[cfg(target_os = "windows")]
fn create_windows_shortcut() -> Result<PathBuf, Box<dyn std::error::Error>> {
    // As a lightweight fallback, create a small .bat on the user's Desktop which starts the installed exe.
    let user_dirs = directories::UserDirs::new()
        .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
    let desktop = user_dirs
        .desktop_dir()
        .ok_or_else(|| "couldn't find Desktop directory")?;

    let exe = std::env::current_exe()?;
    let exe_str = exe
        .to_str()
        .ok_or_else(|| "executable path is not valid Unicode")?;
    let bat_path = desktop.join(format!("{}.bat", display_name_from_package()));
    let bat_contents = format!("@echo off\r\nstart \"\" \"{}\" %*\r\n", exe_str);
    fs::write(&bat_path, bat_contents.as_bytes())?;
    Ok(bat_path)
}

#[cfg(not(target_os = "windows"))]
fn create_windows_shortcut() -> Result<PathBuf, Box<dyn std::error::Error>> {
    Err("Windows shortcut creation is only supported on Windows builds".into())
}

/// Uninstall from a custom destination:
/// - macOS: dest may be `.app` path or directory containing `<name>.app`.
/// - Linux: dest may be a `.desktop` file path or directory for the `.desktop` file.
/// - Windows: dest may be a `.bat`/`.exe`/`.lnk` path or a directory on the Desktop.
pub fn uninstall_app_at(dest: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    if cfg!(target_os = "macos") {
        uninstall_macos_app_at(dest)?;
    } else if cfg!(target_os = "linux") {
        uninstall_linux_desktop_entry_at(dest)?;
    } else if cfg!(target_os = "windows") {
        uninstall_windows_shortcut_at(dest)?;
    } else {
        return Err("Unsupported target OS for uninstall_app".into());
    }
    Ok(())
}

#[cfg(target_os = "macos")]
fn uninstall_macos_app_at(dest: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    let app_display = display_name_from_package();
    let app_path = match dest {
        Some(p) => {
            if p.extension().and_then(|s| s.to_str()) == Some("app") {
                p
            } else {
                p.join(format!("{}.app", app_display))
            }
        }
        None => {
            let user_dirs = directories::UserDirs::new()
                .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
            user_dirs
                .home_dir()
                .join("Applications")
                .join(format!("{}.app", app_display))
        }
    };
    if app_path.exists() {
        fs::remove_dir_all(&app_path)?;
        println!("Removed macOS app: {}", app_path.display());
    } else {
        println!("macOS app not found at {}", app_path.display());
    }
    Ok(())
}

#[cfg(not(target_os = "macos"))]
fn uninstall_macos_app_at(_dest: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    Err("macOS uninstall is only supported on macOS builds".into())
}

#[cfg(target_os = "linux")]
fn uninstall_linux_desktop_entry_at(
    dest: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    if dest.is_none() {
        return uninstall_linux_desktop_entry();
    }

    let p = dest.unwrap();
    let desktop_path = if p.extension().and_then(|s| s.to_str()) == Some("desktop") {
        p
    } else {
        p.join(format!("{}.desktop", env!("CARGO_PKG_NAME")))
    };

    if desktop_path.exists() {
        fs::remove_file(&desktop_path)?;
        println!("Removed desktop entry: {}", desktop_path.display());
    } else {
        println!("Desktop entry not found: {}", desktop_path.display());
    }

    let icon_path = desktop_path
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."))
        .join(format!("{}.png", env!("CARGO_PKG_NAME")));
    if icon_path.exists() {
        fs::remove_file(&icon_path)?;
        println!("Removed icon: {}", icon_path.display());
    } else {
        println!("Icon not found: {}", icon_path.display());
    }

    Ok(())
}

#[cfg(not(target_os = "linux"))]
fn uninstall_linux_desktop_entry_at(
    _dest: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    Err("Linux uninstall is only supported on Linux builds".into())
}

#[cfg(target_os = "windows")]
fn uninstall_windows_shortcut_at(dest: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    if dest.is_none() {
        return uninstall_windows_shortcut();
    }

    let p = dest.unwrap();
    if p.is_dir() {
        let bat = p.join(format!("{}.bat", display_name_from_package()));
        let exe = p.join("VeriChord.exe");
        let lnk = p.join(format!("{}.lnk", display_name_from_package()));
        for path in &[bat, exe, lnk] {
            if path.exists() {
                match fs::remove_file(path) {
                    Ok(_) => println!("Removed {}", path.display()),
                    Err(e) => eprintln!("Failed to remove {}: {}", path.display(), e),
                }
            }
        }
    } else {
        // single file path specified
        if p.exists() {
            fs::remove_file(&p)?;
            println!("Removed {}", p.display());
        } else {
            println!("No file found to remove at {}", p.display());
        }
    }
    Ok(())
}

#[cfg(not(target_os = "windows"))]
fn uninstall_windows_shortcut_at(_dest: Option<PathBuf>) -> Result<(), Box<dyn std::error::Error>> {
    Err("Windows uninstall is only supported on Windows builds".into())
}

/// Run the standalone installer logic with the provided args slice.
/// This implements the behavior previously in the separate installer binary:
/// - parse `--path`, `--no-build`, `--install`
/// - optionally run `cargo install --path .` (then run the installed binary)
/// - otherwise `cargo build --release` (unless `--no-build`) and create platform app bundle
pub fn run_installer(args: &[String]) -> Result<(), Box<dyn std::error::Error>> {
    // help
    if args.iter().any(|a| a == "--help" || a == "-h") {
        println!("{}", INSTALLER_USAGE);
        return Ok(());
    }

    // parse flags: --path <PATH> | --path=PATH | --no-build | --install
    let mut target_path: Option<PathBuf> = None;
    let mut no_build = false;
    let mut do_install = false;

    let mut i = 0usize;
    while i < args.len() {
        let a = &args[i];
        if a == "--no-build" {
            no_build = true;
        } else if a == "--install" {
            do_install = true;
        } else if a == "--path" {
            if i + 1 >= args.len() {
                eprintln!("--path requires an argument");
                println!("{}", INSTALLER_USAGE);
                return Ok(());
            }
            target_path = Some(PathBuf::from(args[i + 1].clone()));
            i += 1; // skip value
        } else if a.starts_with("--path=") {
            let v = a.splitn(2, '=').nth(1).unwrap_or("");
            target_path = Some(PathBuf::from(v));
        } else {
            eprintln!("Unknown arg: {}", a);
            println!("{}", INSTALLER_USAGE);
            return Ok(());
        }
        i += 1;
    }

    // Basic sanity check: prefer running from crate root where Cargo.toml exists
    if !env::current_dir()?.join("Cargo.toml").exists() {
        eprintln!(
            "Warning: no Cargo.toml found in current directory. Make sure you run this from the crate root."
        );
    }

    // If user requested `cargo install --path .`
    if do_install {
        println!(
            "Running `cargo install --path .` (this performs a release build and installs to cargo bin)"
        );
        let status = Command::new("cargo")
            .args(["install", "--path", ".", "--force"])
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()?;
        if !status.success() {
            return Err("`cargo install` failed".into());
        }

        // Try to find installed binary and create app from it (do not run the installed exe)
        // Try to find installed binary
        let exe = installed_binary_path()?;
        println!("Creating app from installed binary: {}", exe.display());

        // Create platform-appropriate app directly from the installed binary
        create_app_from_exe(&exe, target_path.clone())?;

        println!("Install complete.");
        return Ok(());
    }

    // build release unless user asked to skip
    if !no_build {
        println!("Building release (cargo build --release)...");
        let status = Command::new("cargo")
            .args(["build", "--release"])
            .stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit())
            .status()?;
        if !status.success() {
            return Err("cargo build --release failed".into());
        }
    } else {
        println!("Skipping build as requested (--no-build).");
    }

    // locate release binary
    let mut exe = env::current_dir()?;
    exe.push("target");
    exe.push("release");
    let exe_name = if cfg!(target_os = "windows") {
        "verichord.exe"
    } else {
        "verichord"
    };
    exe.push(exe_name);

    if !exe.exists() {
        eprintln!("Release binary not found at: {}", exe.display());
        eprintln!("Try running without `--no-build` or run `cargo build --release` first.");
        return Err("release binary not found".into());
    }

    println!("Using release binary: {}", exe.display());

    // Create app directly from the built release binary (no need to spawn it)
    create_app_from_exe(&exe, target_path.clone())?;

    println!("Installer finished successfully.");
    Ok(())
}

/// Attempt to locate the installed `verichord` binary after `cargo install`.
fn installed_binary_path() -> Result<PathBuf, Box<dyn std::error::Error>> {
    // Prefer CARGO_HOME/bin if set
    if let Ok(ch) = env::var("CARGO_HOME") {
        let mut p = PathBuf::from(ch);
        p.push("bin");
        let exe_name = if cfg!(target_os = "windows") {
            "verichord.exe"
        } else {
            "verichord"
        };
        p.push(exe_name);
        if p.exists() {
            return Ok(p);
        }
    }

    // Fallback to ~/.cargo/bin
    if let Some(home) = directories::UserDirs::new().and_then(|d| Some(d.home_dir().to_path_buf()))
    {
        let mut p = home;
        p.push(".cargo");
        p.push("bin");
        let exe_name = if cfg!(target_os = "windows") {
            "verichord.exe"
        } else {
            "verichord"
        };
        p.push(exe_name);
        if p.exists() {
            return Ok(p);
        }
    }

    Err("installed binary not found in CARGO_HOME or ~/.cargo/bin".into())
}

#[cfg(target_os = "linux")]
fn uninstall_linux_desktop_entry() -> Result<(), Box<dyn std::error::Error>> {
    let user_dirs = directories::UserDirs::new()
        .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
    let home = user_dirs.home_dir();

    let desktop_path = home
        .join(".local/share/applications")
        .join(format!("{}.desktop", env!("CARGO_PKG_NAME")));
    if desktop_path.exists() {
        fs::remove_file(&desktop_path)?;
        println!("Removed desktop entry: {}", desktop_path.display());
    } else {
        println!("Desktop entry not found: {}", desktop_path.display());
    }

    let icon_path = home
        .join(".local/share/icons/hicolor/256x256/apps")
        .join(format!("{}.png", env!("CARGO_PKG_NAME")));
    if icon_path.exists() {
        fs::remove_file(&icon_path)?;
        println!("Removed icon: {}", icon_path.display());
    } else {
        println!("Icon not found: {}", icon_path.display());
    }

    Ok(())
}

#[cfg(target_os = "windows")]
fn uninstall_windows_shortcut() -> Result<(), Box<dyn std::error::Error>> {
    let user_dirs = directories::UserDirs::new()
        .ok_or_else(|| "failed to determine user directories (UserDirs::new())")?;
    if let Some(desktop) = user_dirs.desktop_dir() {
        let bat_path = desktop.join(format!("{}.bat", display_name_from_package()));
        if bat_path.exists() {
            fs::remove_file(&bat_path)?;
            println!("Removed startup script: {}", bat_path.display());
        } else {
            let exe_path = desktop.join("VeriChord.exe");
            if exe_path.exists() {
                fs::remove_file(&exe_path)?;
                println!("Removed copied executable: {}", exe_path.display());
            } else {
                println!("No Desktop installer files found to remove.");
            }
        }
        Ok(())
    } else {
        Err("no desktop dir".into())
    }
}
