#![cfg_attr(
    all(target_os = "windows", not(debug_assertions)),
    windows_subsystem = "windows"
)]

mod algo_load;
mod chord;
mod metronome;
mod midi;
mod rhythm;
mod ui;

use eframe::egui::viewport::IconData;
use resvg::tiny_skia;
use resvg::usvg;
use std::sync::Arc;

/// Load the embedded SVG (compiled into the binary via `include_bytes!`) and rasterize it
/// to raw RGBA bytes plus dimensions. The SVG bytes are embedded at compile-time; the
/// rasterization happens at runtime, and the resulting pixels are in straight RGBA.
fn load_embedded_icon_rgba() -> Option<(Vec<u8>, u32, u32)> {
    // Embedded at compile-time
    let svg_bytes: &[u8] = include_bytes!("../assets/logo.svg");

    let options = usvg::Options::default();
    // parse SVG tree
    let rtree = usvg::Tree::from_data(svg_bytes, &options).ok()?;

    // target icon size (square). 256 is a reasonable default; change if desired.
    let width: u32 = 256;
    let height: u32 = 256;

    let mut pixmap = tiny_skia::Pixmap::new(width, height)?;
    // compute uniform scale to fit the SVG into the target size while preserving aspect ratio
    let tree_size = rtree.size();
    let scale_x = (width as f32) / tree_size.width();
    let scale_y = (height as f32) / tree_size.height();
    let scale = scale_x.min(scale_y);
    let transform = tiny_skia::Transform::from_scale(scale, scale);
    let mut pixmap_mut = pixmap.as_mut();
    resvg::render(&rtree, transform, &mut pixmap_mut);

    let mut data = pixmap.data().to_vec();
    // tiny-skia produces premultiplied alpha; convert to straight RGBA expected by egui
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

    Some((data, width, height))
}

fn main() {
    let mut options = eframe::NativeOptions::default();

    // Try to set the embedded icon. Failure here is non-fatal; we simply omit the icon.
    if let Some((rgba, w, h)) = load_embedded_icon_rgba() {
        // egui viewport IconData expects straight RGBA
        let icon = IconData {
            rgba,
            width: w,
            height: h,
        };
        options.viewport = options.viewport.with_icon(Arc::new(icon));
    }

    // Set persistence path to executable directory to save window state file locally
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            options.persistence_path = Some(exe_dir.join("verichord_store.ron"));
        }
    }

    eframe::run_native(
        "VeriChord",
        options,
        Box::new(|cc| Ok(Box::new(ui::MidiApp::new(cc)))),
    )
    .unwrap();
}
