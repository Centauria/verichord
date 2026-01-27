use std::env;
use std::fs::{self, File};
use std::io::{BufWriter, Cursor};
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("cargo:rerun-if-changed=assets/logo.svg");

    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")?);
    let svg_path = manifest_dir.join("assets").join("logo.svg");
    let svg_data = std::fs::read(&svg_path)
        .map_err(|e| format!("failed to read {}: {}", svg_path.display(), e))?;

    // Parse SVG
    let options = usvg::Options::default();
    let rtree = usvg::Tree::from_data(&svg_data, &options)
        .map_err(|e| format!("failed to parse svg: {}", e))?;

    // sizes to generate
    let sizes = [16u32, 32, 48, 64, 128, 256, 512];

    // output dir
    let out_dir = PathBuf::from(env::var("OUT_DIR")?);
    let icons_dir = out_dir.join("generated_icons");
    fs::create_dir_all(&icons_dir)?;

    // store png buffers
    let mut png_images: Vec<(u32, Vec<u8>)> = Vec::new();

    for &size in &sizes {
        let width = size;
        let height = size;
        let mut pixmap = tiny_skia::Pixmap::new(width, height)
            .ok_or_else(|| format!("failed to create pixmap {}x{}", width, height))?;

        let tree_size = rtree.size();
        let scale_x = (width as f32) / tree_size.width();
        let scale_y = (height as f32) / tree_size.height();
        let scale = scale_x.min(scale_y);
        let transform = tiny_skia::Transform::from_scale(scale, scale);
        let mut pixmap_mut = pixmap.as_mut();
        resvg::render(&rtree, transform, &mut pixmap_mut);

        // tiny-skia yields premultiplied alpha, convert to straight RGBA
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

        // encode PNG
        let mut png_buf: Vec<u8> = Vec::new();
        {
            let mut encoder = png::Encoder::new(&mut png_buf, width, height);
            encoder.set_color(png::ColorType::Rgba);
            encoder.set_depth(png::BitDepth::Eight);
            let mut writer = encoder.write_header()?;
            writer.write_image_data(&data)?;
        }

        // save individual png for packaging convenience
        let png_path = icons_dir.join(format!("icon_{}x{}.png", size, size));
        fs::write(&png_path, &png_buf)?;
        png_images.push((size, png_buf));
    }

    // Create ICO (Windows)
    let ico_path = icons_dir.join("logo.ico");
    {
        let file = File::create(&ico_path)?;
        let mut icon_dir = ico::IconDir::new(ico::ResourceType::Icon);
        for (_size, png_bytes) in &png_images {
            let image = ico::IconImage::read_png(Cursor::new(png_bytes))?;
            icon_dir.add_entry(ico::IconDirEntry::encode(&image)?);
        }
        icon_dir.write(file)?;
    }

    // Create ICNS (macOS)
    let icns_path = icons_dir.join("logo.icns");
    {
        let file = File::create(&icns_path)?;
        let mut icon_family = icns::IconFamily::new();
        for (_size, png_bytes) in &png_images {
            let img = icns::Image::read_png(Cursor::new(png_bytes))?;
            icon_family.add_icon(&img).ok();
        }
        let mut writer = BufWriter::new(file);
        icon_family.write(&mut writer)?;
    }

    // Windows resource embedding
    if let Ok(target) = env::var("TARGET") {
        if target.contains("windows") {
            println!("Embedding generated icon into Windows executable (using winres)");
            let mut res = winres::WindowsResource::new();
            res.set_icon(ico_path.to_str().unwrap());
            res.compile()?;
            // winres emits "cargo:rustc-link-lib=dylib=resource" which might be stripped by the linker
            // if no symbols are used (which is true for a resource-only lib).
            // Explicitly linking the file forces it to be included.
            // We applied it previously to the main 'verichord' binary but it caused duplicate VERSION
            // resources on MSVC when winres already embedded the resource. Remove the explicit link
            // and rely on `res.compile()` to supply the resource.
            // println!(
            //     "cargo:rustc-link-arg-bin=verichord={}",
            //     out_dir.join("resource.lib").display()
            // );
        }
    }

    // Instruct packagers / show paths
    println!("Generated icons are in: {}", icons_dir.display());
    println!("Windows .ico: {}", ico_path.display());
    println!("macOS .icns: {}", icns_path.display());
    println!(
        "PNG icons available for Linux packaging (AppImage/.desktop): {}",
        icons_dir.display()
    );

    Ok(())
}
