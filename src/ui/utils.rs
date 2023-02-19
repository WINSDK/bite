use super::Error;
use std::borrow::Cow;
use std::path::{Path, PathBuf};

use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use naga::{
    back::spv,
    front::glsl,
    valid::{Capabilities, ValidationFlags, Validator},
};

pub struct Png {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
}

pub fn decode_png<P: AsRef<Path>>(path: P) -> Result<Png, Error> {
    let bytes = std::fs::read(&path).map_err(|_| Error::NotFound(path.as_ref().to_owned()))?;

    decode_png_bytes(&bytes)
}

pub fn decode_png_bytes(bytes: &[u8]) -> Result<Png, Error> {
    let mut decoder = png::Decoder::new(bytes);
    decoder.set_transformations(png::Transformations::STRIP_16 | png::Transformations::EXPAND);

    let mut reader = decoder.read_info().map_err(|_| Error::PngDecode)?;
    let mut data = vec![0; reader.output_buffer_size()];
    let info = reader.next_frame(&mut data).map_err(|_| Error::PngDecode)?;

    if info.width == 0 || info.height == 0 {
        return Err(Error::PngDecode);
    }

    if info.color_type != png::ColorType::Rgba {
        return Err(Error::PngFormat);
    }

    Ok(Png {
        data,
        width: info.width,
        height: info.height,
    })
}

pub async fn generate_vulkan_shader_module<P: AsRef<Path>>(
    path: P,
    stage: wgpu::ShaderStages,
    device: &wgpu::Device,
) -> Result<wgpu::ShaderModule, Error> {
    let cache_path = cached_path(&path);

    match retrieve_cached_module(&path, cache_path, device).await {
        None => compile_shader(&path, stage, device).await,
        Some(module) => Ok(module),
    }
}

fn cached_path<P: AsRef<Path>>(path: P) -> PathBuf {
    let cache_path = path.as_ref().with_extension("spv");
    let cache_path = cache_path.file_name().unwrap();
    if cfg!(target_os = "windows") {
        Path::new(&std::env::var("TMP").unwrap()).join(cache_path)
    } else {
        Path::new("/tmp").join(cache_path)
    }
}

/// checks if shader is already cached, if so returns a ShaderModule
async fn retrieve_cached_module<P1: AsRef<Path>, P2: AsRef<Path>>(
    path: P1,
    cache_path: P2,
    device: &wgpu::Device,
) -> Option<wgpu::ShaderModule> {
    let src_file = File::open(&path).await.ok()?;
    let mut cache_file = File::open(cache_path).await.ok()?;

    let cache_modified = cache_file.read_u128().await.ok()?;
    let date_modified = src_file
        .metadata()
        .await
        .ok()?
        .modified()
        .ok()?
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_millis();

    // Check if the src_file's modified date equals the modified date stored in the cache file,
    // this ensures that if the source file get's modified, the cache file must be outdated.
    if date_modified == cache_modified {
        let mut shader: Vec<u8> = Vec::new();
        cache_file.read_to_end(&mut shader).await.ok()?;

        return Some(device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::util::make_spirv(&shader[..]),
        }));
    }

    None
}

async fn compile_shader<P: AsRef<Path>>(
    path: P,
    stage: wgpu::ShaderStages,
    device: &wgpu::Device,
) -> Result<wgpu::ShaderModule, Error> {
    let mut src_file = File::open(&path)
        .await
        .map_err(|_| Error::NotFound(path.as_ref().to_owned()))?;

    let cache_path = cached_path(&path);
    let mut cache_file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(&cache_path)
        .await
        .map_err(|_| Error::NotFound(cache_path))?;

    let stage = match stage {
        wgpu::ShaderStages::COMPUTE => naga::ShaderStage::Compute,
        wgpu::ShaderStages::VERTEX => naga::ShaderStage::Vertex,
        wgpu::ShaderStages::FRAGMENT => naga::ShaderStage::Fragment,
        _ => return Err(Error::UnknownShaderStage),
    };

    let module = {
        let mut src = String::new();
        src_file.read_to_string(&mut src).await.map_err(Error::IO)?;

        glsl::Parser::default()
            .parse(&glsl::Options::from(stage), &src[..])
            .map_err(|_| Error::CompilationFailed)?
    };

    let mut validator = if cfg!(debug_assertions) {
        Validator::new(ValidationFlags::all(), Capabilities::empty())
    } else {
        Validator::new(ValidationFlags::empty(), Capabilities::empty())
    };

    let module_info = validator
        .validate(&module)
        .map_err(|_| Error::CompilationFailed)?;

    let binary = spv::write_vec(&module, &module_info, &spv::Options::default(), None).unwrap();

    // As different OS's use different underlying measurements for time, we can't just cast this to
    // a byte array and compare time differences. For this reason we converts the date modified to
    // a UNIX timestamp.
    let date_modified = src_file
        .metadata()
        .await
        .map_err(Error::IO)?
        .modified()
        .map_err(Error::IO)?
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_millis();

    cache_file
        .write_u128(date_modified)
        .await
        .map_err(Error::IO)?;

    cache_file
        .write_all(bytemuck::cast_slice(binary.as_slice()))
        .await
        .map_err(Error::IO)?;

    Ok(device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: None,
        source: wgpu::ShaderSource::SpirV(Cow::Borrowed(&binary[..])),
    }))
}

#[cfg(not(target_os = "windows"))]
pub fn generate_window(
    title: &str,
    icon: Option<winit::window::Icon>,
    event_loop: &winit::event_loop::EventLoop<()>,
) -> Result<winit::window::Window, Error> {
    winit::window::WindowBuilder::new()
        .with_title(title)
        .with_decorations(false)
        .with_theme(Some(winit::window::Theme::Dark))
        .with_window_icon(icon)
        .with_min_inner_size(super::MIN_WIN_SIZE)
        .build(event_loop)
        .map_err(|_| Error::WindowCreation)
}

#[cfg(target_os = "windows")]
pub fn generate_window(
    title: &str,
    icon: Option<winit::window::Icon>,
    event_loop: &winit::event_loop::EventLoop<()>,
) -> Result<winit::window::Window, Error> {
    use winit::platform::windows::WindowBuilderExtWindows;

    winit::window::WindowBuilder::new()
        .with_title(title)
        .with_decorations(false)
        .with_theme(Some(winit::window::Theme::Dark))
        .with_transparent(true)
        .with_drag_and_drop(true)
        .with_taskbar_icon(icon.clone())
        .with_window_icon(icon)
        .with_min_inner_size(super::MIN_WIN_SIZE)
        .build(event_loop)
        .map_err(|_| Error::WindowCreation)
}
