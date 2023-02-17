use super::{utils::Png, Error};

use std::num::NonZeroU32;
use std::path::Path;

pub struct Texture {
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    pub sampler: wgpu::Sampler,
}

impl Texture {
    pub fn new<P: AsRef<Path>>(
        path: P,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Result<Texture, Error> {
        let image = super::utils::decode_png(path)?;

        Self::from_png(image, device, queue)
    }

    pub fn from_bytes(
        bytes: &[u8],
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Result<Texture, Error> {
        let image = super::utils::decode_png_bytes(bytes)?;

        Self::from_png(image, device, queue)
    }

    pub fn from_png(
        image: Png,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Result<Texture, Error> {
        let size = wgpu::Extent3d {
            width: image.width,
            height: image.height,
            depth_or_array_layers: 1,
        };

        let texture = device.create_texture(&wgpu::TextureDescriptor {
            size,
            label: None,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[]
        });

        queue.write_texture(
            wgpu::ImageCopyTexture {
                aspect: wgpu::TextureAspect::All, // Depth, Stencil, and Color.
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
            },
            image.data.as_slice(),
            wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: NonZeroU32::new(4 * image.width),
                rows_per_image: NonZeroU32::new(image.height),
            },
            size,
        );

        let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Nearest,
            mipmap_filter: wgpu::FilterMode::Nearest,
            ..Default::default()
        });

        Ok(Self {
            texture,
            view,
            sampler,
        })
    }
}
