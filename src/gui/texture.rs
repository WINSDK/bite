#![allow(dead_code)]

use crate::gui::utils::{decode_png, decode_png_bytes, Png};
use crate::gui::Error;

use once_cell::sync::OnceCell;
use std::path::Path;
use wgpu::util::DeviceExt;

static BIND_GROUP_LAYOUT: OnceCell<wgpu::BindGroupLayout> = OnceCell::new();

pub struct Texture {
    pub texture: wgpu::Texture,
    pub view: wgpu::TextureView,
    pub sampler: wgpu::Sampler,
    pub bind_group: wgpu::BindGroup,
}

impl Texture {
    pub async fn new<P: AsRef<Path>>(
        path: P,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Result<Texture, Error> {
        let image = decode_png(path)?;

        Self::from_png(image, device, queue)
    }

    pub fn set_layout(device: &wgpu::Device, desc: &wgpu::BindGroupLayoutDescriptor) {
        BIND_GROUP_LAYOUT.set(device.create_bind_group_layout(desc)).unwrap();
    }

    pub fn layout() -> &'static wgpu::BindGroupLayout {
        BIND_GROUP_LAYOUT.get().unwrap()
    }

    pub async fn from_bytes(
        bytes: &[u8],
        device: &wgpu::Device,
        queue: &wgpu::Queue,
    ) -> Result<Texture, Error> {
        let image = decode_png_bytes(bytes)?;

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

        let texture_desc = wgpu::TextureDescriptor {
            size,
            label: None,
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: wgpu::TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        };

        let texture = device.create_texture_with_data(queue, &texture_desc, &image.data[..]);

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

        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("bite::ui texture bind group"),
            layout: Self::layout(),
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        Ok(Self {
            texture,
            view,
            sampler,
            bind_group,
        })
    }
}
