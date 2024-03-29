//! You need to create a [`Pipeline`] and feed it with the output data provided by egui.

use super::ScreenDescriptor;
use crate::Error;

use std::borrow::Cow;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;

use wgpu::util::DeviceExt;

pub const CLEAR_COLOR: wgpu::Color = wgpu::Color {
    r: 10.0 / 255.0,
    g: 10.0 / 255.0,
    b: 10.0 / 255.0,
    a: 1.0,
};

/// Enum for selecting the right buffer type.
#[derive(Debug)]
enum BufferType {
    Uniform,
    Index,
    Vertex,
}

/// Uniform buffer used when rendering.
#[derive(Clone, Copy, Debug)]
#[repr(C, align(16))]
struct UniformBuffer {
    screen_size: [f32; 2],
}

unsafe impl bytemuck::Pod for UniformBuffer {}
unsafe impl bytemuck::Zeroable for UniformBuffer {}

/// RenderPass to render a egui based GUI.
pub struct Pipeline {
    pipeline: wgpu::RenderPipeline,
    index_buffers: Vec<wgpu::Buffer>,
    vertex_buffers: Vec<wgpu::Buffer>,
    uniform_buffer: wgpu::Buffer,
    uniform_bind_group: wgpu::BindGroup,
    texture_bind_group_layout: wgpu::BindGroupLayout,

    /// Map of egui texture IDs to textures and their associated bindgroups (texture view +
    /// sampler). The texture may be None if the TextureId is just a handle to a user-provided
    /// sampler.
    textures: BTreeMap<egui::TextureId, (Option<wgpu::Texture>, wgpu::BindGroup)>,
}

impl Pipeline {
    /// Creates a new render pass to render a egui UI.
    ///
    /// If the format passed is not a *Srgb format, the shader will automatically convert to sRGB
    /// colors in the shader.
    pub fn new(instance: &super::Instance, msaa_samples: u32) -> Self {
        let (device, output_format) = (&instance.device, instance.surface_cfg.format);

        let shader = wgpu::ShaderModuleDescriptor {
            label: Some("bite::gui shader"),
            source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(include_str!("./egui.wgsl"))),
        };
        let module = device.create_shader_module(shader);

        let uniform_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("bite::gui uniform buffer"),
            contents: bytemuck::cast_slice(&[UniformBuffer {
                screen_size: [0.0, 0.0],
            }]),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });
        let uniform_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("bite::gui uniform buffer layout"),
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX,
                    ty: wgpu::BindingType::Buffer {
                        has_dynamic_offset: false,
                        min_binding_size: std::num::NonZeroU64::new(
                            std::mem::size_of::<UniformBuffer>() as u64,
                        ),
                        ty: wgpu::BufferBindingType::Uniform,
                    },
                    count: None,
                }],
            });

        let uniform_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("bite::gui uniform buffer"),
            layout: &uniform_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: &uniform_buffer,
                    offset: 0,
                    size: std::num::NonZeroU64::new(std::mem::size_of::<UniformBuffer>() as u64),
                }),
            }],
        });

        let texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("bite::gui texture bind group layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
            });

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("bite::gui pipeline layout"),
            bind_group_layouts: &[&uniform_bind_group_layout, &texture_bind_group_layout],
            push_constant_ranges: &[],
        });

        let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("bite::gui pipeline"),
            layout: Some(&pipeline_layout),
            vertex: wgpu::VertexState {
                entry_point: "vs_main",
                module: &module,
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: 5 * 4,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    // 0: vec2 position
                    // 1: vec2 texture coordinates
                    // 2: uint color
                    attributes: &wgpu::vertex_attr_array![
                        0 => Float32x2,
                        1 => Float32x2,
                        2 => Uint32
                    ],
                }],
            },
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                unclipped_depth: false,
                conservative: false,
                cull_mode: None,
                front_face: wgpu::FrontFace::default(),
                polygon_mode: wgpu::PolygonMode::default(),
                strip_index_format: None,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
                alpha_to_coverage_enabled: false,
                count: msaa_samples,
                mask: !0,
            },

            fragment: Some(wgpu::FragmentState {
                module: &module,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: output_format,
                    blend: Some(wgpu::BlendState {
                        color: wgpu::BlendComponent {
                            src_factor: wgpu::BlendFactor::One,
                            dst_factor: wgpu::BlendFactor::OneMinusSrcAlpha,
                            operation: wgpu::BlendOperation::Add,
                        },
                        alpha: wgpu::BlendComponent {
                            src_factor: wgpu::BlendFactor::OneMinusDstAlpha,
                            dst_factor: wgpu::BlendFactor::One,
                            operation: wgpu::BlendOperation::Add,
                        },
                    }),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview: None,
        });

        Self {
            pipeline: render_pipeline,
            vertex_buffers: Vec::with_capacity(64),
            index_buffers: Vec::with_capacity(64),
            uniform_buffer,
            uniform_bind_group,
            texture_bind_group_layout,
            textures: BTreeMap::new(),
        }
    }

    /// Executes the egui render pass. When `clear_color` is not None, the output target will get
    /// cleared with clear_color before writing to it.
    pub fn execute(
        &self,
        encoder: &mut wgpu::CommandEncoder,
        color_attachment: &wgpu::TextureView,
        paint_jobs: &[egui::epaint::ClippedPrimitive],
        screen_descriptor: &ScreenDescriptor,
    ) -> Result<(), Error> {
        let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: color_attachment,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(CLEAR_COLOR),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
            label: Some("bite::gui render pass"),
        });

        rpass.push_debug_group("bite::egui render pass");
        self.execute_with_renderpass(&mut rpass, paint_jobs, screen_descriptor)?;
        rpass.pop_debug_group();

        Ok(())
    }

    /// Executes the egui render pass onto an existing wgpu renderpass.
    pub fn execute_with_renderpass<'rpass>(
        &'rpass self,
        rpass: &mut wgpu::RenderPass<'rpass>,
        paint_jobs: &[egui::epaint::ClippedPrimitive],
        screen_descriptor: &ScreenDescriptor,
    ) -> Result<(), Error> {
        rpass.set_pipeline(&self.pipeline);

        rpass.set_bind_group(0, &self.uniform_bind_group, &[]);

        let scale_factor = screen_descriptor.scale_factor;
        let physical_width = screen_descriptor.physical_width;
        let physical_height = screen_descriptor.physical_height;

        for ((job, vertex_buffer), index_buffer) in
            paint_jobs.iter().zip(self.vertex_buffers.iter()).zip(self.index_buffers.iter())
        {
            // Transform clip rect to physical pixels.
            let clip_min_x = scale_factor * job.clip_rect.min.x;
            let clip_min_y = scale_factor * job.clip_rect.min.y;
            let clip_max_x = scale_factor * job.clip_rect.max.x;
            let clip_max_y = scale_factor * job.clip_rect.max.y;

            // Make sure clip rect can fit within an `u32`.
            let clip_min_x = clip_min_x.clamp(0.0, physical_width as f32);
            let clip_min_y = clip_min_y.clamp(0.0, physical_height as f32);
            let clip_max_x = clip_max_x.clamp(clip_min_x, physical_width as f32);
            let clip_max_y = clip_max_y.clamp(clip_min_y, physical_height as f32);

            let clip_min_x = clip_min_x.round() as u32;
            let clip_min_y = clip_min_y.round() as u32;
            let clip_max_x = clip_max_x.round() as u32;
            let clip_max_y = clip_max_y.round() as u32;

            let width = (clip_max_x - clip_min_x).max(1);
            let height = (clip_max_y - clip_min_y).max(1);

            // Clip scissor rectangle to target size.
            let x = clip_min_x.min(physical_width);
            let y = clip_min_y.min(physical_height);
            let width = width.min(physical_width - x);
            let height = height.min(physical_height - y);

            // Skip rendering with zero-sized clip areas.
            if width == 0 || height == 0 {
                continue;
            }

            rpass.set_scissor_rect(x, y, width, height);

            if let egui::epaint::Primitive::Mesh(ref mesh) = job.primitive {
                let bind_group = self.get_texture_bind_group(mesh.texture_id)?;
                rpass.set_bind_group(1, bind_group, &[]);

                rpass.set_index_buffer(index_buffer.slice(..), wgpu::IndexFormat::Uint32);
                rpass.set_vertex_buffer(0, vertex_buffer.slice(..));
                rpass.draw_indexed(0..mesh.indices.len() as u32, 0, 0..1);
            }
        }

        Ok(())
    }

    fn get_texture_bind_group(
        &self,
        texture_id: egui::TextureId,
    ) -> Result<&wgpu::BindGroup, Error> {
        self.textures
            .get(&texture_id)
            .ok_or(Error::InvalidTextureId(texture_id))
            .map(|x| &x.1)
    }

    /// Updates the texture used by egui for the fonts etc. Should be called before `execute()`.
    pub fn add_textures(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        textures: &egui::TexturesDelta,
    ) -> Result<(), Error> {
        for (texture_id, image_delta) in textures.set.iter() {
            let image_size = image_delta.image.size();

            let origin = match image_delta.pos {
                Some([x, y]) => wgpu::Origin3d {
                    x: x as u32,
                    y: y as u32,
                    z: 0,
                },
                None => wgpu::Origin3d::ZERO,
            };

            let alpha_srgb_pixels: Option<Vec<_>> = match &image_delta.image {
                egui::ImageData::Color(_) => None,
                egui::ImageData::Font(a) => Some(a.srgba_pixels(Some(1.0)).collect()),
            };

            let image_data: &[u8] = match &image_delta.image {
                egui::ImageData::Color(c) => bytemuck::cast_slice(c.pixels.as_slice()),
                egui::ImageData::Font(_) => {
                    // The unwrap here should never fail as alpha_srgb_pixels will have been set to
                    // `Some` above.
                    bytemuck::cast_slice(
                        alpha_srgb_pixels
                            .as_ref()
                            .expect("Alpha texture should have been converted already")
                            .as_slice(),
                    )
                }
            };

            let image_size = wgpu::Extent3d {
                width: image_size[0] as u32,
                height: image_size[1] as u32,
                depth_or_array_layers: 1,
            };

            let image_data_layout = wgpu::ImageDataLayout {
                offset: 0,
                bytes_per_row: Some(4 * image_size.width),
                rows_per_image: None,
            };

            match self.textures.entry(*texture_id) {
                Entry::Occupied(mut o) => match image_delta.pos {
                    None => {
                        let (texture, bind_group) = create_texture_and_bind_group(
                            device,
                            queue,
                            origin,
                            image_data,
                            image_data_layout,
                            image_size,
                            &self.texture_bind_group_layout,
                        );

                        let (texture, _) = o.insert((Some(texture), bind_group));

                        if let Some(texture) = texture {
                            texture.destroy();
                        }
                    }
                    Some(_) => {
                        if let Some(texture) = o.get().0.as_ref() {
                            queue.write_texture(
                                wgpu::ImageCopyTexture {
                                    texture,
                                    mip_level: 0,
                                    origin,
                                    aspect: wgpu::TextureAspect::All,
                                },
                                image_data,
                                image_data_layout,
                                image_size,
                            );
                        } else {
                            return Err(Error::InvalidTextureId(*texture_id));
                        }
                    }
                },
                Entry::Vacant(v) => {
                    let (texture, bind_group) = create_texture_and_bind_group(
                        device,
                        queue,
                        origin,
                        image_data,
                        image_data_layout,
                        image_size,
                        &self.texture_bind_group_layout,
                    );

                    v.insert((Some(texture), bind_group));
                }
            }
        }

        Ok(())
    }

    /// Remove the textures egui no longer needs. Should be called after `execute()`
    pub fn remove_textures(&mut self, textures: egui::TexturesDelta) -> Result<(), Error> {
        for texture_id in textures.free {
            let (texture, _binding) = self.textures.remove(&texture_id).ok_or({
                // This can happen due to a bug in egui, or if the user doesn't call `add_textures`
                // when required.
                Error::InvalidTextureId(texture_id)
            })?;

            if let Some(texture) = texture {
                texture.destroy();
            }
        }

        Ok(())
    }

    /// Uploads the uniform, vertex and index data used by the render pass.
    /// Should be called before `execute()`.
    pub fn update_buffers(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        paint_jobs: &[egui::epaint::ClippedPrimitive],
        screen_descriptor: &ScreenDescriptor,
    ) {
        let index_size = self.index_buffers.len();
        let vertex_size = self.vertex_buffers.len();

        let (logical_width, logical_height) = screen_descriptor.logical_size();

        self.update_buffer(
            device,
            queue,
            BufferType::Uniform,
            0,
            bytemuck::cast_slice(&[UniformBuffer {
                screen_size: [logical_width as f32, logical_height as f32],
            }]),
        );

        for (idx, egui::ClippedPrimitive { primitive, .. }) in paint_jobs.iter().enumerate() {
            let mesh = match primitive {
                egui::epaint::Primitive::Mesh(mesh) => mesh,
                egui::epaint::Primitive::Callback(_) => continue,
            };

            let data: &[u8] = bytemuck::cast_slice(&mesh.indices);
            if idx < index_size {
                self.update_buffer(device, queue, BufferType::Index, idx, data)
            } else {
                let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("bite::gui uniform buffer"),
                    contents: data,
                    usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
                });
                self.index_buffers.push(buffer);
            }

            let data: &[u8] = bytemuck::cast_slice(&mesh.vertices);
            if idx < vertex_size {
                self.update_buffer(device, queue, BufferType::Vertex, idx, data)
            } else {
                let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("bite::gui vertex buffer"),
                    contents: data,
                    usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                });

                self.vertex_buffers.push(buffer);
            }
        }
    }

    /// Updates the buffers used by egui. Will properly re-size the buffers if needed.
    fn update_buffer(
        &mut self,
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        buffer_type: BufferType,
        index: usize,
        data: &[u8],
    ) {
        let (buffer, storage, ident) = match buffer_type {
            BufferType::Index => (
                &mut self.index_buffers[index],
                wgpu::BufferUsages::INDEX,
                "bite::gui index buffer",
            ),
            BufferType::Vertex => (
                &mut self.vertex_buffers[index],
                wgpu::BufferUsages::VERTEX,
                "bite::gui vertex buffer",
            ),
            BufferType::Uniform => (
                &mut self.uniform_buffer,
                wgpu::BufferUsages::UNIFORM,
                "bite::gui uniform buffer",
            ),
        };

        if data.len() > buffer.size() as usize {
            *buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some(ident),
                contents: bytemuck::cast_slice(data),
                usage: storage | wgpu::BufferUsages::COPY_DST,
            });
        } else {
            queue.write_buffer(&buffer, 0, data);
        }
    }
}

/// Create a texture and bind group from existing data
#[allow(clippy::too_many_arguments)]
fn create_texture_and_bind_group(
    device: &wgpu::Device,
    queue: &wgpu::Queue,
    origin: wgpu::Origin3d,
    image_data: &[u8],
    image_data_layout: wgpu::ImageDataLayout,
    image_size: wgpu::Extent3d,
    texture_bind_group_layout: &wgpu::BindGroupLayout,
) -> (wgpu::Texture, wgpu::BindGroup) {
    let texture = device.create_texture(&wgpu::TextureDescriptor {
        label: Some("bite::gui texture buffer"),
        size: image_size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: wgpu::TextureFormat::Rgba8UnormSrgb,
        usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
        view_formats: &[],
    });

    queue.write_texture(
        wgpu::ImageCopyTexture {
            texture: &texture,
            mip_level: 0,
            origin,
            aspect: wgpu::TextureAspect::All,
        },
        image_data,
        image_data_layout,
        image_size,
    );

    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some("bite::gui sampler"),
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });

    let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
        label: Some("bite::gui texture bind group"),
        layout: texture_bind_group_layout,
        entries: &[
            wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(
                    &texture.create_view(&wgpu::TextureViewDescriptor::default()),
                ),
            },
            wgpu::BindGroupEntry {
                binding: 1,
                resource: wgpu::BindingResource::Sampler(&sampler),
            },
        ],
    });

    (texture, bind_group)
}
