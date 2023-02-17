use super::{uniforms, Error};
use std::mem::size_of;

use winit::dpi::PhysicalSize;

pub struct Backend {
    pub size: winit::dpi::PhysicalSize<u32>,

    pub instance: wgpu::Instance,
    pub adapter: wgpu::Adapter,
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,

    pub surface: wgpu::Surface,
    pub surface_cfg: wgpu::SurfaceConfiguration,
    pub pipeline: wgpu::RenderPipeline,
    pub bind_groups: Vec<wgpu::BindGroup>,
    pub vertex_buffers: Vec<wgpu::Buffer>,
    pub index_buffers: Vec<wgpu::Buffer>,
    pub index_count: u32,
}

impl Backend {
    pub async fn new(window: &winit::window::Window) -> Result<Self, Error> {
        let backends = if cfg!(target_os = "windows") || cfg!(target_os = "linux") {
            wgpu::Backends::VULKAN
        } else {
            wgpu::Backends::METAL
        };

        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends,
            dx12_shader_compiler: wgpu::Dx12Compiler::Fxc,
        });

        let size = window.inner_size();
        let surface = unsafe {
            instance
                .create_surface(&window)
                .map_err(|_| Error::SurfaceCreation)?
        };

        let adapter = instance
            .enumerate_adapters(backends)
            .find(|adapter| adapter.is_surface_supported(&surface))
            .ok_or(Error::AdapterCreation)?;

        let device_desc = wgpu::DeviceDescriptor {
            label: Some("rustdump::ui device"),
            features: wgpu::Features::empty(),
            limits: adapter.limits(),
        };

        let (device, queue) = adapter
            .request_device(&device_desc, None)
            .await
            .map_err(Error::GPU)?;

        let surface_capabilities = surface.get_capabilities(&adapter);

        let present_mode = surface_capabilities.present_modes[0];
        let alpha_mode = surface_capabilities.alpha_modes[0];

        let surface_format = {
            let default_format = surface_capabilities.formats[0];

            surface_capabilities
                .formats
                .into_iter()
                .find(|format| format.describe().srgb)
                .unwrap_or(default_format)
        };

        let surface_cfg = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode,
            alpha_mode,
            view_formats: Vec::new(),
        };

        surface.configure(&device, &surface_cfg);

        let texture = super::texture::Texture::new("./assets/joe_biden.png", &device, &queue)?;

        let texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: Some("rustdump::ui texture bind group layout"),
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D2,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
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

        let texture_bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("rustdump::ui texture bind group"),
            layout: &texture_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&texture.view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&texture.sampler),
                },
            ],
        });

        let (vertices, indices) = uniforms::create_vertices();

        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("rustdump::ui vertex buffer"),
            size: (size_of::<uniforms::Vertex>() * vertices.len()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        queue.write_buffer(&vertex_buffer, 0, bytemuck::cast_slice(&vertices[..]));

        let index_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("rustdump::ui indices buffer"),
            size: (size_of::<u16>() * indices.len()) as u64,
            usage: wgpu::BufferUsages::INDEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        queue.write_buffer(&index_buffer, 0, bytemuck::cast_slice(&indices[..]));

        // TODO make module compiling run in parallel, not concurrently
        let now = std::time::Instant::now();
        let (vert_module, frag_module) = tokio::try_join!(
            super::utils::generate_vulkan_shader_module(
                "./shaders/vert.glsl",
                wgpu::ShaderStages::VERTEX,
                &device,
            ),
            super::utils::generate_vulkan_shader_module(
                "./shaders/frag.glsl",
                wgpu::ShaderStages::FRAGMENT,
                &device,
            )
        )?;

        println!("took {:#?} to generate shaders", now.elapsed());

        let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("rustdump::ui pipeline layout"),
            bind_group_layouts: &[&texture_bind_group_layout],
            push_constant_ranges: &[],
        });

        // Create the render pipelines. These describe how the data will flow through the GPU, and
        // what constraints and modifiers it will have.
        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Primary pipeline"),
            layout: Some(&pipeline_layout),
            multiview: None,
            vertex: wgpu::VertexState {
                module: &vert_module,
                entry_point: "main",
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: size_of::<uniforms::Vertex>() as wgpu::BufferAddress,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &wgpu::vertex_attr_array![0 => Float32x3, 1 => Float32x2],
                }],
            },
            fragment: Some(wgpu::FragmentState {
                module: &frag_module,
                entry_point: "main",
                targets: &[Some(wgpu::ColorTargetState {
                    format: surface_format,
                    blend: Some(wgpu::BlendState {
                        color: wgpu::BlendComponent::REPLACE,
                        alpha: wgpu::BlendComponent::REPLACE,
                    }),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            // How the triangles will be rasterized
            primitive: wgpu::PrimitiveState {
                // type of data we are passing in
                topology: wgpu::PrimitiveTopology::TriangleList,
                front_face: wgpu::FrontFace::Cw,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
                // The number of samples for multisampling
                count: 1,
                // a mask for what samples are active: !0 means all of them
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
        });

        Ok(Self {
            size,
            instance,
            adapter,
            device,
            queue,
            surface,
            surface_cfg,
            bind_groups: vec![texture_bind_group],
            pipeline,
            vertex_buffers: vec![vertex_buffer],
            index_buffers: vec![index_buffer],
            index_count: indices.len() as u32,
        })
    }

    fn redraw_frame(&mut self, frame: wgpu::SurfaceTexture) {
        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let encoder_desc = wgpu::CommandEncoderDescriptor {
            label: Some("rustdump::ui encoder"),
        };

        let mut encoder = self.device.create_command_encoder(&encoder_desc);

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color {
                        r: 5.0 / 255.0,
                        g: 5.0 / 255.0,
                        b: 5.0 / 255.0,
                        a: 1.0,
                    }),
                    store: true,
                },
            })],
            depth_stencil_attachment: None,
        });

        render_pass.set_pipeline(&self.pipeline);
        render_pass.set_bind_group(0, &self.bind_groups[0], &[]);
        render_pass.set_vertex_buffer(0, self.vertex_buffers[0].slice(..));
        render_pass.set_index_buffer(self.index_buffers[0].slice(..), wgpu::IndexFormat::Uint16);
        render_pass.draw_indexed(0..self.index_count, 0, 0..1);

        // Required because render_pass and queue takes a &T.
        drop(render_pass);

        self.queue.submit(Some(encoder.finish()));

        // Schedule texture to be renderer on surface.
        frame.present();
    }

    pub fn redraw(&mut self) {
        match self.surface.get_current_texture() {
            Ok(frame) => self.redraw_frame(frame),
            Err(info) => eprintln!("{:?}", Error::DRAW(info)),
        }
    }

    pub fn resize(&mut self, size: PhysicalSize<u32>) {
        if size.width > 0 && size.height > 0 {
            self.size = size;
            self.surface_cfg.width = std::cmp::max(size.width, super::MIN_REAL_SIZE.width);
            self.surface_cfg.height = std::cmp::max(size.height, super::MIN_REAL_SIZE.height);
            self.surface.configure(&self.device, &self.surface_cfg);
        }
    }
}
