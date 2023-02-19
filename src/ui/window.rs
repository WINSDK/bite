use super::{uniforms, Error};
use std::mem::size_of;

use wgpu_glyph::{GlyphBrush, GlyphBrushBuilder};
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

    pub staging_belt: wgpu::util::StagingBelt,

    pub glyph_brush: GlyphBrush<()>,
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
                .map_err(Error::SurfaceCreation)?
        };

        let adapter = instance
            .enumerate_adapters(backends)
            .find(|adapter| adapter.is_surface_supported(&surface))
            .ok_or(Error::AdapterRequest)?;

        let device_desc = wgpu::DeviceDescriptor {
            label: Some("bite::ui device"),
            features: wgpu::Features::empty(),
            limits: adapter.limits(),
        };

        let (device, queue) = adapter
            .request_device(&device_desc, None)
            .await
            .map_err(Error::DeviceRequest)?;

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
                label: Some("bite::ui texture bind group layout"),
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
            label: Some("bite::ui texture bind group"),
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
            label: Some("bite::ui vertex buffer"),
            size: (size_of::<uniforms::Vertex>() * vertices.len()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        queue.write_buffer(&vertex_buffer, 0, bytemuck::cast_slice(&vertices[..]));

        let index_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("bite::ui indices buffer"),
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
            label: Some("bite::ui pipeline layout"),
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

        let staging_belt = wgpu::util::StagingBelt::new(1024);

        let font = include_bytes!("../../assets/LigaSFMonoNerdFont-Regular.otf");
        let font = ab_glyph::FontArc::try_from_slice(font).unwrap();
        let glyph_brush = GlyphBrushBuilder::using_font(font).build(&device, surface_format);

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
            staging_belt,
            glyph_brush,
        })
    }

    pub fn redraw(&mut self, fps: usize) -> Result<(), Error> {
        let frame = self
            .surface
            .get_current_texture()
            .map_err(Error::DrawTexture)?;

        let view = frame
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("bite::ui encoder"),
            });

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

        // required drop because render_pass and queue take a &wgpu::Device
        drop(render_pass);

        self.glyph_brush.queue(wgpu_glyph::Section {
            screen_position: (10.0, 10.0),
            bounds: (self.size.width as f32, self.size.height as f32),
            text: vec![wgpu_glyph::Text::new(&format!("FPS: {fps}"))
                .with_color([1.0, 1.0, 1.0, 1.0])
                .with_scale(40.0)],
            ..wgpu_glyph::Section::default()
        });

        // draw text
        self.glyph_brush
            .draw_queued(
                &self.device,
                &mut self.staging_belt,
                &mut encoder,
                &view,
                self.size.width,
                self.size.height,
            )
            .map_err(Error::DrawText)?;

        // draw clipped text
        // self.glyph_brush
        //     .draw_queued_with_transform_and_scissoring(
        //         &self.device,
        //         &mut self.staging_belt,
        //         &mut encoder,
        //         &view,
        //         wgpu_glyph::orthographic_projection(self.size.width, self.size.height),
        //         wgpu_glyph::Region {
        //             x: 40,
        //             y: 105,
        //             width: 200,
        //             height: 15,
        //         },
        //     )
        //     .map_err(Error::DrawText)?;

        // submit work
        self.staging_belt.finish();
        self.queue.submit(Some(encoder.finish()));

        // schedule texture to be renderer on surface
        frame.present();

        // recall unused staging buffers
        self.staging_belt.recall();

        Ok(())
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
