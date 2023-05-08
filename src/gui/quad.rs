use std::mem;

use bytemuck::{Pod, Zeroable};
use tokenizing::Color;
use wgpu::util::DeviceExt;

#[repr(C)]
#[derive(Copy, Clone, Pod, Zeroable)]
pub struct Instance {
    pub position: [f32; 2],
    pub size: [f32; 2],
    pub color: Color,
}

const MAX_INSTANCES: u64 = 10;

const VERTICES: &[glam::Vec3] = &[
    glam::vec3(0.0, 0.0, 0.0),
    glam::vec3(0.0, -1.0, 0.0),
    glam::vec3(1.0, -1.0, 0.0),
    glam::vec3(1.0, 0.0, 0.0),
];

const INDICES: &[u16] = &[0, 1, 2, 0, 2, 3];

#[repr(C)]
#[derive(Clone, Copy, Zeroable, Pod)]
struct Uniforms {
    window_size: [f32; 2],
}

impl Uniforms {
    fn new(window_size: [f32; 2]) -> Self {
        Uniforms { window_size }
    }
}

pub struct Pipeline {
    constant_buffer: wgpu::Buffer,
    constants: wgpu::BindGroup,
    pipeline: wgpu::RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,
    instance_buffer: wgpu::Buffer,
}

impl Pipeline {
    pub fn new(device: &wgpu::Device, format: wgpu::TextureFormat) -> Self {
        let constant_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("bite::gui::quad uniform layout"),
            entries: &[wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::VERTEX,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: wgpu::BufferSize::new(
                        mem::size_of::<Uniforms>() as wgpu::BufferAddress
                    ),
                },
                count: None,
            }],
        });

        let constant_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("bite::gui::quad uniforms buffer"),
            size: mem::size_of::<Uniforms>() as wgpu::BufferAddress,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let constants = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("bite::gui::quad uniforms bind group"),
            layout: &constant_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: constant_buffer.as_entire_binding(),
            }],
        });

        let layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
            label: Some("bite::gui::quad pipeline layout"),
            push_constant_ranges: &[],
            bind_group_layouts: &[&constant_layout],
        });

        let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: Some("bite::gui::quad shader"),
            source: wgpu::ShaderSource::Wgsl(include_str!("../../shaders/quad.wgsl").into()),
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("bite::gui::quad pipeline"),
            layout: Some(&layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: "vs_main",
                buffers: &[
                    wgpu::VertexBufferLayout {
                        array_stride: mem::size_of::<glam::Vec3>() as u64,
                        step_mode: wgpu::VertexStepMode::Vertex,
                        attributes: &wgpu::vertex_attr_array![
                            0 => Float32x3,
                        ],
                    },
                    wgpu::VertexBufferLayout {
                        array_stride: mem::size_of::<Instance>() as u64,
                        step_mode: wgpu::VertexStepMode::Instance,
                        attributes: &wgpu::vertex_attr_array![
                            1 => Float32x2,
                            2 => Float32x2,
                            3 => Float32x3,
                        ],
                    },
                ],
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader,
                entry_point: "fs_main",
                targets: &[Some(wgpu::ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::REPLACE),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw,
                cull_mode: Some(wgpu::Face::Back),
                polygon_mode: wgpu::PolygonMode::Fill,
                unclipped_depth: false,
                conservative: false,
            },
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            multiview: None,
        });

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("bite::gui::quad vertex buffer"),
            contents: bytemuck::cast_slice(VERTICES),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("bite::gui::quad index buffer"),
            contents: bytemuck::cast_slice(INDICES),
            usage: wgpu::BufferUsages::INDEX,
        });

        let instance_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("bite::gui::quad instance buffer"),
            size: mem::size_of::<Instance>() as u64 * MAX_INSTANCES,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Pipeline {
            constant_buffer,
            constants,
            pipeline,
            vertex_buffer,
            index_buffer,
            instance_buffer,
        }
    }

    pub fn draw(
        &mut self,
        encoder: &mut wgpu::CommandEncoder,
        instances: &[Instance],
        device: &wgpu::Device,
        view: &wgpu::TextureView,
        staging_belt: &mut wgpu::util::StagingBelt,
        size: winit::dpi::PhysicalSize<u32>,
    ) {
        let uniforms = Uniforms::new([size.width as f32, size.height as f32]);

        staging_belt
            .write_buffer(
                encoder,
                &self.constant_buffer,
                0,
                wgpu::BufferSize::new(mem::size_of::<Uniforms>() as u64).unwrap(),
                device,
            )
            .copy_from_slice(bytemuck::bytes_of(&uniforms));

        let instance_bytes = bytemuck::cast_slice(instances);

        staging_belt
            .write_buffer(
                encoder,
                &self.instance_buffer,
                0,
                wgpu::BufferSize::new(instance_bytes.len() as u64).unwrap(),
                device,
            )
            .copy_from_slice(instance_bytes);

        staging_belt.finish();

        let mut render_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("bite::gui::quad render pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: true,
                },
            })],
            depth_stencil_attachment: None,
        });

        render_pass.set_pipeline(&self.pipeline);
        render_pass.set_bind_group(0, &self.constants, &[]);
        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        render_pass.set_vertex_buffer(1, self.instance_buffer.slice(..));
        render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);
        render_pass.draw_indexed(0..INDICES.len() as u32, 0, 0..instances.len() as u32);
    }
}
