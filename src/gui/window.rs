use crate::gui::quad;
use crate::gui::Error;
use crate::gui::RenderContext;
use tokenizing::colors;

use std::ops::Bound;
use std::sync::atomic::Ordering;

use tokenizing::Token;
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
    pub staging_belt: wgpu::util::StagingBelt,

    pub glyph_brush: GlyphBrush<()>,

    quad_pipeline: crate::gui::quad::Pipeline,
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
        let surface = unsafe { instance.create_surface(&window).map_err(Error::SurfaceCreation)? };

        let adapter = instance
            .enumerate_adapters(backends)
            .find(|adapter| adapter.is_surface_supported(&surface))
            .ok_or(Error::AdapterRequest)?;

        let device_desc = wgpu::DeviceDescriptor {
            label: Some("bite::gui device"),
            features: wgpu::Features::empty(),
            limits: wgpu::Limits::downlevel_defaults(),
        };

        let (device, queue) =
            adapter.request_device(&device_desc, None).await.map_err(Error::DeviceRequest)?;

        let surface_capabilities = surface.get_capabilities(&adapter);

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
            present_mode: wgpu::PresentMode::Fifo,
            alpha_mode,
            view_formats: Vec::new(),
        };

        surface.configure(&device, &surface_cfg);

        let staging_belt = wgpu::util::StagingBelt::new(1024);
        let font = include_bytes!("../../assets/LigaSFMonoNerdFont-Regular.otf");
        let font = ab_glyph::FontArc::try_from_slice(font).unwrap();
        let glyph_brush = GlyphBrushBuilder::using_font(font).build(&device, surface_format);
        let quad_pipeline = crate::gui::quad::Pipeline::new(&device, surface_format);

        Ok(Self {
            size,
            instance,
            adapter,
            device,
            queue,
            surface,
            surface_cfg,
            staging_belt,
            glyph_brush,
            quad_pipeline,
        })
    }

    pub fn redraw(&mut self, ctx: &mut RenderContext) -> Result<(), Error> {
        let frame = self.surface.get_current_texture().map_err(Error::DrawTexture)?;
        let view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("bite::gui encoder"),
        });

        let font_size = ctx.scale_factor * ctx.font_size;

        // queue fps text
        self.glyph_brush.queue(wgpu_glyph::Section {
            screen_position: (ctx.scale_factor * 5.0, ctx.scale_factor * 5.0),
            bounds: (self.size.width as f32, self.size.height as f32),
            text: vec![wgpu_glyph::Text::new(&format!("FPS: {}", ctx.fps))
                .with_color(colors::WHITE)
                .with_scale(font_size)],
            ..wgpu_glyph::Section::default()
        });

        if ctx.show_donut.load(Ordering::Relaxed) {
            ctx.listing_offset = 0.0;

            // queue donut text
            self.glyph_brush.queue(wgpu_glyph::Section {
                screen_position: (self.size.width as f32 / 2.0, self.size.height as f32 / 2.0),
                layout: wgpu_glyph::Layout::default()
                    .h_align(wgpu_glyph::HorizontalAlign::Center)
                    .v_align(wgpu_glyph::VerticalAlign::Center),
                text: vec![wgpu_glyph::Text::new(&ctx.donut.frame)
                    .with_color(colors::WHITE)
                    .with_scale(ctx.scale_factor * 10.0)],
                ..wgpu_glyph::Section::default()
            });
        }

        // draw donut/fps
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

        if let Some(ref mut dissasembly) = ctx.dissasembly {
            let mut lines_scrolled = (ctx.listing_offset / ctx.font_size) as isize;

            #[allow(unused_must_use)]
            if lines_scrolled != 0 {
                // don't count any lines scrolled before the dissasembly is loaded
                if dissasembly.current_addr == 0 {
                    lines_scrolled = 0;
                }

                // find the instructions equal to or high than the current address
                // then skipping the number of lines we've scrolled past and if there
                // isn't any instructions left, show just the last instruction
                let inst = if lines_scrolled < 0 {
                    dissasembly
                        .proc
                        .in_range(Bound::Unbounded, Bound::Included(dissasembly.current_addr))
                        .rev()
                        .skip(-lines_scrolled as usize)
                        .next()
                        .unwrap_or(dissasembly.proc.iter().next().unwrap())
                } else {
                    dissasembly
                        .proc
                        .in_range(Bound::Included(dissasembly.current_addr), Bound::Unbounded)
                        .skip(lines_scrolled as usize)
                        .next()
                        .unwrap_or(dissasembly.proc.iter().last().unwrap())
                };

                dissasembly.current_addr = inst.0;
                ctx.listing_offset = 0.0;
            }

            let mut text: Vec<Token> = Vec::new();
            let symbols = &dissasembly.symbols;
            let mut lines_scrolled = 0;
            let lines = dissasembly
                .proc
                .iter()
                .skip_while(|(addr, _)| {
                    lines_scrolled += 1;
                    *addr < dissasembly.current_addr
                })
                .take((self.size.height as f32 / font_size).ceil() as usize);

            // for each instruction
            for (addr, inst) in lines {
                ctx.show_donut.store(false, Ordering::Relaxed);

                // if the address matches a symbol, print it
                if let Some(label) = symbols.get_by_addr(addr) {
                    text.push(Token::from_str("\n<", &colors::BLUE));
                    for token in label.tokens() {
                        text.push(token.as_ref());
                    }

                    text.push(Token::from_str(">:\n", &colors::BLUE));
                }

                // memory address
                text.push(Token::from_string(
                    format!("0x{addr:0>10X}  "),
                    &colors::GRAY40,
                ));

                // instruction's bytes
                text.push(Token::from_string(
                    dissasembly.proc.bytes(inst, addr),
                    &colors::GREEN,
                ));

                match inst {
                    Ok(inst) => {
                        for token in inst.tokens().iter() {
                            text.push(token.clone());
                        }
                    }
                    Err(err) => {
                        text.push(Token::from_str("<", &colors::GRAY40));
                        text.push(Token::from_string(format!("{err:?}"), &colors::RED));
                        text.push(Token::from_str(">", &colors::GRAY40));
                    }
                }

                text.push(Token::from_str("\n", &colors::WHITE));
            }

            // queue assembly listing text
            self.glyph_brush.queue(wgpu_glyph::Section {
                screen_position: (ctx.scale_factor * 5.0, font_size * 1.5),
                text: text.iter().map(|t| t.text(font_size)).collect(),
                ..wgpu_glyph::Section::default()
            });

            // orthogonal projection
            let proj = glam::mat4(
                glam::vec4(2.0 / self.size.width as f32, 0.0, 0.0, 0.0),
                glam::vec4(0.0, -2.0 / self.size.height as f32, 0.0, 0.0),
                glam::vec4(0.0, 0.0, 1.0, 0.0),
                glam::vec4(-1.0, 1.0, 0.0, 1.0),
            );

            // draw assembly listing
            self.glyph_brush
                .draw_queued_with_transform(
                    &self.device,
                    &mut self.staging_belt,
                    &mut encoder,
                    &view,
                    proj.to_cols_array(),
                )
                .map_err(Error::DrawText)?;

            let len = dissasembly.proc.instruction_count() as f32;
            let bar_height = (self.size.height * self.size.height) as f32 / (len * font_size);
            let bar_height = bar_height.max(font_size);
            let offset = lines_scrolled as f32 / len;
            let screen_offset = offset * (self.size.height as f32 - bar_height);

            let instances = [
                quad::Instance {
                    position: [self.size.width as f32 - font_size / 2.0, 0.0],
                    size: [font_size / 3.0, self.size.height as f32],
                    color: colors::GRAY10,
                },
                quad::Instance {
                    position: [self.size.width as f32 - font_size / 2.0, screen_offset],
                    size: [font_size / 3.0, bar_height],
                    color: colors::GRAY99,
                },
            ];

            self.quad_pipeline.draw(
                &mut encoder,
                &instances,
                &self.device,
                &view,
                &mut self.staging_belt,
                self.size,
            );
        }

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
            self.surface_cfg.width = size.width;
            self.surface_cfg.height = size.height;
            self.surface.configure(&self.device, &self.surface_cfg);
        }
    }
}
