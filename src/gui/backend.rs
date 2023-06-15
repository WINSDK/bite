use crate::disassembly::Disassembly;
use crate::gui::egui_backend::{self, ScreenDescriptor};
use crate::gui::winit_backend::Platform;
use crate::gui::Error;
use crate::gui::RenderContext;

use std::sync::atomic::Ordering;
use std::sync::Arc;

use wgpu_glyph::{GlyphBrush, GlyphBrushBuilder};
use winit::dpi::PhysicalSize;
use winit::event::{VirtualKeyCode, ModifiersState};

const NO_MODIFIERS: ModifiersState = ModifiersState::empty();

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
}

impl Backend {
    pub async fn new(window: &winit::window::Window) -> Result<Self, Error> {
        let backends = if cfg!(target_os = "windows") || cfg!(target_os = "linux") {
            wgpu::Backends::VULKAN
        } else {
            wgpu::Backends::METAL
        };

        // try to use vulkan/metal, else fallback to any supported backend
        match Self::new_with_backends(window, backends).await {
            Err(..) => Self::new_with_backends(window, wgpu::Backends::all()).await,
            Ok(this) => Ok(this),
        }
    }

    async fn new_with_backends(
        window: &winit::window::Window,
        backends: wgpu::Backends,
    ) -> Result<Self, Error> {
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
                .find(wgpu::TextureFormat::is_srgb)
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
        })
    }

    fn draw_donut(
        &mut self,
        ctx: &mut RenderContext,
        platform: &mut Platform,
    ) -> Result<(), Error> {
        let frame = match self.surface.get_current_texture() {
            Ok(frame) => frame,
            Err(..) => return Ok(()),
        };

        let view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("bite::donut encoder"),
        });

        let rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("bite::donut render pass"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(egui_backend::CLEAR_COLOR),
                    store: true,
                },
            })],
            depth_stencil_attachment: None,
        });

        drop(rpass);

        if ctx.show_donut.load(Ordering::Relaxed) {
            let text = wgpu_glyph::Text::new(&ctx.donut.frame)
                .with_color([1.0, 1.0, 1.0, 1.0])
                .with_scale(platform.scale_factor() * 10.0);

            // queue donut text
            self.glyph_brush.queue(wgpu_glyph::Section {
                screen_position: (self.size.width as f32 / 2.0, self.size.height as f32 / 2.0),
                layout: wgpu_glyph::Layout::default()
                    .h_align(wgpu_glyph::HorizontalAlign::Center)
                    .v_align(wgpu_glyph::VerticalAlign::Center),
                text: vec![text],
                ..wgpu_glyph::Section::default()
            });

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

    pub fn redraw(
        &mut self,
        ctx: &mut RenderContext,
        platform: &mut Platform,
        render_pass: &mut egui_backend::Pipeline,
    ) -> Result<(), Error> {
        if ctx.show_donut.load(Ordering::Relaxed) {
            return self.draw_donut(ctx, platform);
        }

        let frame = match self.surface.get_current_texture() {
            Ok(frame) => frame,
            Err(..) => return Ok(()),
        };

        let view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("bite::gui encoder"),
        });

        // keep a record of all keystroke since the previous frame
        let keys = platform.raw_keys();

        if keys.contains(&(NO_MODIFIERS, VirtualKeyCode::Back)) {
            ctx.cmd_input.pop();
        }

        if keys.contains(&(NO_MODIFIERS, VirtualKeyCode::Return)) {
            ctx.cmd_input = String::new();
        }

        for (modi, key) in keys {
            // only record keys that either have no modifiers or the shift modifier
            if (modi | ModifiersState::SHIFT) != ModifiersState::SHIFT {
                continue;
            }

            if let Some(chr) = winit_key_to_char(modi, key) {
                ctx.cmd_input.push(chr);
            }
        }

        // begin to draw the UI frame
        platform.begin_frame();

        egui::TopBottomPanel::top("top bar").show(&platform.context(), |ui| {
            // generic keyboard inputs
            keyboard_input(ui, ctx);

            // title bar
            super::top_bar(ui, ctx, platform);
        });

        // draw the primary panel
        egui::CentralPanel::default()
            .frame({
                let margin = egui::Margin {
                    left: 0.0,
                    right: 0.0,
                    top: ctx.style.separator_width,
                    bottom: 0.0,
                };

                egui::Frame::none().outer_margin(margin)
            })
            .show(&platform.context(), |ui| {
                super::tabbed_panel(ui, ctx);
            });

        egui::TopBottomPanel::bottom("terminal").show(&platform.context(), |ui| {
            super::terminal(ui, ctx);
        });

        // end the UI frame. We could now handle the output and draw the UI with the backend
        let full_output = platform.end_frame(Some(&*ctx.window));
        let paint_jobs = platform.context().tessellate(full_output.shapes);

        // upload all resources for the GPU
        let screen_descriptor = ScreenDescriptor {
            physical_width: self.surface_cfg.width,
            physical_height: self.surface_cfg.height,
            scale_factor: platform.scale_factor(),
        };

        let tdelta: egui::TexturesDelta = full_output.textures_delta;

        render_pass.add_textures(&self.device, &self.queue, &tdelta)?;
        render_pass.update_buffers(&self.device, &self.queue, &paint_jobs, &screen_descriptor);

        // Record all render passes.
        render_pass.execute(
            &mut encoder,
            &view,
            &paint_jobs,
            &screen_descriptor,
        )?;

        // submit work
        self.staging_belt.finish();
        self.queue.submit(Some(encoder.finish()));

        // schedule texture to be renderer on surface
        frame.present();

        render_pass.remove_textures(tdelta)?;

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

fn winit_key_to_char(modifiers: ModifiersState, keycode: VirtualKeyCode) -> Option<char> {
    if keycode == VirtualKeyCode::Space {
        return Some(' ');
    }

    if keycode >= VirtualKeyCode::A && keycode <= VirtualKeyCode::Z {
        let base_char = if modifiers.shift() {
            'A'
        } else {
            'a'
        };

        let key_index = keycode as u8 - VirtualKeyCode::A as u8;
        let character = (base_char as u8 + key_index) as char;
        return Some(character)
    }

    None
}

pub fn ask_for_binary(ctx: &mut RenderContext) {
    // create dialog popup and get references to the donut and dissasembly
    let dialog = rfd::FileDialog::new().set_parent(&*ctx.window).pick_file();

    // load binary
    if let Some(path) = dialog {
        // ghost disassembling thread if a binary is already loaded.
        if ctx.disassembling_thread.is_some() {
            ctx.disassembling_thread = None;
        }

        let show_donut = Arc::clone(&ctx.show_donut);
        ctx.dissasembly = None;
        ctx.disassembling_thread = Some(std::thread::spawn(|| Disassembly::new(path, show_donut)));
    }
}

pub fn fullscreen(ctx: &mut RenderContext) {
    let monitor = match ctx.window.current_monitor() {
        Some(monitor) => monitor,
        None => return,
    };

    #[cfg(target_family = "windows")]
    unsafe {
        use crate::gui::utils::windows::*;
        use winit::platform::windows::{MonitorHandleExtWindows, WindowExtWindows};

        let mut info = MonitorInfo {
            size: std::mem::size_of::<MonitorInfo>() as u32,
            monitor_area: Rect::default(),
            work_area: Rect::default(),
            flags: 0,
        };

        if GetMonitorInfoW(monitor.hmonitor(), &mut info) == 0 {
            return;
        }

        let PhysicalSize { width, height } = ctx.window.outer_size();
        let work_area_width = info.work_area.right - info.work_area.left;
        let work_area_height = info.work_area.bottom - info.work_area.top;

        // check if the window is fullscreen borderless
        if width == work_area_width && height == work_area_height {
            let attr = WS_VISIBLE | WS_THICKFRAME | WS_POPUP;

            SetWindowLongPtrW(ctx.window.hwnd(), GWL_STYLE, attr);
            SetWindowPos(
                ctx.window.hwnd(),
                HWND_TOP,
                ctx.unwindowed_pos.x as u32,
                ctx.unwindowed_pos.y as u32,
                ctx.unwindowed_size.width,
                ctx.unwindowed_size.height,
                SWP_NOZORDER,
            );
        } else {
            let attr = WS_VISIBLE | WS_OVERLAPPED;

            ctx.unwindowed_size = ctx.window.outer_size();
            ctx.unwindowed_pos = ctx.window.outer_position().unwrap_or_default();

            SetWindowLongPtrW(ctx.window.hwnd(), GWL_STYLE, attr);
            SetWindowPos(
                ctx.window.hwnd(),
                HWND_TOP,
                info.work_area.left,
                info.work_area.top,
                work_area_width,
                work_area_height,
                SWP_NOZORDER,
            );
        }
    }

    #[cfg(target_family = "unix")]
    ctx.window.set_fullscreen(match ctx.window.fullscreen() {
        Some(..) => None,
        None => Some(winit::window::Fullscreen::Borderless(Some(monitor))),
    });
}

pub fn keyboard_input(ui: &mut egui::Ui, ctx: &mut RenderContext) {
    if ui.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::O)) {
        ask_for_binary(ctx);
    }

    // alt-tab'ing between tabs
    if ui.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::Tab)) {
        let focused_idx = match ctx.panels.focused_leaf() {
            Some(idx) => idx,
            None => egui_dock::NodeIndex::root(),
        };

        // don't do tab'ing if there are no tabs
        if ctx.panels.num_tabs() == 0 {
            return;
        }

        let focused = &mut ctx.panels[focused_idx];
        if let egui_dock::Node::Leaf { tabs, active, .. } = focused {
            if active.0 != tabs.len() - 1 {
                let tab_idx = active.0 + 1;
                ctx.panels.set_active_tab(focused_idx, egui_dock::TabIndex(tab_idx));
            } else {
                ctx.panels.set_active_tab(focused_idx, egui_dock::TabIndex(0));
            }
        }
    }
}
