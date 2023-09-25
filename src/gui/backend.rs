use crate::gui::egui_backend::{self, ScreenDescriptor};
use crate::gui::winit_backend::Platform;
use crate::gui::Error;
use crate::gui::RenderContext;

use std::sync::atomic::Ordering;
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

    pub fn redraw(
        &mut self,
        ctx: &mut RenderContext,
        platform: &mut Platform,
        render_pass: &mut egui_backend::Pipeline,
    ) -> Result<(), Error> {
        let frame = match self.surface.get_current_texture() {
            Ok(frame) => frame,
            Err(..) => return Ok(()),
        };

        let view = frame.texture.create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self.device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("bite::gui encoder"),
        });

        // handle inputs before `begin_frame` consumes them
        let events_processed = platform.record_terminal_input(&mut ctx.terminal);

        if events_processed > 0 {
            // if a goto command is being run, start performing the autocomplete
            if let Some(("g" | "goto", arg)) = ctx.terminal.current_line().split_once(' ') {
                dbg!(arg);
                if let Some(ref dissasembly) = ctx.dissasembly {
                    dbg!(crate::expr::parse(&dissasembly.symbols, arg));
                }
            }

            // store new commands recorded
            let _ = ctx.terminal.save_command_history();
        }

        // begin to draw the UI frame
        platform.begin_frame();

        // process terminal commands
        // FIXME: remove `to_vec`
        let cmds = ctx.terminal.take_commands().to_vec();
        crate::commands::process_commands(ctx, &cmds);

        egui::TopBottomPanel::top("top bar").show(&platform.context(), |ui| {
            // generic keyboard inputs
            keyboard_input(ui, ctx);

            // title bar
            super::top_bar(ui, ctx, platform);
        });

        let terminal = egui::TopBottomPanel::bottom("terminal")
            .min_height(80.0)
            .max_height(510.0)
            .resizable(true)
            .frame({
                let mut margin = super::STYLE.egui().spacing.window_margin;
                margin.top = ctx.style.separator_width * 2.0;

                egui::Frame::default()
                    .outer_margin(egui::Margin {
                        top: ctx.style.separator_width * 2.0,
                        ..Default::default()
                    })
                    .inner_margin(margin)
                    .fill(tokenizing::colors::GRAY35)
            });

        // disable on-hover highlighting for terminal
        let mut visuals = super::STYLE.egui().visuals.clone();
        visuals.widgets.active.fg_stroke = egui::Stroke::NONE;
        visuals.widgets.hovered.fg_stroke = egui::Stroke::NONE;
        platform.context().set_visuals(visuals);

        terminal.show(&platform.context(), |ui| {
            ui.with_layout(egui::Layout::bottom_up(egui::Align::Min), |ui| {
                super::terminal(ui, ctx);
            })
        });

        // re-enable on-hover highlighting
        let visuals = super::STYLE.egui().visuals.clone();
        platform.context().set_visuals(visuals);

        // draw the primary panel
        egui::CentralPanel::default()
            .frame({
                egui::Frame::default().inner_margin(egui::Margin {
                    top: ctx.style.separator_width,
                    ..Default::default()
                })
            })
            .show(&platform.context(), |ui| {
                if ctx.show_donut.load(Ordering::Relaxed) {
                    let donut_frame = ctx.donut.frame.clone();
                    let layout = egui::Layout::centered_and_justified(egui::Direction::LeftToRight);
                    ui.with_layout(layout, |ui| {
                        ui.label(donut_frame);
                    });
                } else {
                    super::tabbed_panel(ui, ctx);
                }
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

        let tdelta = full_output.textures_delta;

        render_pass.add_textures(&self.device, &self.queue, &tdelta)?;
        render_pass.update_buffers(&self.device, &self.queue, &paint_jobs, &screen_descriptor);

        // Record all render passes.
        render_pass.execute(&mut encoder, &view, &paint_jobs, &screen_descriptor)?;

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

pub fn ask_for_binary(ctx: &mut RenderContext) {
    // create dialog popup and get references to the donut and dissasembly
    let dialog = rfd::FileDialog::new().set_parent(&*ctx.window).pick_file();

    // load binary
    if let Some(path) = dialog {
        // ghost disassembling thread if a binary is already loaded.
        if ctx.disassembling_thread.is_some() {
            ctx.disassembling_thread = None;
        }

        ctx.start_disassembling(path);
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
