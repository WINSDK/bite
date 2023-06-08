use crate::disassembly::Disassembly;
use crate::gui::egui_backend::{self, ScreenDescriptor};
use crate::gui::winit_backend::{CustomEvent, Platform};
use crate::gui::Error;
use crate::gui::RenderContext;
use tokenizing::{colors, Token};

use std::sync::atomic::Ordering;
use std::sync::Arc;

use egui::CentralPanel;
use egui::{Button, RichText};
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

    // fn refresh_listing(&mut self, disassembly: &mut Disassembly, font_size: f32) {
    //     disassembly.lines_scrolled = 0;
    //     let mut text: Vec<Token> = Vec::new();
    //     let symbols = &disassembly.symbols;
    //     let lines = disassembly
    //         .proc
    //         .iter()
    //         .skip_while(|(addr, _)| {
    //             disassembly.lines_scrolled += 1;
    //             *addr < disassembly.current_addr
    //         })
    //         .take((self.size.height as f32 / font_size).ceil() as usize);

    //     // for each instruction
    //     for (addr, inst) in lines {
    //         // if the address matches a symbol, print it
    //         if let Some(label) = symbols.get_by_addr_ref(addr) {
    //             text.push(Token::from_str("\n<", &colors::BLUE));
    //             for token in label.tokens() {
    //                 text.push(token.to_owned());
    //             }

    //             text.push(Token::from_str(">:\n", &colors::BLUE));
    //         }

    //         // memory address
    //         text.push(Token::from_string(
    //             format!("0x{addr:0>10X}  "),
    //             &colors::GRAY40,
    //         ));

    //         // instruction's bytes
    //         text.push(Token::from_string(
    //             disassembly.proc.bytes(inst, addr),
    //             &colors::GREEN,
    //         ));

    //         match inst {
    //             Ok(inst) => {
    //                 for token in inst.tokens().iter() {
    //                     text.push(token.clone());
    //                 }
    //             }
    //             Err(err) => {
    //                 text.push(Token::from_str("<", &colors::GRAY40));
    //                 text.push(Token::from_string(format!("{err:?}"), &colors::RED));
    //                 text.push(Token::from_str(">", &colors::GRAY40));
    //             }
    //         }

    //         text.push(Token::from_str("\n", &colors::WHITE));
    //     }

    //     disassembly.lines = text;
    // }
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
            label: Some("bite::gui encoder"),
        });

        if ctx.show_donut.load(Ordering::Relaxed) {
            // queue donut text
            self.glyph_brush.queue(wgpu_glyph::Section {
                screen_position: (self.size.width as f32 / 2.0, self.size.height as f32 / 2.0),
                layout: wgpu_glyph::Layout::default()
                    .h_align(wgpu_glyph::HorizontalAlign::Center)
                    .v_align(wgpu_glyph::VerticalAlign::Center),
                text: vec![wgpu_glyph::Text::new(&ctx.donut.frame)
                    .with_color(colors::WHITE)
                    .with_scale(platform.scale_factor as f32 * 10.0)],
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

        // begin to draw the UI frame
        platform.begin_frame();

        // draw the primary panel
        CentralPanel::default()
            .frame(
                egui::Frame::central_panel(&platform.context().style())
                    .inner_margin(0.0)
                    .fill(ctx.style.tab_color),
            )
            .show(&platform.context(), |ui| {
                // alt-tab'ing between tabs
                if ui.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::Tab)) {
                    let focused_idx = match ctx.tabs.focused_leaf() {
                        Some(idx) => idx,
                        None => egui_dock::NodeIndex::root(),
                    };

                    // don't do tab'ing if there are no tabs
                    if ctx.tabs.len() == 0 {
                        return;
                    }

                    let focused = &mut ctx.tabs[focused_idx];
                    if let egui_dock::Node::Leaf { tabs, active, .. } = focused {
                        if active.0 != tabs.len() - 1 {
                            let tab_idx = active.0 + 1;
                            ctx.tabs.set_active_tab(focused_idx, egui_dock::TabIndex(tab_idx));
                        } else {
                            ctx.tabs.set_active_tab(focused_idx, egui_dock::TabIndex(0));
                        }
                    }
                }

                // generic keyboard inputs
                keyboard_input(ui, ctx);

                // top bar
                title_bar_ui(ui, platform, ctx);

                egui_dock::DockArea::new(&mut ctx.tabs)
                    .style(ctx.style.dock())
                    .show_close_buttons(ctx.buffers.has_multiple_tabs())
                    .draggable_tabs(ctx.buffers.has_multiple_tabs())
                    .show_inside(ui, &mut ctx.buffers);
            });

        // end the UI frame. We could now handle the output and draw the UI with the backend
        let full_output = platform.end_frame(Some(&*ctx.window));
        let paint_jobs = platform.context().tessellate(full_output.shapes);

        // upload all resources for the GPU
        let screen_descriptor = ScreenDescriptor {
            physical_width: self.surface_cfg.width,
            physical_height: self.surface_cfg.height,
            scale_factor: ctx.window.scale_factor() as f32,
        };

        let tdelta: egui::TexturesDelta = full_output.textures_delta;
        render_pass
            .add_textures(&self.device, &self.queue, &tdelta)
            .expect("add texture ok");
        render_pass.update_buffers(&self.device, &self.queue, &paint_jobs, &screen_descriptor);

        // Record all render passes.
        render_pass
            .execute(
                &mut encoder,
                &view,
                &paint_jobs,
                &screen_descriptor,
                Some(wgpu::Color::BLACK),
            )
            .unwrap();

        // submit work
        self.staging_belt.finish();
        self.queue.submit(Some(encoder.finish()));

        // schedule texture to be renderer on surface
        frame.present();

        render_pass.remove_textures(tdelta).expect("remove texture ok");

        // recall unused staging buffers
        self.staging_belt.recall();

        Ok(())
    }

    pub fn resize(&mut self, ctx: &mut RenderContext, size: PhysicalSize<u32>) {
        if size.width > 0 && size.height > 0 {
            self.size = size;
            self.surface_cfg.width = size.width;
            self.surface_cfg.height = size.height;
            self.surface.configure(&self.device, &self.surface_cfg);
        }

        if let Some(ref mut disassembly) = ctx.dissasembly {}
    }
}

fn ask_for_binary(ctx: &mut RenderContext) {
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

// if let Some(ref mut disassembly) = ctx.dissasembly {
//     let mut lines_scrolled = (ctx.listing_offset / ctx.font_size) as isize;
//     ctx.show_donut.store(false, Ordering::Relaxed);
//
//     // on first access refresh the listing
//     if disassembly.current_addr == 0 {
//         self.refresh_listing(disassembly, font_size);
//     }
//
//     if lines_scrolled != 0 {
//         // don't count any lines scrolled before the dissasembly is loaded
//         if disassembly.current_addr == 0 {
//             lines_scrolled = 0;
//         }
//
//         // find the instructions equal to or high than the current address
//         // then skipping the number of lines we've scrolled past and if there
//         // isn't any instructions left, show just the last instruction
//         let inst = if lines_scrolled < 0 {
//             disassembly
//                 .proc
//                 .in_range(Bound::Unbounded, Bound::Included(disassembly.current_addr))
//                 .rev()
//                 .skip(-lines_scrolled as usize)
//                 .next()
//                 .unwrap_or_else(|| disassembly.proc.iter().next().unwrap())
//         } else {
//             disassembly
//                 .proc
//                 .in_range(Bound::Included(disassembly.current_addr), Bound::Unbounded)
//                 .skip(lines_scrolled as usize)
//                 .next()
//                 .unwrap_or_else(|| disassembly.proc.iter().last().unwrap())
//         };
//
//         disassembly.current_addr = inst.0;
//         ctx.listing_offset = 0.0;
//         self.refresh_listing(disassembly, font_size);
//     }
// }

fn fullscreen(ctx: &mut RenderContext) {
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

fn keyboard_input(ui: &mut egui::Ui, ctx: &mut RenderContext) {
    if ui.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::O)) {
        ask_for_binary(ctx);
    }

    if ui.input_mut(|i| i.consume_key(egui::Modifiers::CTRL, egui::Key::F)) {
        fullscreen(ctx);
    }
}

fn title_bar_ui(ui: &mut egui::Ui, platform: &mut Platform, ctx: &mut RenderContext) {
    egui::menu::bar(ui, |ui| {
        ui.menu_button("File", |ui| {
            if ui.button(crate::icon!(FOLDER_OPEN, "Open")).clicked() {
                ask_for_binary(ctx);
                ui.close_menu();
            }

            if ui.button(crate::icon!(CROSS, "Exit")).clicked() {
                platform.send_event(CustomEvent::CloseRequest);
                ui.close_menu();
            }
        });

        ui.with_layout(egui::Layout::right_to_left(egui::Align::Max), |ui| {
            ui.spacing_mut().item_spacing.x = 0.0;
            close_maximize_minimize(ui, platform, ctx);
        });
    });
}

// Show some close/maximize/minimize buttons for the native window.
fn close_maximize_minimize(ui: &mut egui::Ui, platform: &mut Platform, ctx: &mut RenderContext) {
    let height = 12.0;
    let close_response = ui.add(Button::new(
        RichText::new(crate::icon!(CROSS, "")).size(height),
    ));

    if close_response.clicked() {
        platform.send_event(CustomEvent::CloseRequest);
    }

    let maximized_response = ui.add(Button::new(
        RichText::new(crate::icon!(CHECKBOX_UNCHECKED, "")).size(height),
    ));

    if maximized_response.clicked() {
        fullscreen(ctx);
    }

    let minimized_response = ui.add(Button::new(
        RichText::new(crate::icon!(MINUS, "")).size(height),
    ));

    if minimized_response.clicked() {
        if let Some(true) = ctx.window.is_minimized() {
            ctx.window.set_minimized(true);
        } else {
            ctx.window.set_minimized(false);
        }
    }
}
