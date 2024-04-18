pub mod egui;

use crate::Error;

/// Information about the screen used for rendering.
pub struct ScreenDescriptor {
    /// Width of the window in physical pixel.
    pub physical_width: u32,

    /// Height of the window in physical pixel.
    pub physical_height: u32,

    /// HiDPI scale factor.
    pub scale_factor: f32,
}

impl ScreenDescriptor {
    fn logical_size(&self) -> (u32, u32) {
        let logical_width = self.physical_width as f32 / self.scale_factor;
        let logical_height = self.physical_height as f32 / self.scale_factor;
        (logical_width as u32, logical_height as u32)
    }
}

pub struct Instance<'a> {
    device: wgpu::Device,
    queue: wgpu::Queue,
    surface: wgpu::Surface<'a>,
    surface_cfg: wgpu::SurfaceConfiguration,
    staging_belt: wgpu::util::StagingBelt,
}

impl<'window> Instance<'window> {
    pub fn new(window: &'window crate::Window) -> Result<Self, Error> {
        let backends = if cfg!(target_os = "windows") || cfg!(target_os = "linux") {
            wgpu::Backends::VULKAN
        } else {
            wgpu::Backends::METAL
        };

        // try to use vulkan/metal, else fallback to any supported backend
        match Self::new_with_backends(window, backends) {
            Err(..) => Self::new_with_backends(window, wgpu::Backends::all()),
            Ok(this) => Ok(this),
        }
    }

    fn new_with_backends(
        window: &'window crate::Window,
        backends: wgpu::Backends,
    ) -> Result<Self, Error> {
        let instance = wgpu::Instance::new(wgpu::InstanceDescriptor {
            backends,
            dx12_shader_compiler: wgpu::Dx12Compiler::Fxc,
            flags: wgpu::InstanceFlags::empty(),
            gles_minor_version: wgpu::Gles3MinorVersion::Automatic,
        });

        let surface = instance.create_surface(window).map_err(Error::SurfaceCreation)?;

        let adapter_options = wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        };

        let adapter = pollster::block_on(instance.request_adapter(&adapter_options))
            .ok_or(Error::AdapterRequest)?;

        let device_desc = wgpu::DeviceDescriptor {
            label: Some("bite::gui device"),
            required_features: wgpu::Features::empty(),
            required_limits: wgpu::Limits::default(),
        };

        let (device, queue) = pollster::block_on(adapter.request_device(&device_desc, None))
            .map_err(Error::DeviceRequest)?;

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

        let present_mode = surface_capabilities
            .present_modes
            .into_iter()
            .find(|&mode| mode == wgpu::PresentMode::Mailbox)
            .unwrap_or(wgpu::PresentMode::Fifo);

        let size = window.inner_size();
        let surface_cfg = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode,
            alpha_mode,
            view_formats: Vec::new(),
            desired_maximum_frame_latency: 2,
        };

        surface.configure(&device, &surface_cfg);

        let staging_belt = wgpu::util::StagingBelt::new(1024);

        Ok(Self {
            device,
            queue,
            surface,
            surface_cfg,
            staging_belt,
        })
    }

    pub fn draw(
        &mut self,
        window: &crate::Window,
        platform: &mut crate::winit_backend::Platform,
        render_pass: &mut egui::Pipeline,
        panels: &mut crate::panes::Panels,
    ) -> Result<(), Error> {
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

        // run all egui window rendering code
        panels.draw(&mut platform.context());

        // end the UI frame. We could now handle the output and draw the UI with the backend
        let full_output = platform.end_frame(Some(&window));
        let ppp = platform.context().pixels_per_point();
        let paint_jobs = platform.context().tessellate(full_output.shapes, ppp);

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

    pub fn resize(&mut self, width: u32, height: u32) {
        if width > 0 && height > 0 {
            self.surface_cfg.width = width;
            self.surface_cfg.height = height;
            self.surface.configure(&self.device, &self.surface_cfg);
        }
    }
}
