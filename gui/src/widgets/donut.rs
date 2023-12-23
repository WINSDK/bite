use crate::common::*;

pub struct Donut {
    timer: Timer,
    is_eaten: bool,
    a: f32,
    b: f32,
    frame: String,
}

impl Donut {
    pub fn new(is_eaten: bool) -> Self {
        Self {
            timer: Timer::new(60),
            is_eaten,
            a: 0.0,
            b: 0.0,
            frame: String::with_capacity(80 * 64),
        }
    }

    pub fn frame(&self) -> &str {
        &self.frame
    }

    fn update(&mut self) {
        let elapsed = self.timer.times_elapsed();

        for _ in 0..elapsed {
            self.step();
        }

        if elapsed > 0 {
            self.timer.reset();
        }
    }

    fn step(&mut self) {
        let mut b2 = [b' '; 1408];
        let mut z = [0.0f32; 1408];

        for j in (0..90).map(|x| x as f32 * 0.07) {
            for i in (0..314 - self.is_eaten as usize * 60).map(|x| x as f32 * 0.02) {
                let sini = i.sin();
                let cosi = i.cos();
                let sinj = j.sin();
                let cosj = j.cos();
                let cosj2 = cosj + 2.0;
                let sina = self.a.sin();
                let cosa = self.a.cos();
                let sinb = self.b.sin();
                let cosb = self.b.cos();
                let m = (sini * cosj2 * sina + sinj * cosa + 5.0).recip();
                let t = sini * cosj2 * cosa - sinj * sina;
                let x = 24.0 + 30.0 * m * (cosi * cosj2 * cosb - t * sinb);
                let y = 12.0 + 15.0 * m * (cosi * cosj2 * sinb + t * cosb);
                let o = x as usize + 64 * y as usize;

                let n = sinj * sina - sini * cosj * cosa;
                let n = n * cosb - sini * cosj * sina - sinj * cosa - cosi * cosj * sinb;
                let n = (n * 8.0) as usize;

                if y > 0.0 && y < 22.0 && x > 0.0 && x < 64.0 && m > z[o] {
                    z[o] = m;
                    b2[o] = b".,-~:;=!*#$@"[n];
                }
            }
        }

        self.frame.clear();
        for k in 0..1408 {
            match k % 64 {
                0 => self.frame.push('\n'),
                _ => self.frame.push(b2[k] as char),
            }
        }

        self.a += 0.04;
        self.b += 0.02;
    }
}

impl Display for Donut {
    fn show(&mut self, ui: &mut egui::Ui) {
        self.update();

        // HACK: has to be done this way since egui can't center two
        // widgets at once (progress bar and donut).
        let panel = ui.max_rect();
        let mut font = FONT;
        font.size /= 1.5;
        let rect = ui.painter().text(
            panel.center(),
            egui::Align2::CENTER_CENTER,
            self.frame(),
            font,
            egui::Color32::WHITE,
        );
        ui.allocate_rect(rect, egui::Sense::hover());
    }
}
