pub struct Donut {
    is_eaten: bool,
    a: f64,
    b: f64,
    pub frame: String,
}

impl Donut {
    pub fn new(is_eaten: bool) -> Self {
        Self {
            is_eaten,
            a: 0.0,
            b: 0.0,
            frame: String::with_capacity(80 * 64),
        }
    }

    pub fn update_frame(&mut self) {
        let mut b2 = [' '; 1408];
        let mut z = [0.0; 1408];

        for j in (0..90).map(|x| x as f64 * 0.07) {
            for i in (0..314 - self.is_eaten as usize * 60).map(|x| x as f64 * 0.02) {
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
                    b2[o] = ['.', ',', '-', '~', ':', ';', '=', '!', '*', '#', '$', '@'][n];
                }
            }
        }

        self.frame.clear();
        for k in 0..1408 {
            match k % 64 {
                0 => self.frame.push('\n'),
                _ => self.frame.push(b2[k]),
            }
        }

        self.a += 0.04;
        self.b += 0.02;
    }
}
