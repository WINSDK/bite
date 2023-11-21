pub type Identifier = &'static str;

pub const FONT: egui::FontId = egui::FontId::new(14.0, egui::FontFamily::Monospace);

// pub const SOURCE: Identifier = crate::icon!(EMBED2, " Source");
pub const DISASSEMBLY: Identifier = crate::icon!(PARAGRAPH_LEFT, " Disassembly");
pub const FUNCTIONS: Identifier = crate::icon!(LIGATURE, " Functions");
pub const LOGGING: Identifier = crate::icon!(TERMINAL, " Logs");

pub fn tokens_to_layoutjob(tokens: Vec<tokenizing::Token>) -> egui::text::LayoutJob {
    let mut job = egui::text::LayoutJob::default();

    for token in tokens {
        job.append(
            &token.text,
            0.0,
            egui::TextFormat {
                font_id: FONT,
                color: token.color,
                ..Default::default()
            },
        );
    }

    job
}

