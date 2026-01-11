use egui::style::{ScrollStyle, Selection, Spacing, Visuals, WidgetVisuals, Widgets};
use egui::{Color32, CornerRadius, FontFamily, FontId, Stroke, TextStyle};
use once_cell::sync::Lazy;
use tokenizing::colors;
use config::CONFIG;

#[derive(Debug, Clone)]
pub struct Style {
    pub separator_width: f32,
    pub selection_color: Color32,
    pub tab_rounding: CornerRadius,
    pub active_text_color: Color32,
    pub text_color: Color32,
}

pub static STYLE: Lazy<Style> = Lazy::new(|| Style {
    separator_width: 3.0,
    selection_color: Color32::from_rgba_unmultiplied(0xbf, 0xbf, 0xbf, 60),
    tab_rounding: CornerRadius::ZERO,
    active_text_color: colors::WHITE,
    text_color: colors::GRAYAA,
});

pub static EGUI: Lazy<egui::Style> = Lazy::new(|| egui::Style {
    spacing: Spacing {
        slider_width: 10.0,
        scroll: ScrollStyle {
            bar_width: 10.0,
            bar_inner_margin: STYLE.separator_width,
            bar_outer_margin: STYLE.separator_width,
            ..Default::default()
        },
        ..Default::default()
    },
    visuals: Visuals {
        // Slightly transparent selection for text fields so background shows through.
        // Tabs/pane drag still use `STYLE.selection_color` directly.
        selection: {
            let [r, g, b, a] = STYLE.selection_color.to_srgba_unmultiplied();
            let selection_bg =
                Color32::from_rgba_unmultiplied(r, g, b, ((a as f32) * 0.5).round() as u8);
            Selection {
                bg_fill: selection_bg,
                stroke: Stroke::NONE,
            }
        },
        widgets: Widgets {
            noninteractive: WidgetVisuals {
                bg_fill: CONFIG.colors.bg_secondary,
                weak_bg_fill: CONFIG.colors.bg_secondary,
                corner_radius: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 0.0,
                    color: STYLE.active_text_color,
                },
                expansion: 0.0,
            },
            inactive: WidgetVisuals {
                bg_fill: CONFIG.colors.bg_secondary,
                weak_bg_fill: CONFIG.colors.bg_secondary,
                corner_radius: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 0.0,
                    color: STYLE.text_color,
                },
                expansion: 0.0,
            },
            hovered: WidgetVisuals {
                bg_fill: CONFIG.colors.bg_secondary,
                weak_bg_fill: CONFIG.colors.bg_secondary,
                corner_radius: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 0.0,
                    color: STYLE.text_color,
                },
                expansion: 0.0,
            },
            active: WidgetVisuals {
                bg_fill: CONFIG.colors.bg_primary,
                weak_bg_fill: CONFIG.colors.bg_secondary,
                corner_radius: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 0.0,
                    color: STYLE.active_text_color,
                },
                expansion: 0.0,
            },
            open: WidgetVisuals {
                bg_fill: CONFIG.colors.bg_secondary,
                weak_bg_fill: CONFIG.colors.bg_secondary,
                corner_radius: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 0.0,
                    color: STYLE.text_color,
                },
                expansion: 0.0,
            },
        },
        menu_corner_radius: STYLE.tab_rounding,
        dark_mode: true,
        override_text_color: None,
        popup_shadow: egui::epaint::Shadow::NONE,
        window_corner_radius: STYLE.tab_rounding,
        window_fill: colors::BLACK,
        panel_fill: colors::BLACK,
        extreme_bg_color: colors::BLACK,
        ..Default::default()
    },
    wrap_mode: Some(egui::TextWrapMode::Wrap),
    explanation_tooltips: false,
    text_styles: {
        let mut styles = std::collections::BTreeMap::new();

        styles.insert(TextStyle::Heading, FontId::new(18.0, FontFamily::Monospace));
        styles.insert(TextStyle::Body, FontId::new(16.0, FontFamily::Monospace));
        styles.insert(TextStyle::Button, FontId::new(14.0, FontFamily::Monospace));
        styles.insert(TextStyle::Small, FontId::new(12.0, FontFamily::Monospace));
        styles
    },
    #[cfg(debug_assertions)]
    debug: if commands::ARGS.debug {
        egui::style::DebugOptions {
            debug_on_hover: true,
            debug_on_hover_with_all_modifiers: true,
            hover_shows_next: true,
            show_expand_width: true,
            show_expand_height: true,
            show_resize: true,
            show_interactive_widgets: true,
            show_widget_hits: true,
            show_unaligned: true,
        }
    } else {
        egui::style::DebugOptions::default()
    },
    ..Default::default()
});
