use egui::style::{ScrollStyle, Selection, Spacing, Visuals, WidgetVisuals, Widgets};
use egui::{Color32, FontFamily, FontId, Rounding, Stroke, TextStyle};

use once_cell::sync::Lazy;
use tokenizing::colors;

#[derive(Debug, Clone)]
pub struct Style {
    pub primary_background: Color32,
    pub separator_width: f32,
    pub button_background: Color32,
    pub selection_color: Color32,
    pub tab_color: Color32,
    pub close_tab_color: Color32,
    pub tab_rounding: Rounding,
    pub active_text_color: Color32,
    pub text_color: Color32,
    pub separator_color: Color32,
    pub interactive_color: Color32,
}

pub static STYLE: Lazy<Style> = Lazy::new(|| Style {
    primary_background: Color32::from_rgb(45, 45, 45),
    separator_width: 3.0,
    button_background: colors::GRAY20,
    selection_color: Color32::from_rgba_unmultiplied(150, 150, 150, 60),
    close_tab_color: colors::GRAYAA,
    tab_color: colors::GRAY30,
    tab_rounding: Rounding::ZERO,
    active_text_color: colors::WHITE,
    text_color: colors::GRAYAA,
    separator_color: colors::GRAY20,
    interactive_color: Color32::LIGHT_GRAY,
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
        widgets: Widgets {
            noninteractive: WidgetVisuals {
                bg_fill: STYLE.tab_color,
                weak_bg_fill: Color32::TRANSPARENT,
                rounding: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 5.0,
                    color: STYLE.active_text_color,
                },
                expansion: 0.0,
            },
            inactive: WidgetVisuals {
                bg_fill: STYLE.primary_background,
                weak_bg_fill: Color32::TRANSPARENT,
                rounding: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 5.0,
                    color: STYLE.text_color,
                },
                expansion: 0.0,
            },
            hovered: WidgetVisuals {
                bg_fill: STYLE.primary_background,
                weak_bg_fill: Color32::TRANSPARENT,
                rounding: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 5.0,
                    color: STYLE.text_color,
                },
                expansion: 0.0,
            },
            active: WidgetVisuals {
                bg_fill: STYLE.tab_color,
                weak_bg_fill: Color32::TRANSPARENT,
                rounding: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 5.0,
                    color: STYLE.active_text_color,
                },
                expansion: 0.0,
            },
            open: WidgetVisuals {
                bg_fill: STYLE.primary_background,
                weak_bg_fill: Color32::TRANSPARENT,
                rounding: STYLE.tab_rounding,
                bg_stroke: Stroke::NONE,
                fg_stroke: Stroke {
                    width: 5.0,
                    color: STYLE.text_color,
                },
                expansion: 0.0,
            },
        },
        selection: Selection {
            bg_fill: STYLE.selection_color,
            stroke: Stroke::NONE,
        },
        menu_rounding: STYLE.tab_rounding,
        dark_mode: true,
        override_text_color: None,
        popup_shadow: egui::epaint::Shadow::NONE,
        window_rounding: STYLE.tab_rounding,
        window_fill: STYLE.tab_color,
        panel_fill: STYLE.tab_color,
        extreme_bg_color: Color32::from_rgba_unmultiplied(100, 100, 100, 160),
        text_cursor: Stroke::new(12.0, Color32::from_rgba_unmultiplied(150, 150, 150, 200)),
        ..Default::default()
    },
    wrap: Some(false),
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
            show_blocking_widget: true,
        }
    } else {
        egui::style::DebugOptions::default()
    },
    ..Default::default()
});

pub static DOCK: Lazy<egui_dock::Style> = Lazy::new(|| egui_dock::Style {
    dock_area_padding: None,
    main_surface_border_stroke: Stroke::NONE,
    main_surface_border_rounding: Rounding::ZERO,
    buttons: egui_dock::ButtonsStyle {
        close_tab_color: STYLE.close_tab_color,
        close_tab_active_color: STYLE.close_tab_color,
        close_tab_bg_fill: STYLE.close_tab_color,
        add_tab_bg_fill: STYLE.button_background,
        ..Default::default()
    },
    separator: egui_dock::SeparatorStyle {
        width: STYLE.separator_width,
        color_idle: STYLE.separator_color,
        color_hovered: STYLE.separator_color,
        color_dragged: STYLE.separator_color,
        ..Default::default()
    },
    tab_bar: egui_dock::TabBarStyle {
        bg_fill: STYLE.separator_color,
        height: 24.0,
        show_scroll_bar_on_overflow: true,
        rounding: STYLE.tab_rounding,
        hline_color: STYLE.separator_color,
        fill_tab_bar: true,
    },
    tab: egui_dock::TabStyle {
        active: egui_dock::TabInteractionStyle {
            outline_color: Color32::TRANSPARENT,
            rounding: STYLE.tab_rounding,
            bg_fill: STYLE.tab_color,
            text_color: STYLE.active_text_color,
        },
        inactive: egui_dock::TabInteractionStyle {
            outline_color: Color32::TRANSPARENT,
            rounding: STYLE.tab_rounding,
            bg_fill: STYLE.primary_background,
            text_color: STYLE.text_color,
        },
        focused: egui_dock::TabInteractionStyle {
            outline_color: Color32::TRANSPARENT,
            rounding: STYLE.tab_rounding,
            bg_fill: STYLE.tab_color,
            text_color: STYLE.active_text_color,
        },
        hovered: egui_dock::TabInteractionStyle {
            outline_color: Color32::TRANSPARENT,
            rounding: STYLE.tab_rounding,
            bg_fill: STYLE.primary_background,
            text_color: STYLE.text_color,
        },
        tab_body: egui_dock::TabBodyStyle::from_egui(&*EGUI),
        hline_below_active_tab_name: false,
        minimum_width: None
    },
    overlay: egui_dock::OverlayStyle {
        selection_color: STYLE.selection_color,
        ..Default::default()
    },
});
