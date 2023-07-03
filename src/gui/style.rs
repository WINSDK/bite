use std::collections::BTreeMap;

use egui::epaint::Shadow;
use egui::style::{Selection, Spacing, Visuals, WidgetVisuals, Widgets};
use egui::{Color32, FontFamily, FontId, Rounding, Stroke, TextStyle};

use tokenizing::colors;

#[derive(Debug, Clone)]
pub struct Style {
    pub separator_width: f32,
    pub active_background: Color32,
    pub background: Color32,
    pub button_background: Color32,
    pub selection_color: Color32,
    pub tab_color: Color32,
    pub close_tab_color: Color32,
    pub tab_rounding: Rounding,
    pub active_text_color: Color32,
    pub text_color: Color32,
    pub separator: Color32,
    egui: egui::Style,
    dock: egui_dock::Style,
}

impl Default for Style {
    fn default() -> Self {
        let mut this = Self {
            separator_width: 3.0,
            active_background: colors::GRAY20,
            background: colors::GRAY20,
            button_background: colors::GRAY20,
            selection_color: Color32::from_rgba_unmultiplied(61, 133, 224, 60),
            close_tab_color: colors::GRAYAA,
            tab_color: colors::GRAY30,
            tab_rounding: Rounding::none(),
            active_text_color: colors::WHITE,
            text_color: colors::GRAYAA,
            separator: colors::GRAY20,
            egui: egui::Style::default(),
            dock: egui_dock::Style::default(),
        };

        this.egui = egui::Style {
            spacing: Spacing {
                slider_width: 10.0,
                scroll_bar_width: 10.0,
                scroll_bar_inner_margin: this.separator_width,
                scroll_bar_outer_margin: this.separator_width,
                ..Default::default()
            },
            visuals: Visuals {
                widgets: Widgets {
                    noninteractive: WidgetVisuals {
                        bg_fill: this.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        rounding: this.tab_rounding,
                        bg_stroke: Stroke::NONE,
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: this.active_text_color,
                        },
                        expansion: 0.0,
                    },
                    inactive: WidgetVisuals {
                        bg_fill: this.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        rounding: this.tab_rounding,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: this.text_color,
                        },
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: this.text_color,
                        },
                        expansion: 0.0,
                    },
                    hovered: WidgetVisuals {
                        bg_fill: this.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        rounding: this.tab_rounding,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: this.active_text_color,
                        },
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: this.active_text_color,
                        },
                        expansion: 0.0,
                    },
                    active: WidgetVisuals {
                        bg_fill: this.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        rounding: this.tab_rounding,
                        bg_stroke: Stroke::NONE,
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: this.active_text_color,
                        },
                        expansion: 0.0,
                    },
                    open: WidgetVisuals {
                        bg_fill: this.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        rounding: this.tab_rounding,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: this.active_text_color,
                        },
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: this.active_text_color,
                        },
                        expansion: 0.0,
                    },
                },
                selection: Selection {
                    bg_fill: Color32::from_rgba_unmultiplied(61, 133, 224, 60),
                    stroke: Stroke::NONE,
                },
                menu_rounding: this.tab_rounding,
                dark_mode: true,
                override_text_color: None,
                popup_shadow: Shadow::NONE,
                window_rounding: this.tab_rounding,
                window_fill: this.tab_color,
                panel_fill: this.tab_color,
                extreme_bg_color: Color32::from_rgba_unmultiplied(100, 100, 100, 160),
                ..Default::default()
            },
            wrap: Some(false),
            explanation_tooltips: false,
            text_styles: {
                let mut styles = BTreeMap::new();

                styles.insert(TextStyle::Heading, FontId::new(18.0, FontFamily::Monospace));
                styles.insert(TextStyle::Body, FontId::new(16.0, FontFamily::Monospace));
                styles.insert(TextStyle::Button, FontId::new(14.0, FontFamily::Monospace));
                styles.insert(TextStyle::Small, FontId::new(12.0, FontFamily::Monospace));
                styles
            },
            // debug: egui::style::DebugOptions {
            //     debug_on_hover: true,
            //     show_expand_width: true,
            //     show_expand_height: true,
            //     show_resize: true,
            //     show_interactive_widgets: true,
            //     show_blocking_widget: true
            // },
            ..Default::default()
        };

        this.dock = egui_dock::Style {
            dock_area_padding: None,
            selection_color: this.selection_color,
            border: Stroke::NONE,
            buttons: egui_dock::ButtonsStyle {
                close_tab_color: this.close_tab_color,
                close_tab_active_color: this.close_tab_color,
                close_tab_bg_fill: this.close_tab_color,
                add_tab_bg_fill: this.button_background,
                ..Default::default()
            },
            separator: egui_dock::SeparatorStyle {
                width: this.separator_width,
                color_idle: this.separator,
                color_hovered: this.separator,
                color_dragged: this.separator,
                ..Default::default()
            },
            tab_bar: egui_dock::TabBarStyle {
                bg_fill: this.separator,
                height: 24.0,
                show_scroll_bar_on_overflow: true,
                rounding: this.tab_rounding,
                hline_color: this.separator,
            },
            tabs: egui_dock::TabStyle {
                bg_fill: this.tab_color,
                text_color_focused: this.active_text_color,
                text_color_unfocused: this.text_color,
                text_color_active_focused: this.active_text_color,
                text_color_active_unfocused: this.text_color,
                inner_margin: egui::Margin::same(4.0),
                ..Default::default()
            },
        };

        this
    }
}

impl Style {
    pub fn egui(&self) -> &egui::Style {
        &self.egui
    }

    pub fn dock(&self) -> &egui_dock::Style {
        &self.dock
    }
}
