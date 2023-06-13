use std::collections::BTreeMap;

use egui::epaint::Shadow;
use egui::style::{Interaction, Selection, Spacing, Visuals, WidgetVisuals, Widgets};
use egui::{vec2, Color32, FontFamily, FontId, Margin, Rounding, Stroke, TextStyle};

use tokenizing::colors;

#[derive(Clone)]
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
}

impl Default for Style {
    fn default() -> Self {
        Self {
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
        }
    }
}

impl Style {
    pub fn egui(&self) -> egui::Style {
        egui::Style {
            override_text_style: None,
            override_font_id: None,
            wrap: Some(false),
            spacing: Spacing {
                menu_margin: Margin {
                    top: 4.0,
                    bottom: 4.0,
                    left: 4.0,
                    right: 4.0,
                },
                combo_width: 0.0,
                scroll_bar_inner_margin: 0.0,
                scroll_bar_outer_margin: 0.0,
                scroll_handle_min_length: 15.0,
                item_spacing: vec2(8.0, 3.0),
                window_margin: Margin::same(6.0),
                button_padding: vec2(0.0, 0.0),
                indent: 18.0,
                interact_size: vec2(40.0, 18.0),
                slider_width: 100.0,
                text_edit_width: 280.0,
                icon_width: 14.0,
                icon_width_inner: 8.0,
                icon_spacing: 4.0,
                tooltip_width: 600.0,
                indent_ends_with_horizontal_line: false,
                combo_height: 200.0,
                scroll_bar_width: 10.0,
            },
            interaction: Interaction {
                resize_grab_radius_side: 5.0,
                resize_grab_radius_corner: 10.0,
                show_tooltips_only_when_still: false,
            },
            visuals: Visuals {
                menu_rounding: self.tab_rounding,
                dark_mode: true,
                override_text_color: None,
                widgets: Widgets {
                    noninteractive: WidgetVisuals {
                        bg_fill: self.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        bg_stroke: Stroke::NONE,
                        rounding: self.tab_rounding,
                        fg_stroke: Stroke::NONE,
                        expansion: 0.0,
                    },
                    inactive: WidgetVisuals {
                        bg_fill: self.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: self.text_color,
                        },
                        rounding: self.tab_rounding,
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: self.text_color,
                        },
                        expansion: 0.0,
                    },
                    hovered: WidgetVisuals {
                        bg_fill: self.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: self.active_text_color,
                        },
                        rounding: self.tab_rounding,
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: self.active_text_color,
                        },
                        expansion: 0.0,
                    },
                    active: WidgetVisuals {
                        bg_fill: self.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: self.active_text_color,
                        },
                        rounding: self.tab_rounding,
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: self.active_text_color,
                        },
                        expansion: 0.0,
                    },
                    open: WidgetVisuals {
                        bg_fill: self.tab_color,
                        weak_bg_fill: Color32::TRANSPARENT,
                        bg_stroke: Stroke {
                            width: 5.0,
                            color: self.active_text_color,
                        },
                        rounding: self.tab_rounding,
                        fg_stroke: Stroke {
                            width: 5.0,
                            color: self.active_text_color,
                        },
                        expansion: 0.0,
                    },
                },
                selection: Selection {
                    bg_fill: Color32::from_rgba_unmultiplied(61, 133, 224, 60),
                    stroke: Stroke {
                        width: 1.0,
                        color: Color32::from_rgb(192, 222, 255),
                    },
                },
                button_frame: false,
                popup_shadow: Shadow::NONE,
                clip_rect_margin: 0.0,
                window_rounding: self.tab_rounding,
                window_fill: self.tab_color,
                panel_fill: self.tab_color,
                extreme_bg_color: Color32::from_rgba_unmultiplied(100, 100, 100, 160),
                ..Default::default()
            },
            explanation_tooltips: false,
            text_styles: {
                let mut styles = BTreeMap::new(); 

                styles.insert(TextStyle::Heading, FontId::new(18.0, FontFamily::Monospace));
                styles.insert(TextStyle::Body, FontId::new(16.0, FontFamily::Monospace));
                styles.insert(TextStyle::Button, FontId::new(14.0, FontFamily::Monospace));
                styles.insert(TextStyle::Small, FontId::new(11.0, FontFamily::Monospace));
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
        }
    }

    pub fn dock(&self) -> egui_dock::Style {
        egui_dock::Style {
            dock_area_padding: None,
            selection_color: self.selection_color,
            border: Stroke::NONE,
            buttons: egui_dock::ButtonsStyle {
                close_tab_color: self.close_tab_color,
                close_tab_active_color: self.close_tab_color,
                close_tab_bg_fill: self.close_tab_color,
                add_tab_bg_fill: self.button_background,
                ..Default::default()
            },
            separator: egui_dock::SeparatorStyle {
                width: 3.0,
                color_idle: self.separator,
                color_hovered: self.separator,
                color_dragged: self.separator,
                ..Default::default()
            },
            tab_bar: egui_dock::TabBarStyle {
                bg_fill: self.separator,
                height: 30.0,
                show_scroll_bar_on_overflow: true,
                rounding: self.tab_rounding,
                hline_color: self.separator,
            },
            tabs: egui_dock::TabStyle {
                bg_fill: self.tab_color,
                text_color_focused: self.active_text_color,
                text_color_unfocused: self.text_color,
                text_color_active_focused: self.active_text_color,
                text_color_active_unfocused: self.text_color,
                inner_margin: egui::Margin::same(4.0),
                ..Default::default()
            },
        }
    }
}
