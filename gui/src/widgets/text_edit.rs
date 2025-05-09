#![allow(dead_code)]

use egui::mutex::Mutex;
use egui::output::IMEOutput;
use std::sync::Arc;

use egui::epaint::text::{cursor::*, Galley, LayoutJob};
use egui::text_selection::{CCursorRange, CursorRange};
use egui::{output::OutputEvent, *};

type Undoer = egui::util::undoer::Undoer<(CCursorRange, String)>;

/// The output from a [`TextEdit`](TextEdit).
pub struct TextEditOutput {
    /// The interaction response.
    pub response: egui::Response,

    /// How the text was displayed.
    pub galley: Arc<egui::Galley>,

    /// Where the text in [`Self::galley`] ended up on the screen.
    pub text_draw_pos: egui::Pos2,

    /// The text was clipped to this rectangle when painted.
    pub text_clip_rect: egui::Rect,

    /// The state we stored after the run.
    pub state: TextEditState,

    /// Where the text cursor is.
    pub cursor_range: Option<CursorRange>,
}

/// The text edit state stored between frames.
///
/// Attention: You also need to `store` the updated state.
#[derive(Clone, Default)]
pub struct TextEditState {
    cursor_range: Option<CursorRange>,

    /// This is what is easiest to work with when editing text,
    /// so users are more likely to read/write this.
    ccursor_range: Option<CCursorRange>,

    /// Wrapped in Arc for cheaper clones.
    pub(crate) undoer: Arc<Mutex<Undoer>>,

    // If IME candidate window is shown on this text edit.
    pub(crate) has_ime: bool,

    // Visual offset when editing singleline text bigger than the width.
    pub(crate) singleline_offset: f32,
}

impl TextEditState {
    pub fn load(ctx: &Context, id: Id) -> Option<Self> {
        ctx.data_mut(|d| d.get_persisted(id))
    }

    pub fn store(self, ctx: &Context, id: Id) {
        ctx.data_mut(|d| d.insert_persisted(id, self));
    }

    /// The currently selected range of characters.
    pub fn ccursor_range(&self) -> Option<CCursorRange> {
        self.ccursor_range
            .or_else(|| self.cursor_range.map(|cursor_range| cursor_range.as_ccursor_range()))
    }

    /// Sets the currently selected range of characters.
    pub fn set_ccursor_range(&mut self, ccursor_range: Option<CCursorRange>) {
        self.cursor_range = None;
        self.ccursor_range = ccursor_range;
    }

    pub fn set_cursor_range(&mut self, cursor_range: Option<CursorRange>) {
        self.cursor_range = cursor_range;
        self.ccursor_range = None;
    }

    pub fn cursor_range(&mut self, galley: &Galley) -> Option<CursorRange> {
        self.cursor_range
            .map(|cursor_range| {
                // We only use the PCursor (paragraph number, and character offset within that paragraph).
                // This is so that if we resize the [`TextEdit`] region, and text wrapping changes,
                // we keep the same byte character offset from the beginning of the text,
                // even though the number of rows changes
                // (each paragraph can be several rows, due to word wrapping).
                // The column (character offset) should be able to extend beyond the last word so that we can
                // go down and still end up on the same column when we return.
                CursorRange {
                    primary: galley.from_pcursor(cursor_range.primary.pcursor),
                    secondary: galley.from_pcursor(cursor_range.secondary.pcursor),
                }
            })
            .or_else(|| {
                self.ccursor_range.map(|ccursor_range| CursorRange {
                    primary: galley.from_ccursor(ccursor_range.primary),
                    secondary: galley.from_ccursor(ccursor_range.secondary),
                })
            })
    }
}

/// A text region that the user can edit the contents of.
///
/// See also [`Ui::text_edit_singleline`] and [`Ui::text_edit_multiline`].
///
/// Example:
///
/// ```
/// # egui::__run_test_ui(|ui| {
/// # let mut my_string = String::new();
/// let response = ui.add(egui::TextEdit::singleline(&mut my_string));
/// if response.changed() {
///     // …
/// }
/// if response.lost_focus() && ui.input(|i| i.key_pressed(egui::Key::Enter)) {
///     // …
/// }
/// # });
/// ```
///
/// To fill an [`Ui`] with a [`TextEdit`] use [`Ui::add_sized`]:
///
/// ```
/// # egui::__run_test_ui(|ui| {
/// # let mut my_string = String::new();
/// ui.add_sized(ui.available_size(), egui::TextEdit::multiline(&mut my_string));
/// # });
/// ```
///
///
/// You can also use [`TextEdit`] to show text that can be selected, but not edited.
/// To do so, pass in a `&mut` reference to a `&str`, for instance:
///
/// ```
/// fn selectable_text(ui: &mut egui::Ui, mut text: &str) {
///     ui.add(egui::TextEdit::multiline(&mut text));
/// }
/// ```
///
/// ## Advanced usage
/// See [`TextEdit::show`].
///
/// ## Other
/// The background color of a [`TextEdit`] is [`Visuals::extreme_bg_color`].
#[must_use = "You should put this widget in an ui with `ui.add(widget);`"]
pub struct TextEdit<'t> {
    text: &'t mut dyn TextBuffer,
    hint_text: WidgetText,
    id: Option<Id>,
    id_source: Option<Id>,
    font_selection: FontSelection,
    text_color: Option<Color32>,
    layouter: Option<&'t mut dyn FnMut(&Ui, &str, f32) -> Arc<Galley>>,
    password: bool,
    frame: bool,
    margin: Vec2,
    multiline: bool,
    interactive: bool,
    desired_width: Option<f32>,
    desired_height_rows: usize,
    event_filter: EventFilter,
    cursor_at_end: bool,
    min_size: Vec2,
    align: Align2,
    clip_text: bool,
    char_limit: usize,
}

impl<'t> WidgetWithState for TextEdit<'t> {
    type State = TextEditState;
}

impl<'t> TextEdit<'t> {
    pub fn load_state(ctx: &Context, id: Id) -> Option<TextEditState> {
        TextEditState::load(ctx, id)
    }

    pub fn store_state(ctx: &Context, id: Id, state: TextEditState) {
        state.store(ctx, id);
    }
}

impl<'t> TextEdit<'t> {
    /// No newlines (`\n`) allowed. Pressing enter key will result in the [`TextEdit`] losing focus (`response.lost_focus`).
    pub fn singleline(text: &'t mut dyn TextBuffer) -> Self {
        Self {
            desired_height_rows: 1,
            multiline: false,
            clip_text: true,
            ..Self::multiline(text)
        }
    }

    /// A [`TextEdit`] for multiple lines. Pressing enter key will create a new line.
    pub fn multiline(text: &'t mut dyn TextBuffer) -> Self {
        Self {
            text,
            hint_text: Default::default(),
            id: None,
            id_source: None,
            font_selection: Default::default(),
            text_color: None,
            layouter: None,
            password: false,
            frame: true,
            margin: vec2(4.0, 2.0),
            multiline: true,
            interactive: true,
            desired_width: None,
            desired_height_rows: 4,
            event_filter: EventFilter {
                // moving the cursor is really important
                horizontal_arrows: true,
                vertical_arrows: true,
                tab: false, // tab is used to change focus, not to insert a tab character
                ..Default::default()
            },
            cursor_at_end: true,
            min_size: Vec2::ZERO,
            align: Align2::LEFT_TOP,
            clip_text: false,
            char_limit: usize::MAX,
        }
    }

    /// Build a [`TextEdit`] focused on code editing.
    /// By default it comes with:
    /// - monospaced font
    /// - focus lock (tab will insert a tab character instead of moving focus)
    pub fn code_editor(self) -> Self {
        self.font(TextStyle::Monospace).lock_focus(true)
    }

    /// Use if you want to set an explicit [`Id`] for this widget.
    #[inline]
    pub fn id(mut self, id: Id) -> Self {
        self.id = Some(id);
        self
    }

    /// A source for the unique [`Id`], e.g. `.id_source("second_text_edit_field")` or `.id_source(loop_index)`.
    #[inline]
    pub fn id_source(mut self, id_source: impl std::hash::Hash) -> Self {
        self.id_source = Some(Id::new(id_source));
        self
    }

    /// Show a faint hint text when the text field is empty.
    #[inline]
    pub fn hint_text(mut self, hint_text: impl Into<WidgetText>) -> Self {
        self.hint_text = hint_text.into();
        self
    }

    /// If true, hide the letters from view and prevent copying from the field.
    #[inline]
    pub fn password(mut self, password: bool) -> Self {
        self.password = password;
        self
    }

    /// Pick a [`FontId`] or [`TextStyle`].
    #[inline]
    pub fn font(mut self, font_selection: impl Into<FontSelection>) -> Self {
        self.font_selection = font_selection.into();
        self
    }

    #[inline]
    pub fn text_color(mut self, text_color: Color32) -> Self {
        self.text_color = Some(text_color);
        self
    }

    #[inline]
    pub fn text_color_opt(mut self, text_color: Option<Color32>) -> Self {
        self.text_color = text_color;
        self
    }

    /// Override how text is being shown inside the [`TextEdit`].
    ///
    /// This can be used to implement things like syntax highlighting.
    ///
    /// This function will be called at least once per frame,
    /// so it is strongly suggested that you cache the results of any syntax highlighter
    /// so as not to waste CPU highlighting the same string every frame.
    ///
    /// The arguments is the enclosing [`Ui`] (so you can access e.g. [`Ui::fonts`]),
    /// the text and the wrap width.
    ///
    /// ```
    /// # egui::__run_test_ui(|ui| {
    /// # let mut my_code = String::new();
    /// # fn my_memoized_highlighter(s: &str) -> egui::text::LayoutJob { Default::default() }
    /// let mut layouter = |ui: &egui::Ui, string: &str, wrap_width: f32| {
    ///     let mut layout_job: egui::text::LayoutJob = my_memoized_highlighter(string);
    ///     layout_job.wrap.max_width = wrap_width;
    ///     ui.fonts(|f| f.layout_job(layout_job))
    /// };
    /// ui.add(egui::TextEdit::multiline(&mut my_code).layouter(&mut layouter));
    /// # });
    /// ```
    #[inline]
    pub fn layouter(mut self, layouter: &'t mut dyn FnMut(&Ui, &str, f32) -> Arc<Galley>) -> Self {
        self.layouter = Some(layouter);

        self
    }

    /// Default is `true`. If set to `false` then you cannot interact with the text (neither edit or select it).
    ///
    /// Consider using [`Ui::add_enabled`] instead to also give the [`TextEdit`] a greyed out look.
    #[inline]
    pub fn interactive(mut self, interactive: bool) -> Self {
        self.interactive = interactive;
        self
    }

    /// Default is `true`. If set to `false` there will be no frame showing that this is editable text!
    #[inline]
    pub fn frame(mut self, frame: bool) -> Self {
        self.frame = frame;
        self
    }

    /// Set margin of text. Default is [4.0,2.0]
    #[inline]
    pub fn margin(mut self, margin: Vec2) -> Self {
        self.margin = margin;
        self
    }

    /// Set to 0.0 to keep as small as possible.
    /// Set to [`f32::INFINITY`] to take up all available space (i.e. disable automatic word wrap).
    #[inline]
    pub fn desired_width(mut self, desired_width: f32) -> Self {
        self.desired_width = Some(desired_width);
        self
    }

    /// Set the number of rows to show by default.
    /// The default for singleline text is `1`.
    /// The default for multiline text is `4`.
    #[inline]
    pub fn desired_rows(mut self, desired_height_rows: usize) -> Self {
        self.desired_height_rows = desired_height_rows;
        self
    }

    /// When `false` (default), pressing TAB will move focus
    /// to the next widget.
    ///
    /// When `true`, the widget will keep the focus and pressing TAB
    /// will insert the `'\t'` character.
    #[inline]
    pub fn lock_focus(mut self, tab_will_indent: bool) -> Self {
        self.event_filter.tab = tab_will_indent;
        self
    }

    /// When `true` (default), the cursor will initially be placed at the end of the text.
    ///
    /// When `false`, the cursor will initially be placed at the beginning of the text.
    #[inline]
    pub fn cursor_at_end(mut self, b: bool) -> Self {
        self.cursor_at_end = b;
        self
    }

    /// When `true` (default), overflowing text will be clipped.
    ///
    /// When `false`, widget width will expand to make all text visible.
    ///
    /// This only works for singleline [`TextEdit`].
    #[inline]
    pub fn clip_text(mut self, b: bool) -> Self {
        // always show everything in multiline
        if !self.multiline {
            self.clip_text = b;
        }
        self
    }

    /// Sets the limit for the amount of characters can be entered
    ///
    /// This only works for singleline [`TextEdit`]
    #[inline]
    pub fn char_limit(mut self, limit: usize) -> Self {
        self.char_limit = limit;
        self
    }

    /// Set the horizontal align of the inner text.
    #[inline]
    pub fn horizontal_align(mut self, align: Align) -> Self {
        self.align.0[0] = align;
        self
    }

    /// Set the vertical align of the inner text.
    #[inline]
    pub fn vertical_align(mut self, align: Align) -> Self {
        self.align.0[1] = align;
        self
    }

    /// Set the minimum size of the [`TextEdit`].
    #[inline]
    pub fn min_size(mut self, min_size: Vec2) -> Self {
        self.min_size = min_size;
        self
    }
}

// ----------------------------------------------------------------------------

impl<'t> Widget for TextEdit<'t> {
    fn ui(self, ui: &mut Ui) -> Response {
        self.show(ui).response
    }
}

impl<'t> TextEdit<'t> {
    /// Show the [`TextEdit`], returning a rich [`TextEditOutput`].
    ///
    /// ```
    /// # egui::__run_test_ui(|ui| {
    /// # let mut my_string = String::new();
    /// let output = egui::TextEdit::singleline(&mut my_string).show(ui);
    /// if let Some(text_cursor_range) = output.cursor_range {
    ///     use egui::TextBuffer as _;
    ///     let selected_chars = text_cursor_range.as_sorted_char_range();
    ///     let selected_text = my_string.char_range(selected_chars);
    ///     ui.label("Selected text: ");
    ///     ui.monospace(selected_text);
    /// }
    /// # });
    /// ```
    pub fn show(self, ui: &mut Ui) -> TextEditOutput {
        let is_mutable = self.text.is_mutable();
        let frame = self.frame;
        let interactive = self.interactive;
        let where_to_put_background = ui.painter().add(Shape::Noop);

        let margin = self.margin;
        let max_rect = ui.available_rect_before_wrap().shrink2(margin);
        let mut content_ui = ui.child_ui(max_rect, *ui.layout());

        let mut output = self.show_content(&mut content_ui);

        let id = output.response.id;
        let frame_rect = output.response.rect.expand2(margin);
        ui.allocate_space(frame_rect.size());
        if interactive {
            output.response |= ui.interact(frame_rect, id, Sense::click());
        }
        if output.response.clicked() && !output.response.lost_focus() {
            ui.memory_mut(|mem| mem.request_focus(output.response.id));
        }

        if frame {
            let visuals = ui.style().interact(&output.response);
            let frame_rect = frame_rect.expand(visuals.expansion);
            let shape = if is_mutable {
                if output.response.has_focus() {
                    epaint::RectShape::new(
                        frame_rect,
                        visuals.rounding,
                        ui.visuals().extreme_bg_color,
                        ui.visuals().selection.stroke,
                    )
                } else {
                    epaint::RectShape::new(
                        frame_rect,
                        visuals.rounding,
                        ui.visuals().extreme_bg_color,
                        visuals.bg_stroke, // TODO(emilk): we want to show something here, or a text-edit field doesn't "pop".
                    )
                }
            } else {
                let visuals = &ui.style().visuals.widgets.inactive;
                epaint::RectShape::stroke(
                    frame_rect,
                    visuals.rounding,
                    visuals.bg_stroke, // TODO(emilk): we want to show something here, or a text-edit field doesn't "pop".
                )
            };

            ui.painter().set(where_to_put_background, shape);
        }

        output
    }

    fn show_content(self, ui: &mut Ui) -> TextEditOutput {
        let TextEdit {
            text,
            hint_text,
            id,
            id_source,
            font_selection,
            text_color,
            layouter,
            password,
            frame: _,
            margin,
            multiline,
            interactive,
            desired_width,
            desired_height_rows,
            event_filter,
            cursor_at_end,
            min_size,
            align,
            clip_text,
            char_limit,
        } = self;

        let text_color = text_color
            .or(ui.visuals().override_text_color)
            // .unwrap_or_else(|| ui.style().interact(&response).text_color()); // too bright
            .unwrap_or_else(|| ui.visuals().widgets.inactive.text_color());

        let prev_text = text.as_str().to_owned();

        let font_id = font_selection.resolve(ui.style());
        let row_height = ui.fonts(|f| f.row_height(&font_id));
        const MIN_WIDTH: f32 = 24.0; // Never make a [`TextEdit`] more narrow than this.
        let available_width = ui.available_width().at_least(MIN_WIDTH);
        let desired_width = desired_width.unwrap_or_else(|| ui.spacing().text_edit_width);
        let wrap_width = if ui.layout().horizontal_justify() {
            available_width
        } else {
            desired_width.min(available_width)
        } - margin.x * 2.0;

        let font_id_clone = font_id.clone();
        let mut default_layouter = move |ui: &Ui, text: &str, wrap_width: f32| {
            let text = mask_if_password(password, text);
            let layout_job = if multiline {
                LayoutJob::simple(text, font_id_clone.clone(), text_color, wrap_width)
            } else {
                LayoutJob::simple_singleline(text, font_id_clone.clone(), text_color)
            };
            ui.fonts(|f| f.layout_job(layout_job))
        };

        let layouter = layouter.unwrap_or(&mut default_layouter);

        let mut galley = layouter(ui, text.as_str(), wrap_width);

        let desired_width = if clip_text {
            wrap_width // visual clipping with scroll in singleline input.
        } else {
            galley.size().x.max(wrap_width)
        };
        let desired_height = (desired_height_rows.at_least(1) as f32) * row_height;
        let desired_size = vec2(desired_width, galley.size().y.max(desired_height))
            .at_least(min_size - margin * 2.0);

        let (auto_id, rect) = ui.allocate_space(desired_size);

        let id = id.unwrap_or_else(|| {
            if let Some(id_source) = id_source {
                ui.make_persistent_id(id_source)
            } else {
                auto_id // Since we are only storing the cursor a persistent Id is not super important
            }
        });
        let mut state = TextEditState::load(ui.ctx(), id).unwrap_or_default();

        // On touch screens (e.g. mobile in `eframe` web), should
        // dragging select text, or scroll the enclosing [`ScrollArea`] (if any)?
        // Since currently copying selected text in not supported on `eframe` web,
        // we prioritize touch-scrolling:
        let allow_drag_to_select =
            ui.input(|i| !i.any_touches()) || ui.memory(|mem| mem.has_focus(id));

        let sense = if interactive {
            if allow_drag_to_select {
                Sense::click_and_drag()
            } else {
                Sense::click()
            }
        } else {
            Sense::hover()
        };
        let mut response = ui.interact(rect, id, sense);
        let text_clip_rect = rect;
        let painter = ui.painter_at(text_clip_rect.expand(1.0)); // expand to avoid clipping cursor

        if interactive {
            if let Some(pointer_pos) = ui.ctx().pointer_interact_pos() {
                if response.hovered() && text.is_mutable() {
                    ui.output_mut(|o| o.mutable_text_under_cursor = true);
                }

                // TODO(emilk): drag selected text to either move or clone (ctrl on windows, alt on mac)
                let singleline_offset = vec2(state.singleline_offset, 0.0);
                let cursor_at_pointer =
                    galley.cursor_from_pos(pointer_pos - response.rect.min + singleline_offset);

                if ui.visuals().text_cursor_preview
                    && response.hovered()
                    && ui.input(|i| i.pointer.is_moving())
                {
                    // preview:
                    paint_cursor_end(
                        ui,
                        row_height,
                        &painter,
                        response.rect.min,
                        &galley,
                        &cursor_at_pointer,
                    );
                }

                if response.double_clicked() {
                    // Select word:
                    let center = cursor_at_pointer;
                    let ccursor_range = select_word_at(text.as_str(), center.ccursor);
                    state.set_cursor_range(Some(CursorRange {
                        primary: galley.from_ccursor(ccursor_range.primary),
                        secondary: galley.from_ccursor(ccursor_range.secondary),
                    }));
                } else if response.triple_clicked() {
                    // Select line:
                    let center = cursor_at_pointer;
                    let ccursor_range = select_line_at(text.as_str(), center.ccursor);
                    state.set_cursor_range(Some(CursorRange {
                        primary: galley.from_ccursor(ccursor_range.primary),
                        secondary: galley.from_ccursor(ccursor_range.secondary),
                    }));
                } else if allow_drag_to_select {
                    if response.hovered() && ui.input(|i| i.pointer.any_pressed()) {
                        ui.memory_mut(|mem| mem.request_focus(id));
                        if ui.input(|i| i.modifiers.shift) {
                            if let Some(mut cursor_range) = state.cursor_range(&galley) {
                                cursor_range.primary = cursor_at_pointer;
                                state.set_cursor_range(Some(cursor_range));
                            } else {
                                state.set_cursor_range(Some(CursorRange::one(cursor_at_pointer)));
                            }
                        } else {
                            state.set_cursor_range(Some(CursorRange::one(cursor_at_pointer)));
                        }
                    } else if ui.input(|i| i.pointer.any_down())
                        && response.is_pointer_button_down_on()
                    {
                        // drag to select text:
                        if let Some(mut cursor_range) = state.cursor_range(&galley) {
                            cursor_range.primary = cursor_at_pointer;
                            state.set_cursor_range(Some(cursor_range));
                        }
                    }
                }
            }
        }

        if interactive && response.hovered() {
            ui.ctx().set_cursor_icon(CursorIcon::Text);
        }

        let mut cursor_range = None;
        let prev_cursor_range = state.cursor_range(&galley);
        if interactive && ui.memory(|mem| mem.has_focus(id)) {
            ui.memory_mut(|mem| mem.set_focus_lock_filter(id, event_filter));

            let default_cursor_range = if cursor_at_end {
                CursorRange::one(galley.end())
            } else {
                CursorRange::default()
            };

            let (changed, new_cursor_range) = events(
                ui,
                &mut state,
                text,
                &mut galley,
                layouter,
                id,
                wrap_width,
                multiline,
                password,
                default_cursor_range,
                char_limit,
                event_filter,
            );

            if changed {
                response.mark_changed();
            }
            cursor_range = Some(new_cursor_range);
        }

        let mut text_draw_pos = align
            .align_size_within_rect(galley.size(), response.rect)
            .intersect(response.rect) // limit pos to the response rect area
            .min;
        let align_offset = response.rect.left() - text_draw_pos.x;

        // Visual clipping for singleline text editor with text larger than width
        if clip_text && align_offset == 0.0 {
            let cursor_pos = match (cursor_range, ui.memory(|mem| mem.has_focus(id))) {
                (Some(cursor_range), true) => galley.pos_from_cursor(&cursor_range.primary).min.x,
                _ => 0.0,
            };

            let mut offset_x = state.singleline_offset;
            let visible_range = offset_x..=offset_x + desired_size.x;

            if !visible_range.contains(&cursor_pos) {
                if cursor_pos < *visible_range.start() {
                    offset_x = cursor_pos;
                } else {
                    offset_x = cursor_pos - desired_size.x;
                }
            }

            offset_x = offset_x.at_most(galley.size().x - desired_size.x).at_least(0.0);

            state.singleline_offset = offset_x;
            text_draw_pos -= vec2(offset_x, 0.0);
        } else {
            state.singleline_offset = align_offset;
        }

        let selection_changed = if let (Some(cursor_range), Some(prev_cursor_range)) =
            (cursor_range, prev_cursor_range)
        {
            prev_cursor_range.as_ccursor_range() != cursor_range.as_ccursor_range()
        } else {
            false
        };

        if ui.is_rect_visible(rect) {
            painter.galley(text_draw_pos, galley.clone(), text_color);

            if text.as_str().is_empty() && !hint_text.is_empty() {
                let hint_text_color = ui.visuals().weak_text_color();
                let galley = if multiline {
                    hint_text.into_galley(ui, Some(true), desired_size.x, font_id)
                } else {
                    hint_text.into_galley(ui, Some(false), f32::INFINITY, font_id)
                };
                painter.galley(response.rect.min, galley, hint_text_color);
            }

            if ui.memory(|mem| mem.has_focus(id)) {
                if let Some(cursor_range) = state.cursor_range(&galley) {
                    // We paint the cursor on top of the text, in case
                    // the text galley has backgrounds (as e.g. `code` snippets in markup do).
                    paint_cursor_selection(ui, &painter, text_draw_pos, &galley, &cursor_range);

                    if text.is_mutable() {
                        let cursor_rect = paint_cursor_end(
                            ui,
                            row_height,
                            &painter,
                            text_draw_pos,
                            &galley,
                            &cursor_range.primary,
                        );

                        // TODO: remove this HACK workaround for
                        // https://github.com/emilk/egui/issues/1531
                        let is_fully_visible = ui.clip_rect().contains_rect(rect);
                        if (response.changed || selection_changed) && !is_fully_visible {
                            ui.scroll_to_rect(cursor_rect, None); // keep cursor in view
                        }

                        if interactive {
                            // For IME, so only set it when text is editable and visible!
                            ui.ctx().output_mut(|o| {
                                o.ime = Some(IMEOutput { rect, cursor_rect });
                            });
                        }
                    }
                }
            }
        }

        state.clone().store(ui.ctx(), id);

        if response.changed {
            response.widget_info(|| {
                WidgetInfo::text_edit(
                    mask_if_password(password, prev_text.as_str()),
                    mask_if_password(password, text.as_str()),
                )
            });
        } else if selection_changed {
            let cursor_range = cursor_range.unwrap();
            let char_range =
                cursor_range.primary.ccursor.index..=cursor_range.secondary.ccursor.index;
            let info = WidgetInfo::text_selection_changed(
                char_range,
                mask_if_password(password, text.as_str()),
            );
            response.output_event(OutputEvent::TextSelectionChanged(info));
        } else {
            response.widget_info(|| {
                WidgetInfo::text_edit(
                    mask_if_password(password, prev_text.as_str()),
                    mask_if_password(password, text.as_str()),
                )
            });
        }

        TextEditOutput {
            response,
            galley,
            text_draw_pos,
            text_clip_rect,
            state,
            cursor_range,
        }
    }
}

fn mask_if_password(is_password: bool, text: &str) -> String {
    fn mask_password(text: &str) -> String {
        std::iter::repeat(epaint::text::PASSWORD_REPLACEMENT_CHAR)
            .take(text.chars().count())
            .collect::<String>()
    }

    if is_password {
        mask_password(text)
    } else {
        text.to_owned()
    }
}

// ----------------------------------------------------------------------------

/// Check for (keyboard) events to edit the cursor and/or text.
#[allow(clippy::too_many_arguments)]
fn events(
    ui: &egui::Ui,
    state: &mut TextEditState,
    text: &mut dyn TextBuffer,
    galley: &mut Arc<Galley>,
    layouter: &mut dyn FnMut(&Ui, &str, f32) -> Arc<Galley>,
    id: Id,
    wrap_width: f32,
    multiline: bool,
    password: bool,
    default_cursor_range: CursorRange,
    char_limit: usize,
    event_filter: EventFilter,
) -> (bool, CursorRange) {
    let mut cursor_range = state.cursor_range(galley).unwrap_or(default_cursor_range);

    // We feed state to the undoer both before and after handling input
    // so that the undoer creates automatic saves even when there are no events for a while.
    state.undoer.lock().feed_state(
        ui.input(|i| i.time),
        &(cursor_range.as_ccursor_range(), text.as_str().to_owned()),
    );

    let copy_if_not_password = |ui: &Ui, text: String| {
        if !password {
            ui.ctx().copy_text(text);
        }
    };

    let mut any_change = false;

    let events = ui.input(|i| i.filtered_events(&event_filter));
    for event in &events {
        let did_mutate_text = match event {
            Event::Copy => {
                str_strip_ending_newline(text, &mut cursor_range);
                copy_if_not_password(ui, selected_str(text, &cursor_range).to_owned());
                None
            }
            Event::Cut => {
                str_strip_ending_newline(text, &mut cursor_range);
                copy_if_not_password(ui, selected_str(text, &cursor_range).to_owned());
                Some(CCursorRange::one(delete_selected(text, &cursor_range)))
            }
            Event::Paste(text_to_insert) => {
                if !text_to_insert.is_empty() {
                    let mut ccursor = delete_selected(text, &cursor_range);

                    insert_text(&mut ccursor, text, text_to_insert, char_limit);

                    Some(CCursorRange::one(ccursor))
                } else {
                    None
                }
            }
            Event::Text(text_to_insert) => {
                // Newlines are handled by `Key::Enter`.
                if !text_to_insert.is_empty() && text_to_insert != "\n" && text_to_insert != "\r" {
                    let mut ccursor = delete_selected(text, &cursor_range);

                    insert_text(&mut ccursor, text, text_to_insert, char_limit);

                    Some(CCursorRange::one(ccursor))
                } else {
                    None
                }
            }
            Event::Key {
                key: Key::Tab,
                pressed: true,
                modifiers,
                ..
            } if multiline => {
                let mut ccursor = delete_selected(text, &cursor_range);
                if modifiers.shift {
                    // TODO(emilk): support removing indentation over a selection?
                    decrease_indentation(&mut ccursor, text);
                } else {
                    insert_text(&mut ccursor, text, "\t", char_limit);
                }
                Some(CCursorRange::one(ccursor))
            }
            Event::Key {
                key: Key::Enter,
                pressed: true,
                ..
            } => {
                if multiline {
                    let mut ccursor = delete_selected(text, &cursor_range);
                    insert_text(&mut ccursor, text, "\n", char_limit);
                    // TODO(emilk): if code editor, auto-indent by same leading tabs, + one if the lines end on an opening bracket
                    Some(CCursorRange::one(ccursor))
                } else {
                    ui.memory_mut(|mem| mem.surrender_focus(id)); // End input with enter
                    break;
                }
            }
            Event::Key {
                key: Key::Z,
                pressed: true,
                modifiers,
                ..
            } if modifiers.matches_logically(Modifiers::COMMAND) => {
                if let Some((undo_ccursor_range, undo_txt)) = state
                    .undoer
                    .lock()
                    .undo(&(cursor_range.as_ccursor_range(), text.as_str().to_owned()))
                {
                    text.replace_with(undo_txt);
                    Some(*undo_ccursor_range)
                } else {
                    None
                }
            }
            Event::Key {
                key,
                pressed: true,
                modifiers,
                ..
            } if (modifiers.matches_logically(Modifiers::COMMAND) && *key == Key::Y)
                || (modifiers.matches_logically(Modifiers::SHIFT | Modifiers::COMMAND)
                    && *key == Key::Z) =>
            {
                if let Some((redo_ccursor_range, redo_txt)) = state
                    .undoer
                    .lock()
                    .redo(&(cursor_range.as_ccursor_range(), text.as_str().to_owned()))
                {
                    text.replace_with(redo_txt);
                    Some(*redo_ccursor_range)
                } else {
                    None
                }
            }

            Event::Key {
                key,
                pressed: true,
                modifiers,
                ..
            } => on_key_press(&mut cursor_range, text, galley, *key, modifiers),

            Event::CompositionStart => {
                state.has_ime = true;
                None
            }

            Event::CompositionUpdate(text_mark) => {
                // empty prediction can be produced when user press backspace
                // or escape during ime. We should clear current text.
                if text_mark != "\n" && text_mark != "\r" && state.has_ime {
                    let mut ccursor = delete_selected(text, &cursor_range);
                    let start_cursor = ccursor;
                    if !text_mark.is_empty() {
                        insert_text(&mut ccursor, text, text_mark, char_limit);
                    }
                    Some(CCursorRange::two(start_cursor, ccursor))
                } else {
                    None
                }
            }

            Event::CompositionEnd(prediction) => {
                // CompositionEnd only characters may be typed into TextEdit without trigger CompositionStart first, so do not check `state.has_ime = true` in the following statement.
                if prediction != "\n" && prediction != "\r" {
                    state.has_ime = false;
                    let mut ccursor = delete_selected(text, &cursor_range);
                    if !prediction.is_empty() {
                        insert_text(&mut ccursor, text, prediction, char_limit);
                    }
                    Some(CCursorRange::one(ccursor))
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(new_ccursor_range) = did_mutate_text {
            any_change = true;

            // Layout again to avoid frame delay, and to keep `text` and `galley` in sync.
            *galley = layouter(ui, text.as_str(), wrap_width);

            // Set cursor_range using new galley:
            cursor_range = CursorRange {
                primary: galley.from_ccursor(new_ccursor_range.primary),
                secondary: galley.from_ccursor(new_ccursor_range.secondary),
            };
        }
    }

    state.set_cursor_range(Some(cursor_range));

    state.undoer.lock().feed_state(
        ui.input(|i| i.time),
        &(cursor_range.as_ccursor_range(), text.as_str().to_owned()),
    );

    (any_change, cursor_range)
}

// ----------------------------------------------------------------------------

fn paint_cursor_selection(
    ui: &Ui,
    painter: &Painter,
    pos: Pos2,
    galley: &Galley,
    cursor_range: &CursorRange,
) {
    if cursor_range.is_empty() {
        return;
    }

    // We paint the cursor selection on top of the text, so make it transparent:
    let color = ui.visuals().selection.bg_fill.linear_multiply(0.5);
    let [min, max] = cursor_range.sorted_cursors();
    let min = min.rcursor;
    let max = max.rcursor;

    for ri in min.row..=max.row {
        let row = &galley.rows[ri];
        let left = if ri == min.row {
            row.x_offset(min.column)
        } else {
            row.rect.left()
        };
        let right = if ri == max.row {
            row.x_offset(max.column)
        } else {
            let newline_size = if row.ends_with_newline {
                row.height() / 2.0 // visualize that we select the newline
            } else {
                0.0
            };
            row.rect.right() + newline_size
        };
        let rect = Rect::from_min_max(
            pos + vec2(left, row.min_y()),
            pos + vec2(right, row.max_y()),
        );
        painter.rect_filled(rect, 0.0, color);
    }
}

fn paint_cursor_end(
    ui: &Ui,
    row_height: f32,
    painter: &Painter,
    pos: Pos2,
    galley: &Galley,
    cursor: &Cursor,
) -> Rect {
    let stroke = ui.visuals().text_cursor;

    let cursor_pos = galley.pos_from_cursor(cursor).translate(pos.to_vec2());
    let cursor_height = row_height;
    let cursor_width = cursor_height * 0.5;

    // Create a rectangle that represents the block cursor.
    // It covers the width and height of a character at the cursor position.
    let cursor_rect = Rect::from_min_size(cursor_pos.min, vec2(cursor_width, cursor_height));

    // Draw the block cursor as a filled rectangle.
    painter.rect_filled(cursor_rect, 0.0, stroke.color);

    cursor_rect
}

// ----------------------------------------------------------------------------

fn str_strip_ending_newline(text: &dyn TextBuffer, cursor_range: &mut CursorRange) {
    let [_, max] = cursor_range.sorted_cursors();

    // If the cursor ends in a newline.
    if text.char_range(max.ccursor.index..max.ccursor.index + 1) == "\n" {
        // strip the character
        if cursor_range.is_sorted() {
            cursor_range.secondary.ccursor.index -= 1;
        } else {
            cursor_range.primary.ccursor.index -= 1;
        }
    }
}

fn selected_str<'s>(text: &'s dyn TextBuffer, cursor_range: &CursorRange) -> &'s str {
    let [min, max] = cursor_range.sorted_cursors();
    // Add one to account for the block cursor.
    text.char_range(min.ccursor.index..max.ccursor.index + 1)
}

fn insert_text(
    ccursor: &mut CCursor,
    text: &mut dyn TextBuffer,
    text_to_insert: &str,
    char_limit: usize,
) {
    if char_limit < usize::MAX {
        let mut new_string = text_to_insert;
        // Avoid subtract with overflow panic
        let cutoff = char_limit.saturating_sub(text.as_str().chars().count());

        new_string = match new_string.char_indices().nth(cutoff) {
            None => new_string,
            Some((idx, _)) => &new_string[..idx],
        };

        ccursor.index += text.insert_text(new_string, ccursor.index);
    } else {
        ccursor.index += text.insert_text(text_to_insert, ccursor.index);
    }
}

// ----------------------------------------------------------------------------

fn delete_selected(text: &mut dyn TextBuffer, cursor_range: &CursorRange) -> CCursor {
    let [min, max] = cursor_range.sorted_cursors();
    delete_selected_ccursor_range(text, [min.ccursor, max.ccursor])
}

fn delete_selected_ccursor_range(text: &mut dyn TextBuffer, [min, max]: [CCursor; 2]) -> CCursor {
    text.delete_char_range(min.index..max.index);
    CCursor {
        index: min.index,
        prefer_next_row: true,
    }
}

fn delete_previous_char(text: &mut dyn TextBuffer, ccursor: CCursor) -> CCursor {
    if ccursor.index > 0 {
        let max_ccursor = ccursor;
        let min_ccursor = max_ccursor - 1;
        delete_selected_ccursor_range(text, [min_ccursor, max_ccursor])
    } else {
        ccursor
    }
}

fn delete_next_char(text: &mut dyn TextBuffer, ccursor: CCursor) -> CCursor {
    delete_selected_ccursor_range(text, [ccursor, ccursor + 1])
}

fn delete_previous_word(text: &mut dyn TextBuffer, max_ccursor: CCursor) -> CCursor {
    let min_ccursor = ccursor_previous_word(text.as_str(), max_ccursor);
    delete_selected_ccursor_range(text, [min_ccursor, max_ccursor])
}

fn delete_next_word(text: &mut dyn TextBuffer, min_ccursor: CCursor) -> CCursor {
    let max_ccursor = ccursor_next_word(text.as_str(), min_ccursor);
    delete_selected_ccursor_range(text, [min_ccursor, max_ccursor])
}

fn delete_paragraph_before_cursor(
    text: &mut dyn TextBuffer,
    galley: &Galley,
    cursor_range: &CursorRange,
) -> CCursor {
    let [min, max] = cursor_range.sorted_cursors();
    let min = galley.from_pcursor(PCursor {
        paragraph: min.pcursor.paragraph,
        offset: 0,
        prefer_next_row: true,
    });
    if min.ccursor == max.ccursor {
        delete_previous_char(text, min.ccursor)
    } else {
        delete_selected(text, &CursorRange::two(min, max))
    }
}

fn delete_paragraph_after_cursor(
    text: &mut dyn TextBuffer,
    galley: &Galley,
    cursor_range: &CursorRange,
) -> CCursor {
    let [min, max] = cursor_range.sorted_cursors();
    let max = galley.from_pcursor(PCursor {
        paragraph: max.pcursor.paragraph,
        offset: usize::MAX, // end of paragraph
        prefer_next_row: false,
    });
    if min.ccursor == max.ccursor {
        delete_next_char(text, min.ccursor)
    } else {
        delete_selected(text, &CursorRange::two(min, max))
    }
}

// ----------------------------------------------------------------------------

/// Returns `Some(new_cursor)` if we did mutate `text`.
fn on_key_press(
    cursor_range: &mut CursorRange,
    text: &mut dyn TextBuffer,
    galley: &Galley,
    key: Key,
    modifiers: &Modifiers,
) -> Option<CCursorRange> {
    match key {
        Key::Backspace => {
            let ccursor = if modifiers.mac_cmd {
                delete_paragraph_before_cursor(text, galley, cursor_range)
            } else if let Some(cursor) = cursor_range.single() {
                if modifiers.alt || modifiers.ctrl {
                    // alt on mac, ctrl on windows
                    delete_previous_word(text, cursor.ccursor)
                } else {
                    delete_previous_char(text, cursor.ccursor)
                }
            } else {
                delete_selected(text, cursor_range)
            };
            Some(CCursorRange::one(ccursor))
        }
        Key::Delete if !modifiers.shift || !cfg!(target_os = "windows") => {
            let ccursor = if modifiers.mac_cmd {
                delete_paragraph_after_cursor(text, galley, cursor_range)
            } else if let Some(cursor) = cursor_range.single() {
                if modifiers.alt || modifiers.ctrl {
                    // alt on mac, ctrl on windows
                    delete_next_word(text, cursor.ccursor)
                } else {
                    delete_next_char(text, cursor.ccursor)
                }
            } else {
                delete_selected(text, cursor_range)
            };
            let ccursor = CCursor {
                prefer_next_row: true,
                ..ccursor
            };
            Some(CCursorRange::one(ccursor))
        }

        Key::A if modifiers.command => {
            // select all
            *cursor_range = CursorRange::two(Cursor::default(), galley.end());
            None
        }

        Key::H if modifiers.ctrl => {
            let ccursor = delete_previous_char(text, cursor_range.primary.ccursor);
            Some(CCursorRange::one(ccursor))
        }

        Key::K if modifiers.ctrl => {
            let ccursor = delete_paragraph_after_cursor(text, galley, cursor_range);
            Some(CCursorRange::one(ccursor))
        }

        Key::U if modifiers.ctrl => {
            let ccursor = delete_paragraph_before_cursor(text, galley, cursor_range);
            Some(CCursorRange::one(ccursor))
        }

        Key::W if modifiers.ctrl => {
            let ccursor = if let Some(cursor) = cursor_range.single() {
                delete_previous_word(text, cursor.ccursor)
            } else {
                delete_selected(text, cursor_range)
            };
            Some(CCursorRange::one(ccursor))
        }

        Key::ArrowLeft | Key::ArrowRight if modifiers.is_none() && !cursor_range.is_empty() => {
            if key == Key::ArrowLeft {
                *cursor_range = CursorRange::one(cursor_range.sorted_cursors()[0]);
            } else {
                *cursor_range = CursorRange::one(cursor_range.sorted_cursors()[1]);
            }
            None
        }

        Key::ArrowLeft | Key::ArrowRight | Key::ArrowUp | Key::ArrowDown | Key::Home | Key::End => {
            move_single_cursor(&mut cursor_range.primary, galley, key, modifiers);
            if !modifiers.shift {
                cursor_range.secondary = cursor_range.primary;
            }
            None
        }

        Key::P | Key::N | Key::B | Key::F | Key::A | Key::E
            if cfg!(target_os = "macos") && modifiers.ctrl && !modifiers.shift =>
        {
            move_single_cursor(&mut cursor_range.primary, galley, key, modifiers);
            cursor_range.secondary = cursor_range.primary;
            None
        }

        _ => None,
    }
}

fn move_single_cursor(cursor: &mut Cursor, galley: &Galley, key: Key, modifiers: &Modifiers) {
    if cfg!(target_os = "macos") && modifiers.ctrl && !modifiers.shift {
        match key {
            Key::A => *cursor = galley.cursor_begin_of_row(cursor),
            Key::E => *cursor = galley.cursor_end_of_row(cursor),
            Key::P => *cursor = galley.cursor_up_one_row(cursor),
            Key::N => *cursor = galley.cursor_down_one_row(cursor),
            Key::B => *cursor = galley.cursor_left_one_character(cursor),
            Key::F => *cursor = galley.cursor_right_one_character(cursor),
            _ => (),
        }
        return;
    }
    match key {
        Key::ArrowLeft => {
            if modifiers.alt || modifiers.ctrl {
                // alt on mac, ctrl on windows
                *cursor = galley.from_ccursor(ccursor_previous_word(galley.text(), cursor.ccursor));
            } else if modifiers.mac_cmd {
                *cursor = galley.cursor_begin_of_row(cursor);
            } else {
                *cursor = galley.cursor_left_one_character(cursor);
            }
        }
        Key::ArrowRight => {
            if modifiers.alt || modifiers.ctrl {
                // alt on mac, ctrl on windows
                *cursor = galley.from_ccursor(ccursor_next_word(galley.text(), cursor.ccursor));
            } else if modifiers.mac_cmd {
                *cursor = galley.cursor_end_of_row(cursor);
            } else {
                *cursor = galley.cursor_right_one_character(cursor);
            }
        }
        Key::ArrowUp => {
            if modifiers.command {
                // mac and windows behavior
                *cursor = Cursor::default();
            } else {
                *cursor = galley.cursor_up_one_row(cursor);
            }
        }
        Key::ArrowDown => {
            if modifiers.command {
                // mac and windows behavior
                *cursor = galley.end();
            } else {
                *cursor = galley.cursor_down_one_row(cursor);
            }
        }

        Key::Home => {
            if modifiers.ctrl {
                // windows behavior
                *cursor = Cursor::default();
            } else {
                *cursor = galley.cursor_begin_of_row(cursor);
            }
        }
        Key::End => {
            if modifiers.ctrl {
                // windows behavior
                *cursor = galley.end();
            } else {
                *cursor = galley.cursor_end_of_row(cursor);
            }
        }

        _ => unreachable!(),
    }
}

// ----------------------------------------------------------------------------

fn select_word_at(text: &str, ccursor: CCursor) -> CCursorRange {
    if ccursor.index == 0 {
        CCursorRange::two(ccursor, ccursor_next_word(text, ccursor))
    } else {
        let it = text.chars();
        let mut it = it.skip(ccursor.index - 1);
        if let Some(char_before_cursor) = it.next() {
            if let Some(char_after_cursor) = it.next() {
                if is_word_char(char_before_cursor) && is_word_char(char_after_cursor) {
                    let min = ccursor_previous_word(text, ccursor + 1);
                    let max = ccursor_next_word(text, min);
                    CCursorRange::two(min, max)
                } else if is_word_char(char_before_cursor) {
                    let min = ccursor_previous_word(text, ccursor);
                    let max = ccursor_next_word(text, min);
                    CCursorRange::two(min, max)
                } else if is_word_char(char_after_cursor) {
                    let max = ccursor_next_word(text, ccursor);
                    CCursorRange::two(ccursor, max)
                } else {
                    let min = ccursor_previous_word(text, ccursor);
                    let max = ccursor_next_word(text, ccursor);
                    CCursorRange::two(min, max)
                }
            } else {
                let min = ccursor_previous_word(text, ccursor);
                CCursorRange::two(min, ccursor)
            }
        } else {
            let max = ccursor_next_word(text, ccursor);
            CCursorRange::two(ccursor, max)
        }
    }
}

fn select_line_at(text: &str, ccursor: CCursor) -> CCursorRange {
    if ccursor.index == 0 {
        CCursorRange::two(ccursor, ccursor_next_line(text, ccursor))
    } else {
        let it = text.chars();
        let mut it = it.skip(ccursor.index - 1);
        if let Some(char_before_cursor) = it.next() {
            if let Some(char_after_cursor) = it.next() {
                if (!is_linebreak(char_before_cursor)) && (!is_linebreak(char_after_cursor)) {
                    let min = ccursor_previous_line(text, ccursor + 1);
                    let max = ccursor_next_line(text, min);
                    CCursorRange::two(min, max)
                } else if !is_linebreak(char_before_cursor) {
                    let min = ccursor_previous_line(text, ccursor);
                    let max = ccursor_next_line(text, min);
                    CCursorRange::two(min, max)
                } else if !is_linebreak(char_after_cursor) {
                    let max = ccursor_next_line(text, ccursor);
                    CCursorRange::two(ccursor, max)
                } else {
                    let min = ccursor_previous_line(text, ccursor);
                    let max = ccursor_next_line(text, ccursor);
                    CCursorRange::two(min, max)
                }
            } else {
                let min = ccursor_previous_line(text, ccursor);
                CCursorRange::two(min, ccursor)
            }
        } else {
            let max = ccursor_next_line(text, ccursor);
            CCursorRange::two(ccursor, max)
        }
    }
}

fn ccursor_next_word(text: &str, ccursor: CCursor) -> CCursor {
    CCursor {
        index: next_word_boundary_char_index(text.chars(), ccursor.index),
        prefer_next_row: false,
    }
}

fn ccursor_next_line(text: &str, ccursor: CCursor) -> CCursor {
    CCursor {
        index: next_line_boundary_char_index(text.chars(), ccursor.index),
        prefer_next_row: false,
    }
}

fn ccursor_previous_word(text: &str, ccursor: CCursor) -> CCursor {
    let num_chars = text.chars().count();
    CCursor {
        index: num_chars
            - next_word_boundary_char_index(text.chars().rev(), num_chars - ccursor.index),
        prefer_next_row: true,
    }
}

fn ccursor_previous_line(text: &str, ccursor: CCursor) -> CCursor {
    let num_chars = text.chars().count();
    CCursor {
        index: num_chars
            - next_line_boundary_char_index(text.chars().rev(), num_chars - ccursor.index),
        prefer_next_row: true,
    }
}

fn next_word_boundary_char_index(it: impl Iterator<Item = char>, mut index: usize) -> usize {
    let mut it = it.skip(index);
    if let Some(_first) = it.next() {
        index += 1;

        if let Some(second) = it.next() {
            index += 1;
            for next in it {
                if is_word_char(next) != is_word_char(second) {
                    break;
                }
                index += 1;
            }
        }
    }
    index
}

fn next_line_boundary_char_index(it: impl Iterator<Item = char>, mut index: usize) -> usize {
    let mut it = it.skip(index);
    if let Some(_first) = it.next() {
        index += 1;

        if let Some(second) = it.next() {
            index += 1;
            for next in it {
                if is_linebreak(next) != is_linebreak(second) {
                    break;
                }
                index += 1;
            }
        }
    }
    index
}

fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn is_linebreak(c: char) -> bool {
    c == '\r' || c == '\n'
}

/// Accepts and returns character offset (NOT byte offset!).
fn find_line_start(text: &str, current_index: CCursor) -> CCursor {
    // We know that new lines, '\n', are a single byte char, but we have to
    // work with char offsets because before the new line there may be any
    // number of multi byte chars.
    // We need to know the char index to be able to correctly set the cursor
    // later.
    let chars_count = text.chars().count();

    let position = text
        .chars()
        .rev()
        .skip(chars_count - current_index.index)
        .position(|x| x == '\n');

    match position {
        Some(pos) => CCursor::new(current_index.index - pos),
        None => CCursor::new(0),
    }
}

fn decrease_indentation(ccursor: &mut CCursor, text: &mut dyn TextBuffer) {
    let line_start = find_line_start(text.as_str(), *ccursor);

    let remove_len = if text.as_str()[line_start.index..].starts_with('\t') {
        Some(1)
    } else if text.as_str()[line_start.index..].chars().take(text::TAB_SIZE).all(|c| c == ' ') {
        Some(text::TAB_SIZE)
    } else {
        None
    };

    if let Some(len) = remove_len {
        text.delete_char_range(line_start.index..(line_start.index + len));
        if *ccursor != line_start {
            *ccursor -= len;
        }
    }
}
