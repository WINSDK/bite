#![forbid(unsafe_code)]
#![warn(missing_docs)]
#![allow(dead_code)]

use std::ops::Range;
use std::time::{SystemTime, Duration};
use egui::{Align, Pos2, Rect, Ui, Vec2};

/// The response from a call to [`VirtualList::ui_custom_layout`]
pub struct VirtualListResponse {
    /// The range of items that was displayed
    pub item_range: Range<usize>,

    /// Any items in this range are now visible
    pub newly_visible_items: Range<usize>,
    /// Any items in this range are no longer visible
    pub hidden_items: Range<usize>,
}

#[derive(Debug)]
struct RowData {
    range: Range<usize>,
    pos: Pos2,
}

/// Virtual list widget for egui.
#[derive(Debug)]
pub struct VirtualList {
    rows: Vec<RowData>,

    previous_item_range: Range<usize>,

    // The index of the first item that has an unknown rect
    last_known_row_index: Option<usize>,

    average_row_size: Option<Vec2>,
    average_items_per_row: Option<f32>,

    // We will recalculate every item's rect if the scroll area's width changes
    last_width: Option<f32>,

    max_rows_calculated_per_frame: usize,

    over_scan: f32,

    // If set, the list will scroll by this many items from the top.
    // Useful when items at the top are added, and the scroll position should be maintained.
    // The value should be the number of items that were added at the top.
    items_inserted_at_start: Option<usize>,

    check_for_resize: bool,
    scroll_position_sync_on_resize: bool,
    hide_on_resize: Option<Duration>,
    /// Stores the index and visibility percentage of the last item that was at the top of the list
    last_top_most_item: Option<(usize, f32)>,
    last_resize: SystemTime,
}

impl Default for VirtualList {
    fn default() -> Self {
        Self::new()
    }
}

impl VirtualList {
    /// Create a new VirtualList
    pub fn new() -> Self {
        Self {
            previous_item_range: usize::MAX..usize::MAX,
            last_known_row_index: None,
            last_width: None,
            average_row_size: None,
            rows: vec![],
            average_items_per_row: None,
            max_rows_calculated_per_frame: 1000,
            over_scan: 0.0,
            items_inserted_at_start: None,
            check_for_resize: true,
            scroll_position_sync_on_resize: true,
            hide_on_resize: Some(Duration::from_millis(100)),
            last_top_most_item: None,
            last_resize: SystemTime::now(),
        }
    }

    /// Call this when you insert items at the start of the list.
    /// The list will offset the scroll position by the height of these items, so that for the user,
    /// the scroll position stays the same.
    pub fn items_inserted_at_start(&mut self, scroll_top_items: usize) {
        self.items_inserted_at_start = Some(scroll_top_items);
    }

    /// Set the overscan, or how much the list should render outside of the visible area.
    /// The default is 0.0.
    pub fn over_scan(&mut self, over_scan: f32) {
        self.over_scan = over_scan;
    }

    /// Checks if the list was resized and resets the cached sizes if it was.
    /// If you are certain that the item heights won't change on resize, you can disable this.
    /// The default is true.
    pub fn check_for_resize(&mut self, check_for_resize: bool) {
        self.check_for_resize = check_for_resize;
    }

    /// Tries to keep the first visible item at the top of the screen when the window is resized.
    /// Depending on the contents, this may cause some flicker.
    /// The default is true.
    pub fn scroll_position_sync_on_resize(&mut self, scroll_position_sync_on_resize: bool) {
        self.scroll_position_sync_on_resize = scroll_position_sync_on_resize;
    }

    /// Prevent flickering while resizing by hiding the list until the resize is done.
    /// The default is true.
    pub fn hide_on_resize(&mut self, hide_on_resize: impl Into<Option<Duration>>) {
        self.hide_on_resize = hide_on_resize.into();
    }

    /// The layout closure gets called with the index of the first item that should be displayed.
    /// It should return the number of items that were displayed.
    pub fn ui_custom_layout(
        &mut self,
        ui: &mut Ui,
        length: usize,
        mut layout: impl FnMut(&mut Ui, usize) -> usize,
    ) -> VirtualListResponse {
        let mut scroll_to_item_index_visibility = None;
        if let Some(last_width) = self.last_width {
            if ui.available_width() != last_width {
                self.last_width = Some(ui.available_width());
                if self.check_for_resize {
                    self.last_known_row_index = None;
                    self.rows.clear();
                    self.last_resize = SystemTime::now();
                    if self.scroll_position_sync_on_resize {
                        scroll_to_item_index_visibility = self.last_top_most_item;
                    }
                }
            }
        } else {
            self.last_width = Some(ui.available_width());
        }

        if let Some(hide_on_resize) = self.hide_on_resize {
            if self.last_resize.elapsed().unwrap_or_default() < hide_on_resize {
                ui.set_visible(false);
                ui.ctx().request_repaint();
            }
        }

        // Start of the scroll area (basically scroll_offset + whatever is above the scroll area)
        let min = ui.next_widget_position().to_vec2();

        let mut row_start_index = self.last_known_row_index.unwrap_or(0);

        // This calculates the visible rect inside the scroll area
        // Should be equivalent to to viewport from ScrollArea::show_viewport(), offset by whatever is above the scroll area
        let visible_rect = ui.clip_rect().translate(-min);

        let mut index_offset = 0;

        // Calculate the added_height for items that were added at the top and scroll by that amount
        // to maintain the scroll position
        let scroll_items_top_step_2 =
            if let Some(scroll_top_items) = self.items_inserted_at_start.take() {
                let mut measure_ui = ui.child_ui(ui.max_rect(), *ui.layout());
                measure_ui.set_visible(false);

                let start_height = measure_ui.next_widget_position();
                for i in 0..scroll_top_items {
                    layout(&mut measure_ui, i);
                }
                let end_height = measure_ui.next_widget_position();

                let added_height = end_height.y - start_height.y + ui.spacing().item_spacing.y;

                // TODO: Ideally we should be able to use scroll_with_delta here but that doesn't work
                // until https://github.com/emilk/egui/issues/2783 is fixed. Before, scroll_to_rect
                // only works when the mouse is over the scroll area.
                // ui.scroll_with_delta(Vec2::new(0.0, -added_height));
                ui.scroll_to_rect(ui.clip_rect().translate(Vec2::new(0.0, added_height)), None);

                index_offset = scroll_top_items;

                ui.ctx().request_repaint();

                Some(added_height)
            } else {
                None
            };

        // Find the first row that is visible
        loop {
            if row_start_index == 0 {
                break;
            }

            if let Some(row) = self.rows.get(row_start_index) {
                let skip = if let Some((idx, _)) = scroll_to_item_index_visibility {
                    row.range.start >= idx
                } else {
                    false
                };

                if row.pos.y <= visible_rect.min.y && !skip {
                    ui.add_space(row.pos.y);
                    break;
                }
            }
            row_start_index -= 1;
        }
        let mut current_row = row_start_index;

        let item_start_index = self
            .rows
            .get(row_start_index)
            .map(|row| row.range.start)
            .unwrap_or(0)
            + index_offset;

        let mut current_item_index = item_start_index;

        let mut iterations = 0;

        let mut first_visible_item_index = None;
        let mut first_visible_item_visibility = None;
        let mut did_scroll = false;

        loop {
            // Bail out if we're recalculating too many items
            if iterations > self.max_rows_calculated_per_frame {
                ui.ctx().request_repaint();
                break;
            }
            iterations += 1;

            // let item = self.items.get_mut(current_row);
            if current_item_index < length {
                let pos = ui.next_widget_position() - min;
                let count = layout(ui, current_item_index);
                let size = ui.next_widget_position() - min - pos;
                let rect = Rect::from_min_size(pos, size);

                let range = current_item_index..current_item_index + count;

                if let Some((scroll_to, visibility)) = scroll_to_item_index_visibility {
                    if range.contains(&scroll_to) {
                        // TODO: Somehow correct for overscan here
                        ui.scroll_to_rect(
                            Rect::from_min_size(
                                pos + min + Vec2::new(0.0, size.y * visibility),
                                Vec2::ZERO,
                            ),
                            Some(Align::Min),
                        );
                        scroll_to_item_index_visibility = None;
                        did_scroll = true;
                    }
                }

                if first_visible_item_index.is_none() && rect.max.y >= visible_rect.min.y {
                    first_visible_item_index = Some(current_item_index);
                    first_visible_item_visibility =
                        Some((visible_rect.min.y - rect.min.y) / (rect.max.y - rect.min.y));
                }

                if let Some(row) = self.rows.get_mut(current_row) {
                    row.range = range;
                    row.pos = pos;
                } else {
                    self.rows.push(RowData {
                        range: current_item_index..current_item_index + count,
                        pos,
                    });

                    let size_with_space = size;

                    self.average_row_size = Some(
                        self.average_row_size
                            .map(|size| {
                                (current_row as f32 * size + size_with_space)
                                    / (current_row as f32 + 1.0)
                            })
                            .unwrap_or(size),
                    );

                    self.average_items_per_row = Some(
                        self.average_items_per_row
                            .map(|avg_count| {
                                (current_row as f32 * avg_count + count as f32)
                                    / (current_row as f32 + 1.0)
                            })
                            .unwrap_or(count as f32),
                    );

                    self.last_known_row_index = Some(current_row);
                }

                current_item_index += count;

                if rect.max.y > visible_rect.max.y && scroll_to_item_index_visibility.is_none() {
                    break;
                }
            } else {
                break;
            }

            current_row += 1;
        }

        let item_range = first_visible_item_index.unwrap_or(item_start_index)..current_item_index;

        // If we scrolled this frame, don't store the last top most item
        if !did_scroll
            && self.last_resize.elapsed().unwrap_or_default() > Duration::from_millis(1000)
        {
            if let Some((first_visible_item_index, first_visible_item_visibility)) =
                first_visible_item_index.zip(first_visible_item_visibility)
            {
                self.last_top_most_item =
                    Some((first_visible_item_index, first_visible_item_visibility));
            }
        }

        if item_range.end < length {
            ui.set_min_height(
                (length - item_range.end) as f32 / self.average_items_per_row.unwrap_or(1.0)
                    * self.average_row_size.unwrap_or(Vec2::ZERO).y,
            );
        }

        let mut hidden_range =
            self.previous_item_range.start..item_range.start.min(self.previous_item_range.end);
        if hidden_range.is_empty() {
            hidden_range =
                item_range.end.max(self.previous_item_range.start)..self.previous_item_range.end;
        }

        let mut visible_range = self.previous_item_range.end.max(item_range.start)..item_range.end;
        if visible_range.is_empty() {
            visible_range =
                self.previous_item_range.start..item_range.start.min(self.previous_item_range.end);
        }

        self.previous_item_range = item_range.clone();

        if let Some(added_height) = scroll_items_top_step_2 {
            // We need to add the height at the bottom or else we might not be able to scroll
            ui.add_space(added_height);
            self.rows.clear();
            self.last_known_row_index = None;
            self.average_items_per_row = None;
            self.average_row_size = None;
        }

        VirtualListResponse {
            item_range,
            newly_visible_items: visible_range,
            hidden_items: hidden_range,
        }
    }

    /// Resets the list, clearing all cached data. Call this if items changed size, items were replaced, etc.
    /// The heights will be recalculated on the next frame.
    pub fn reset(&mut self) {
        self.last_known_row_index = None;
        self.last_width = None;
        self.average_row_size = None;
        self.rows.clear();
        self.average_items_per_row = None;
    }
}
