use eframe::egui::{self, Align2, FontId, TextStyle};

use crate::chord::PitchOrderedSet;
use std::time::Duration;

/// Render a menu row that includes a right-aligned, monospace shortcut hint.
/// Returns the `Response` for the whole row so callers can call `.clicked()` etc.
pub fn menu_item_with_shortcut(
    ui: &mut egui::Ui,
    label: &str,
    shortcut: &str,
    desired_width: Option<f32>,
) -> egui::Response {
    // Calculate fonts
    let label_font = TextStyle::Button.resolve(ui.style());
    let shortcut_font = if cfg!(target_os = "macos") {
        FontId::proportional(label_font.size * 0.95)
    } else {
        FontId::monospace(label_font.size * 0.90)
    };

    // Calculate minimum width required for content
    let min_width = if let Some(w) = desired_width {
        w
    } else {
        let label_w = ui
            .painter()
            .layout_no_wrap(
                label.to_string(),
                label_font.clone(),
                egui::Color32::TRANSPARENT,
            )
            .size()
            .x;
        let shortcut_w = if shortcut.contains('⇧') {
            let mut total = 0.0;
            for c in shortcut.chars() {
                if c == '⇧' {
                    total += shortcut_font.size * 0.9;
                } else {
                    total += ui
                        .painter()
                        .layout_no_wrap(
                            c.to_string(),
                            shortcut_font.clone(),
                            egui::Color32::TRANSPARENT,
                        )
                        .size()
                        .x;
                }
            }
            total
        } else {
            ui.painter()
                .layout_no_wrap(
                    shortcut.to_string(),
                    shortcut_font.clone(),
                    egui::Color32::TRANSPARENT,
                )
                .size()
                .x
        };
        label_w + shortcut_w + 30.0
    };

    let height = ui.spacing().interact_size.y.max(18.0);
    let (rect, resp) = ui.allocate_at_least(egui::vec2(min_width, height), egui::Sense::click());
    let painter = ui.painter();

    // Subtle hover/active background
    if resp.hovered() || resp.is_pointer_button_down_on() {
        let fill = ui.style().visuals.widgets.hovered.bg_fill;
        painter.rect_filled(rect, 0.0, fill);
    }

    // Left label
    painter.text(
        rect.left_center() + egui::vec2(5.0, 0.0),
        Align2::LEFT_CENTER,
        label,
        label_font,
        ui.style().visuals.widgets.inactive.text_color(),
    );

    // Right shortcut
    let text_color = ui.style().visuals.text_color();
    if shortcut.contains('⇧') {
        let mut x = rect.right() - 5.0;
        let y = rect.center().y;

        for c in shortcut.chars().rev() {
            if c == '⇧' {
                let size = shortcut_font.size;
                let symbol_w = size * 0.9;
                let symbol_rect = egui::Rect::from_min_size(
                    egui::pos2(x - symbol_w, y - size / 2.0),
                    egui::vec2(symbol_w, size),
                );

                let stroke = egui::Stroke::new(1.5, text_color);
                let p = |u: f32, v: f32| {
                    egui::pos2(
                        symbol_rect.min.x + u * symbol_rect.width(),
                        symbol_rect.min.y + v * symbol_rect.height(),
                    )
                };

                let points = vec![
                    p(0.5, 0.15),
                    p(0.15, 0.55),
                    p(0.38, 0.55),
                    p(0.38, 0.9),
                    p(0.62, 0.9),
                    p(0.62, 0.55),
                    p(0.85, 0.55),
                    p(0.5, 0.15),
                ];
                painter.add(egui::Shape::line(points, stroke));

                x -= symbol_w;
            } else {
                let galley =
                    painter.layout_no_wrap(c.to_string(), shortcut_font.clone(), text_color);
                let w = galley.size().x;
                painter.galley(
                    egui::pos2(x - w, y - galley.size().y / 2.0),
                    galley,
                    text_color,
                );
                x -= w;
            }
        }
    } else {
        painter.text(
            rect.right_center() + egui::vec2(-5.0, 0.0),
            Align2::RIGHT_CENTER,
            shortcut,
            shortcut_font,
            text_color,
        );
    }

    resp
}

// Extend `egui::Ui` with a convenience button that shows a right-aligned shortcut hint.
// This delegates to `menu_item_with_shortcut` so visuals remain consistent with existing menu rows.
pub trait ButtonWithShortcut {
    fn button_with_shortcut(&mut self, label: &str, shortcut: &str) -> egui::Response;
}

impl ButtonWithShortcut for egui::Ui {
    fn button_with_shortcut(&mut self, label: &str, shortcut: &str) -> egui::Response {
        // Reuse the existing helper so both approaches stay visually consistent.
        menu_item_with_shortcut(self, label, shortcut, None)
    }
}

/// Render chord cards in a horizontal scroll area. Now accepts per-beat chords
/// as (measure, beat_index, chord, duration). Optionally inserts an initial group of
/// N.C. cards (one per beat) at measure 0. Adjacent identical cards are merged visually;
/// groups (per-measure) use a larger gap between them than internal gaps.
pub fn chord_cards_horizontal(
    ui: &mut egui::Ui,
    chords: &[(u32, u32, PitchOrderedSet, Duration)],
    _beats_per_measure: u32,
    show_initial: bool,
    card_w: f32,
    card_h: f32,
    auto_scroll: bool,
    chord_pending_scroll_index: &mut Option<usize>,
) -> f32 {
    let out = egui::ScrollArea::horizontal()
        .stick_to_right(auto_scroll)
        .max_height(card_h + 16.0)
        .show(ui, |ui| {
            ui.horizontal(|ui| {
                let spacing = ui.spacing().item_spacing.x;
                ui.spacing_mut().item_spacing.x = 0.0;

                // Build display list handling "show_initial" setting
                let mut display_chords: Vec<(u32, u32, PitchOrderedSet, Duration)> = Vec::new();
                for c in chords {
                    // if show_initial is false, skip measure 0
                    if show_initial || c.0 > 0 {
                        display_chords.push(c.clone());
                    }
                }

                let mut card_rects: Vec<egui::Rect> = Vec::new();

                // Layout constants
                let small_gap = spacing.min(4.0); // intra-measure (shorter)
                let big_gap = small_gap * 2.0; // inter-measure
                let beat_w = card_w; // use same width as configured card width for exact alignment

                // Instead of drawing one rect per beat, merge consecutive identical chords into segments.
                let mut i = 0usize;
                while i < display_chords.len() {
                    let (m, b, chord, _dur) = &display_chords[i];
                    // extend as long as adjacent chords are identical
                    let mut end = i;
                    while end + 1 < display_chords.len() && display_chords[end + 1].2 == *chord {
                        end += 1;
                    }
                    let count = end - i + 1;
                    // compute width for this merged segment
                    let seg_w = beat_w * (count as f32) + small_gap * ((count - 1) as f32);

                    let total_dur: Duration =
                        display_chords[i..=end].iter().map(|(_, _, _, d)| *d).sum();

                    // allocate space and draw
                    let (rect, _resp) =
                        ui.allocate_exact_size(egui::vec2(seg_w, card_h), egui::Sense::hover());
                    let painter = ui.painter_at(rect);
                    painter.rect_filled(rect.shrink(4.0), 6.0, egui::Color32::from_rgb(60, 60, 70));

                    // Label with measure.start_beat (1-based beat index)
                    painter.text(
                        rect.left_top() + egui::vec2(8.0, 6.0),
                        egui::Align2::LEFT_TOP,
                        format!("{}.{}", m, b + 1),
                        egui::FontId::monospace(10.0),
                        egui::Color32::from_gray(200),
                    );
                    painter.text(
                        rect.center(),
                        egui::Align2::CENTER_CENTER,
                        chord.to_string(),
                        egui::FontId::proportional(18.0),
                        egui::Color32::WHITE,
                    );
                    painter.text(
                        rect.right_bottom() - egui::vec2(6.0, 6.0),
                        egui::Align2::RIGHT_BOTTOM,
                        crate::ui::helpers::format_duration_adaptive(total_dur),
                        egui::FontId::monospace(10.0),
                        egui::Color32::from_gray(180),
                    );

                    // Push the same rect multiple times so indices map 1:1 to beats (useful for scroll targeting)
                    for _ in 0..count {
                        card_rects.push(rect);
                    }

                    // advance i
                    i = end + 1;

                    // insert gap before next segment: use big gap if next segment starts in a different measure
                    if i < display_chords.len() {
                        let next_measure = display_chords[i].0;
                        if next_measure != *m {
                            ui.add_space(big_gap);
                        } else {
                            ui.add_space(small_gap);
                        }
                    }
                }

                if let Some(idx) = chord_pending_scroll_index.take() {
                    if idx < card_rects.len() {
                        ui.scroll_to_rect(card_rects[idx], Some(egui::Align::Max));
                        ui.ctx().request_repaint();
                    }
                }
            });
        });
    out.state.offset.x
}

/// Render chord cards in a wrapped vertical layout inside a vertical scroll area.
/// Each measure occupies one horizontal row containing `beats_per_measure` beat cards.
/// If the measure row is wider than available width, the row becomes horizontally scrollable.
pub fn chord_cards_vertical(
    ui: &mut egui::Ui,
    chords: &[(u32, u32, PitchOrderedSet, Duration)],
    _beats_per_measure: u32,
    show_initial: bool,
    card_w: f32,
    card_h: f32,
    auto_scroll: bool,
    chord_pending_scroll_index: &mut Option<usize>,
) -> f32 {
    let out = egui::ScrollArea::vertical()
        .stick_to_bottom(auto_scroll)
        .show(ui, |ui| {
            let available = ui.available_width();
            let small_gap = ui.spacing().item_spacing.x.min(4.0);
            let big_gap = small_gap * 2.0;
            let beat_w = card_w;
            let mut card_rects: Vec<egui::Rect> = Vec::new();

            // Build display list handling "show_initial" setting
            let mut display_chords: Vec<(u32, u32, PitchOrderedSet, Duration)> = Vec::new();
            for c in chords {
                // if show_initial is false, skip measure 0
                if show_initial || c.0 > 0 {
                    display_chords.push(c.clone());
                }
            }

            // Build measures vector: each entry is (measure, Vec<beats...>)
            let mut measures: Vec<(u32, Vec<(u32, PitchOrderedSet, Duration)>)> = Vec::new();
            let mut di = 0usize;
            while di < display_chords.len() {
                let measure = display_chords[di].0;
                let mut beats: Vec<(u32, PitchOrderedSet, Duration)> = Vec::new();
                while di < display_chords.len() && display_chords[di].0 == measure {
                    let (_m, b, chord, dur) = display_chords[di].clone();
                    beats.push((b, chord, dur));
                    di += 1;
                }
                measures.push((measure, beats));
            }

            // Pack multiple measures per row if they fit; otherwise a row will be horizontally scrollable
            let mut mi = 0usize;
            while mi < measures.len() {
                let mut row_indices: Vec<usize> = Vec::new();
                let mut row_w: f32 = 0.0;
                let mut mj = mi;
                while mj < measures.len() {
                    let beats_count = measures[mj].1.len();
                    let measure_w = beat_w * (beats_count as f32)
                        + small_gap * ((beats_count.saturating_sub(1)) as f32);
                    let new_w = if row_w == 0.0 {
                        measure_w
                    } else {
                        row_w + big_gap + measure_w
                    };
                    if new_w <= available {
                        row_w = new_w;
                        row_indices.push(mj);
                        mj += 1;
                    } else {
                        if row_w == 0.0 {
                            // force oversized single measure into the row (row will be scrollable)
                            row_indices.push(mj);
                            mj += 1;
                        }
                        break;
                    }
                }

                // render row
                if row_w <= available {
                    ui.horizontal(|ui_row| {
                        ui_row.spacing_mut().item_spacing.x = 0.0;
                        for (ri, &midx) in row_indices.iter().enumerate() {
                            let (measure, beats) = &measures[midx];
                            let mut j = 0usize;
                            while j < beats.len() {
                                let chord = &beats[j].1;
                                let mut end = j;
                                while end + 1 < beats.len() && beats[end + 1].1 == *chord {
                                    end += 1;
                                }
                                let count = end - j + 1;
                                let seg_w =
                                    beat_w * (count as f32) + small_gap * ((count - 1) as f32);
                                let total_dur: Duration =
                                    beats[j..=end].iter().map(|(_, _, d)| *d).sum();
                                let (rect, _resp) = ui_row.allocate_exact_size(
                                    egui::vec2(seg_w, card_h),
                                    egui::Sense::hover(),
                                );
                                let painter = ui_row.painter_at(rect);
                                painter.rect_filled(
                                    rect.shrink(4.0),
                                    6.0,
                                    egui::Color32::from_rgb(60, 60, 70),
                                );
                                painter.text(
                                    rect.left_top() + egui::vec2(8.0, 6.0),
                                    egui::Align2::LEFT_TOP,
                                    format!("{}.{}", measure, beats[j].0 + 1),
                                    egui::FontId::monospace(10.0),
                                    egui::Color32::from_gray(200),
                                );
                                painter.text(
                                    rect.center(),
                                    egui::Align2::CENTER_CENTER,
                                    beats[j].1.to_string(),
                                    egui::FontId::proportional(18.0),
                                    egui::Color32::WHITE,
                                );
                                painter.text(
                                    rect.right_bottom() - egui::vec2(6.0, 6.0),
                                    egui::Align2::RIGHT_BOTTOM,
                                    crate::ui::helpers::format_duration_adaptive(total_dur),
                                    egui::FontId::monospace(10.0),
                                    egui::Color32::from_gray(180),
                                );
                                for _ in 0..count {
                                    card_rects.push(rect);
                                }
                                j = end + 1;
                                if j < beats.len() {
                                    ui_row.add_space(small_gap);
                                }
                            }
                            if ri + 1 < row_indices.len() {
                                ui_row.add_space(big_gap);
                            }
                        }
                    });
                } else {
                    egui::ScrollArea::horizontal()
                        .max_height(card_h + 16.0)
                        .show(ui, |ui_row| {
                            ui_row.horizontal(|ui_row| {
                                ui_row.spacing_mut().item_spacing.x = 0.0;
                                for (ri, &midx) in row_indices.iter().enumerate() {
                                    let (measure, beats) = &measures[midx];
                                    let mut j = 0usize;
                                    while j < beats.len() {
                                        let chord = &beats[j].1;
                                        let mut end = j;
                                        while end + 1 < beats.len() && beats[end + 1].1 == *chord {
                                            end += 1;
                                        }
                                        let count = end - j + 1;
                                        let seg_w = beat_w * (count as f32)
                                            + small_gap * ((count - 1) as f32);
                                        let total_dur: Duration =
                                            beats[j..=end].iter().map(|(_, _, d)| *d).sum();
                                        let (rect, _resp) = ui_row.allocate_exact_size(
                                            egui::vec2(seg_w, card_h),
                                            egui::Sense::hover(),
                                        );
                                        let painter = ui_row.painter_at(rect);
                                        painter.rect_filled(
                                            rect.shrink(4.0),
                                            6.0,
                                            egui::Color32::from_rgb(60, 60, 70),
                                        );
                                        painter.text(
                                            rect.left_top() + egui::vec2(8.0, 6.0),
                                            egui::Align2::LEFT_TOP,
                                            format!("{}.{}", measure, beats[j].0 + 1),
                                            egui::FontId::monospace(10.0),
                                            egui::Color32::from_gray(200),
                                        );
                                        painter.text(
                                            rect.center(),
                                            egui::Align2::CENTER_CENTER,
                                            beats[j].1.to_string(),
                                            egui::FontId::proportional(18.0),
                                            egui::Color32::WHITE,
                                        );
                                        painter.text(
                                            rect.right_bottom() - egui::vec2(6.0, 6.0),
                                            egui::Align2::RIGHT_BOTTOM,
                                            crate::ui::helpers::format_duration_adaptive(total_dur),
                                            egui::FontId::monospace(10.0),
                                            egui::Color32::from_gray(180),
                                        );
                                        for _ in 0..count {
                                            card_rects.push(rect);
                                        }
                                        j = end + 1;
                                        if j < beats.len() {
                                            ui_row.add_space(small_gap);
                                        }
                                    }
                                    if ri + 1 < row_indices.len() {
                                        ui_row.add_space(big_gap);
                                    }
                                }
                            });
                        });
                }

                // small vertical padding between measure rows
                ui.add_space(6.0);

                mi = mj;
            }

            // Reserve space at the bottom so the last row can scroll above the status bar.
            let bottom_padding = 12.0;
            ui.add_space(bottom_padding);

            if let Some(idx) = chord_pending_scroll_index.take() {
                if idx < card_rects.len() {
                    ui.scroll_to_rect(card_rects[idx], Some(egui::Align::Max));
                    ui.ctx().request_repaint();
                }
            }
        });
    out.state.offset.y
}
