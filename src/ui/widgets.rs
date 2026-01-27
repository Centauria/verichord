use eframe::egui::{self, Align2, FontId, TextStyle};

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
