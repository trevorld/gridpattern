#' Create grob objects for the pattern elements within a boundary
#'
#' @param params params/coords for a single element. named list or single row data.frame
#' @param boundary_df mask for the pattern rendering
#' @param aspect_ratio a aspect ratio of the plotting area.
#' @param legend is the pattern being created in the legend? default FALSE.
#'  Use this flag if you want different pattern drawing behviour for the legend.
#'
#' @return grid grob objects.
#' @noRd
create_pattern_fill <- function(params, boundary_df, aspect_ratio,
                                legend = FALSE) {
    alpha <- ifelse(is.na(params$pattern_alpha), 1, params$pattern_alpha)
    fill <- update_alpha(params$pattern_fill, alpha)
    gp <- grid::gpar(col = NA_character_, fill = fill)

    convert_polygon_df_to_polygon_grob(boundary_df, gp = gp)
}

#' Grobs with a simple fill pattern
#'
#' `grid.pattern_fill()` draws a simple fill pattern onto the graphics device.
#'
#' @inheritParams grid.pattern_circle
#' @param ... Currently ignored
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' grid.pattern_fill(x_hex, y_hex, fill = "blue")
#'
#' if (guess_has_R4.1_features("patterns")) {
#'   grid::grid.newpage()
#'   stripe_fill <- patternFill("stripe", fill = c("red", "blue"))
#'   grid.pattern_fill(x_hex, y_hex, fill = stripe_fill)
#' }
#' @seealso [grid::grid.polygon()]
#' @export
grid.pattern_fill <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                              fill = gp$fill %||% "grey80", 
                              alpha = gp$alpha %||% NA_real_,
                              default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("fill", x, y, id,
                 fill = fill, alpha = alpha,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}
