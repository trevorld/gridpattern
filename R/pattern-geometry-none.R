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
create_pattern_none <- function(params, boundary_df, aspect_ratio,
                                legend = FALSE) {
  grid::nullGrob()
}

#' Grobs without any pattern
#'
#' `grid.pattern_none()` draws nothing onto the graphic device.
#'
#' @inheritParams grid.pattern
#' @param ... Currently ignored
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' grid.pattern_none(x_hex, y_hex)
#' @seealso [grid::grid.null()]
#' @export
grid.pattern_none <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                              default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("none", x, y, id,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}
