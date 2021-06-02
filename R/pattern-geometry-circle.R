#' Circle patterned grobs
#'
#' `grid.pattern_circle()` draws a circle pattern onto the graphic device.
#'
#' @inheritParams grid.pattern
#' @param ... Currently ignored
#' @param colour Stroke colour
#' @param fill Fill colour
#' @param angle Rotation angle in degrees
#' @param density Approx. fraction of area the pattern fills (between 0 and 1)
#' @param spacing Spacing between repetitions of pattern (between 0 and 1)
#' @param xoffset Shift pattern along x axis (between 0 and 1)
#' @param yoffset Shift pattern along y axis (between 0 and 1)
#' @param alpha Alpha (between 0 and 1) or `NA` (default, preserves colors' alpha value)
#' @param linetype Stroke linetype
#' @param size Stroke linewidth
#' @param type Either `"square"` (default) or `"hex"`.
#'             Adjusts layout, density, and repeating of certain aesthetics to aid in achieving a "tiling" effect.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     grid.pattern_circle(x_hex, y_hex, fill = c("blue", "yellow"), density = 0.5)
#'     grid.newpage()
#'     grid.pattern_circle(x_hex, y_hex, density = 0.8, type = "hex",
#'                         gp = gpar(fill = c("blue", "yellow", "red")))
#'     grid.newpage()
#'     grid.pattern_circle(x_hex, y_hex, density = 1.2, type = "hex",
#'                         gp = gpar(fill = c("blue", "yellow", "red")))
#'   }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-circle.html>
#' @export
grid.pattern_circle <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                colour = gp$col %||% "grey20", fill = gp$fill %||% "grey80", angle = 30,
                                density = 0.2, spacing = 0.05, xoffset = 0, yoffset = 0,
                                alpha = gp$alpha %||% NA_real_, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                type = "square",
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("circle", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 alpha = alpha, linetype = linetype, size = size, type = type,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_circle_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    params$pattern_shape <- "circle"
    grob <- create_pattern_regular_polygon_via_sf(params, boundary_df, aspect_ratio, legend = FALSE)
    editGrob(grob, name = "circle")
}
