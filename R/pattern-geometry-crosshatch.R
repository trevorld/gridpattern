#' Crosshatch patterned grobs
#'
#' `grid.pattern_crosshatch()` draws a crosshatch pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param fill2 The fill colour for the \dQuote{top} crosshatch lines.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     grid.pattern_crosshatch(x_hex, y_hex, colour = "black", fill = "blue",
#'                             fill2 = "yellow", density = 0.5)
#'     grid.newpage()
#'     grid.pattern_crosshatch(x_hex, y_hex, density = 0.3,
#'                             gp = gpar(col = "blue", fill = "yellow"))
#'   }
#' @seealso [grid.pattern_weave()] which interweaves two sets of lines
#'   as well as the `ggpattern` documentation:
#'   <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html>
#' @export
grid.pattern_crosshatch <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                    colour = gp$col %||% "grey20",
                                    fill = gp$fill %||% "grey80", fill2 = fill,
                                    angle = 30, density = 0.2,
                                    spacing = 0.05, xoffset = 0, yoffset = 0,
                                    alpha = gp$alpha %||% NA_real_, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                    grid = "square",
                                    default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("crosshatch", x, y, id,
                 colour = colour, fill = fill, fill2 = fill2, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 alpha = alpha, linetype = linetype, size = size,
                 grid = grid,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_crosshatch_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    create_crosshatch_via_sf_helper(params, boundary_df, add_top_hatch = TRUE)
}

create_crosshatch_via_sf_helper <- function(params, boundary_df, add_top_hatch = TRUE) {

    if (abs(params$pattern_density - 1) < .Machine$double.eps^0.5)
        params$pattern_density <- 1 - 1e-6
    stopifnot(params$pattern_density <= 1)

    # work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
    default.units <- "bigpts"
    boundary_df <- convert_polygon_df_units(boundary_df, default.units)
    params <- convert_params_units(params, default.units)
    vpm <- get_vp_measurements(default.units)

    # create grid of points large enough to cover viewport no matter the angle
    grid_xy <- get_xy_grid(params, vpm)

    fill <- alpha(params$pattern_fill, params$pattern_alpha)
    col  <- alpha(params$pattern_colour, params$pattern_alpha)
    lwd  <- params$pattern_size * .pt
    lty  <- params$pattern_linetype
    gp <- gpar(col = col, fill = fill, lwd = lwd, lty = lty, lineend = 'square')

    boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)

    stripes_sf_bot <- create_h_stripes_sf(params, grid_xy, vpm)
    clipped_stripes_sf_bot <- sf::st_intersection(stripes_sf_bot, boundary_sf)
    grob <- sf_multipolygon_to_polygon_grob(clipped_stripes_sf_bot,
                                            gp, default.units, "stripe")

    if (add_top_hatch) {
        gp$fill <- alpha(params$pattern_fill2, params$pattern_alpha)

        stripes_sf_top <- create_v_stripes_sf(params, grid_xy, vpm)
        clipped_stripes_sf_top <- sf::st_intersection(stripes_sf_top, boundary_sf)
        grob_top <- sf_multipolygon_to_polygon_grob(clipped_stripes_sf_top,
                                                    gp, default.units, "top")

        grob <- editGrob(grob, name = "bottom")
        grob <- grobTree(grob, grob_top, name = "crosshatch")
    }
    grob
}

# build sf multipolygon 'rect' for each grid_xy$y value
create_h_stripes_sf <- function(params, grid_xy, vpm) {
    halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
    l_rects <- lapply(grid_xy$y,
                      function(y0) {
                          x <- c(grid_xy$x_min, grid_xy$x_min, grid_xy$x_max, grid_xy$x_max)
                          y <- y0 + c(-1, 1, 1, -1) * halfwidth
                          xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
                          m <- as.matrix(as.data.frame(xy))
                          list(rbind(m, m[1,]))
                      })
    sf::st_multipolygon(l_rects)
}

# build sf multipolygon 'rect' for each grid_xy$x value
create_v_stripes_sf <- function(params, grid_xy, vpm) {
    halfwidth <- 0.5 * grid_xy$h_spacing * params$pattern_density
    l_rects <- lapply(grid_xy$x,
                      function(x0) {
                          x <- x0 + c(-1, 1, 1, -1) * halfwidth
                          y <- c(grid_xy$y_min, grid_xy$y_min, grid_xy$y_max, grid_xy$y_max)
                          xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
                          m <- as.matrix(as.data.frame(xy))
                          list(rbind(m, m[1,]))
                      })
    sf::st_multipolygon(l_rects)
}
