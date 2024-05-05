#' Weave patterned grobs
#'
#' `grid.pattern_weave()` draws a weave pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_crosshatch
#' @param fill The fill colour for the horizontal "weft" lines.
#' @param fill2 The fill colour for the vertical "warp" lines.
#' @param type The weave type.  See [pattern_weave()] for more details.
#' @param subtype The weave subtype.  See [pattern_weave()] for more details.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' gp <- grid::gpar(colour = "black", fill = "lightblue", lwd=0.5)
#'
#' # Plain weave (default weave)
#' grid.pattern_weave(x_hex, y_hex, fill2 = "yellow",
#'                    gp = gp, spacing = 0.1, density = 0.3)
#'
#' # Irregular matt weave
#' grid::grid.newpage()
#' grid.pattern_weave(x_hex, y_hex,  type = "matt_irregular",
#'                    fill2 = "yellow", gp = gp, spacing = 0.1, density = 0.3)
#'
#' # Twill weave
#' grid::grid.newpage()
#' grid.pattern_weave(x_hex, y_hex, type = "twill",
#'                    fill2 = "yellow", gp = gp, spacing = 0.1, density = 0.3)
#'
#' # Zig-zag twill
#' grid::grid.newpage()
#' grid.pattern_weave(x_hex, y_hex, type = "twill_zigzag",
#'                    fill2 = "yellow", gp = gp, spacing = 0.05, density = 0.7)
#'
#' # Herringbone twill with density 1
#' grid::grid.newpage()
#' gp$col <- NA
#' grid.pattern_weave(x_hex, y_hex, type = "twill_herringbone",
#'                    fill2 = "yellow", gp = gp, spacing = 0.05, density = 1.0)
#' @seealso [pattern_weave()]
#' @export
grid.pattern_weave <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                    colour = gp$col %||% "grey20",
                                    fill = gp$fill %||% "grey80", fill2 = fill,
                                    angle = 30, density = 0.2,
                                    spacing = 0.05, xoffset = 0, yoffset = 0, units = "snpc",
                                    alpha = gp$alpha %||% NA_real_,
                                    linetype = gp$lty %||% 1,
                                    linewidth = size %||% gp$lwd %||% 1,
                                    size = NULL,
                                    grid = "square", type = "plain", subtype = NA,
                                    default.units = "npc", name = NULL,
                                    gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("weave", x, y, id,
                 colour = colour, fill = fill, fill2 = fill2, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset, units = units,
                 alpha = alpha, linetype = linetype, linewidth = linewidth,
                 grid = grid, type = type, subtype = subtype,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_weave_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    # 'weft' 'yarns' will just be normal (horizontal) stripes
    grob_weft <- create_crosshatch_via_sf_helper(params, boundary_df, add_top_hatch = FALSE)
    grob_weft <- editGrob(grob_weft, name = "weft")
    # we'll compute 'covered' and 'uncovered' 'warp' rectangles to represent warp 'yarns'
    l <- create_warp_via_sf(params, boundary_df)
    grobTree(l$warp_covered, grob_weft, l$warp_uncovered, name = "weave")
}

create_warp_via_sf <- function(params, boundary_df) {
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

    fill <- update_alpha(params$pattern_fill2, params$pattern_alpha)
    col  <- update_alpha(params$pattern_colour, params$pattern_alpha)
    lwd  <- params$pattern_linewidth * .pt
    lty  <- params$pattern_linetype
    gp <- gpar(col = col, fill = fill, lwd = lwd, lty = lty, lineend = 'square')

    m_weave <- pattern_weave(params$pattern_type, params$pattern_subtype,
                             nrow = length(grid_xy$y), ncol = length(grid_xy$x))

    # compute vertical stripes clipped to boundary
    boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)
    stripes_sf <- create_v_stripes_sf(params, grid_xy, vpm)
    clipped_stripes_sf <- sf::st_intersection(stripes_sf, boundary_sf)

    # compute warp squares covered by weft lines
    warp_covered_sf <- create_warp_covered_sf(params, grid_xy, vpm, m_weave)
    warp_covered_sf <- sf::st_buffer(warp_covered_sf, dist = 0)
    clipped_covered_sf <- sf::st_intersection(clipped_stripes_sf, warp_covered_sf)

    # warp rectangles not covered by weft lines is just stripes minus under squares
    buffered_covered_sf <- sf::st_buffer(clipped_covered_sf, vpm$length / 1e9)
    clipped_uncovered_sf <- sf::st_difference(clipped_stripes_sf, buffered_covered_sf)

    grob_uncovered <- sf_multipolygon_to_polygon_grob(clipped_uncovered_sf,
                                                      gp, default.units, "warp_uncovered")

    grob_covered <- sf_multipolygon_to_polygon_grob(clipped_covered_sf,
                                                    gp, default.units, "warp_covered")

    list(warp_uncovered = grob_uncovered, warp_covered = grob_covered)
}

create_warp_covered_sf <- function(params, grid_xy, vpm, m_weave) {
    halfwidth <- 0.5 * grid_xy$h_spacing * params$pattern_density
    # need list of lists each containing a five row matrix of rectangle vertices
    l_rects <- list()
    for (i in seq_len(nrow(m_weave))) {
        for (j in seq_len(ncol(m_weave))) {
            if (!m_weave[i, j]) {
                x0 <- grid_xy$x[j]
                y0 <- grid_xy$y[i]
                x <- x0 + c(-1, -1, 1,  1) * halfwidth
                y <- y0 + c(-1,  1, 1, -1) * halfwidth
                xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
                m <- as.matrix(as.data.frame(xy))
                l_rects <- append(l_rects, list(list(rbind(m, m[1,]))))
            }
        }
    }
    sf::st_multipolygon(l_rects)
}
