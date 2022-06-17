#' Wave patterned grobs
#'
#' `grid.pattern_wave()` draws a wave pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param amplitude Wave amplitude (\dQuote{snpc} units)
#' @param frequency Linear frequency (inverse \dQuote{snpc} units)
#' @param type Either \dQuote{sine} or \dQuote{triangle} (default).
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     grid.newpage()
#'     grid.pattern_wave(x_hex, y_hex, colour = "black", type = "sine",
#'                       fill = c("red", "blue"), density = 0.4,
#'                       spacing = 0.15, angle = 0,
#'                       amplitude = 0.05, frequency = 1 / 0.20)
#'
#'     # zig-zag pattern is a wave of `type` "triangle"
#'     grid.newpage()
#'     grid.pattern_wave(x_hex, y_hex, colour = "black", type = "triangle",
#'                         fill = c("red", "blue"), density = 0.4,
#'                         spacing = 0.15, angle = 0, amplitude = 0.075)
#'
#'   }
#' @seealso Use [grid.pattern_stripe()] for straight lines instead of waves.
#' @export
grid.pattern_wave <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                colour = gp$col %||% "grey20", fill = gp$fill %||% "grey80", angle = 30,
                                density = 0.2, spacing = 0.05, xoffset = 0, yoffset = 0,
                                amplitude = 0.5 * spacing, frequency = 1 / spacing,
                                alpha = gp$alpha %||% NA_real_,
                                linetype = gp$lty %||% 1,
                                linewidth = size %||% gp$lwd %||% 1,
                                size = NULL,
                                grid = "square", type = "triangle",
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("wave", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 amplitude = amplitude, frequency = frequency,
                 alpha = alpha, linetype = linetype, linewidth = linewidth,
                 grid = grid, type = type,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_wave_via_sf <- function(params, boundary_df, aspect_ratio,
                                          legend = FALSE) {

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
    lwd  <- params$pattern_linewidth * .pt
    lty  <- params$pattern_linetype
    density <- params$pattern_density

    n_par <- max(lengths(list(fill, col, lwd, lty, density)))

    fill <- rep(fill, length.out = n_par)
    col <- rep(col, length.out = n_par)
    lwd <- rep(lwd, length.out = n_par)
    lty <- rep(lty, length.out = n_par)
    density <- rep(density, length.out = n_par)

    gl <- gList()
    for (i_par in seq_len(n_par)) {

        gp <- gpar(col = col[i_par], fill = fill[i_par],
                   lwd = lwd[i_par], lty = lty[i_par], lineend = 'square')

        boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)

        waves_sf <- create_waves_sf(params, grid_xy, vpm, i_par, n_par)
        clipped_waves_sf_bot <- sf::st_intersection(waves_sf, boundary_sf)
        name <- paste0("wave.", i_par)
        grob <- sf_multipolygon_to_polygon_grob(clipped_waves_sf_bot,
                                                gp, default.units, name)
        gl <- append_gList(gl, grob)
    }
    gTree(children = gl, name = "regular_polygon")
}

create_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
    switch(params$pattern_type,
           sine = create_sine_waves_sf(params, grid_xy, vpm, i_par, n_par),
           triangle = create_triangle_waves_sf(params, grid_xy, vpm, i_par, n_par),
           abort(paste("Don't know how to create wave pattern", dQuote(params$pattern_type))))
}

create_sine_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
    halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
    a <- params$pattern_amplitude
    n_s <- 180L
    theta <- to_radians(seq(0, by = 360L / n_s, length.out = n_s))
    y_s <- a * sin(theta)
    n_y <- length(grid_xy$y)
    indices_y <- seq(from = i_par, to = n_y, by = n_par)
    l_waves <- lapply(grid_xy$y[indices_y],
                      function(y0) {
                        n_x <- length(grid_xy$x)
                        xc <- seq(grid_xy$x_min, grid_xy$x_max, length.out = n_s * n_x + 1L)
                        yc <- y0 + rep(y_s, length.out = n_s * n_x + 1L)
                        yt <- yc + halfwidth
                        yb <- yc - halfwidth
                        x <- c(xc, rev(xc))
                        y <- c(yt, rev(yb))
                        xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
                        m <- as.matrix(as.data.frame(xy))
                        list(rbind(m, m[1,]))
                      })
    sf::st_multipolygon(l_waves)
}

create_triangle_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
    halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
    a <- params$pattern_amplitude
    n_y <- length(grid_xy$y)
    indices_y <- seq(from = i_par, to = n_y, by = n_par)
    l_waves <- lapply(grid_xy$y[indices_y],
                      function(y0) {
                        n_x <- length(grid_xy$x)
                        xc <- seq(grid_xy$x_min, grid_xy$x_max, length.out = 4L * n_x + 1L)
                        yc <- y0 + rep(c(0, a, 0, -a), length.out = 4L * n_x + 1L)
                        yt <- yc + halfwidth
                        yb <- yc - halfwidth
                        x <- c(xc, rev(xc))
                        y <- c(yt, rev(yb))
                        xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
                        m <- as.matrix(as.data.frame(xy))
                        list(rbind(m, m[1,]))
                      })
    sf::st_multipolygon(l_waves)
}

# # build sf multipolygon 'rect' for each grid_xy$y value
# create_h_stripes_sf <- function(params, grid_xy, vpm) {
#     halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
#     l_rects <- lapply(grid_xy$y,
#                       function(y0) {
#                           x <- c(grid_xy$x_min, grid_xy$x_min, grid_xy$x_max, grid_xy$x_max)
#                           y <- y0 + c(-1, 1, 1, -1) * halfwidth
#                           xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
#                           m <- as.matrix(as.data.frame(xy))
#                           list(rbind(m, m[1,]))
#                       })
#     sf::st_multipolygon(l_rects)
# }
#
# # build sf multipolygon 'rect' for each grid_xy$x value
# create_v_stripes_sf <- function(params, grid_xy, vpm) {
#     halfwidth <- 0.5 * grid_xy$h_spacing * params$pattern_density
#     l_rects <- lapply(grid_xy$x,
#                       function(x0) {
#                           x <- x0 + c(-1, 1, 1, -1) * halfwidth
#                           y <- c(grid_xy$y_min, grid_xy$y_min, grid_xy$y_max, grid_xy$y_max)
#                           xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
#                           m <- as.matrix(as.data.frame(xy))
#                           list(rbind(m, m[1,]))
#                       })
#     sf::st_multipolygon(l_rects)
# }
