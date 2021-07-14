#' Rose curve patterned grobs
#'
#' `grid.pattern_rose()` draws a rose curve pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @inheritParams clippingPathGrob
#' @param rot Angle to rotate rose (degrees, counter-clockwise).
#' @param frequency The \dQuote{angular frequency} parameter of the rose pattern.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @seealso See \url{https://en.wikipedia.org/wiki/Rose_(mathematics)} for more information.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'
#'     gp <- gpar(fill = c("blue", "red", "yellow", "green"), col = "black")
#'     grid.newpage()
#'     grid.pattern_rose(x_hex, y_hex,
#'                       spacing = 0.15, density = 0.5, angle = 0,
#'                       frequency = 1:4, gp = gp)
#'     grid.newpage()
#'     grid.pattern_rose(x_hex, y_hex,
#'                       spacing = 0.15, density = 0.5, angle = 0,
#'                       frequency = 1/1:4, gp = gp)
#'   }
#' @export
grid.pattern_rose <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                              colour = gp$col %||% "grey20",
                              fill = gp$fill %||% "grey80",
                              angle = 30, density = 0.2,
                              spacing = 0.05, xoffset = 0, yoffset = 0,
                              frequency = 0.1,
                              grid = "square", type = NULL, subtype = NULL,
                              rot = 0,
                              alpha = gp$alpha %||% NA_real_, linetype = gp$lty %||% 1,
                              size = gp$lwd %||% 1,
                              use_R4.1_clipping = getOption("ggpattern_use_R4.1_clipping",
                                                            getOption("ggpattern_use_R4.1_features")),
                              png_device = NULL, res = 72,
                              default.units = "npc", name = NULL,
                                         gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("rose", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 scale = scale, frequency = frequency,
                 grid = grid, type = type, subtype = subtype, rot = rot,
                 use_R4.1_clipping = use_R4.1_clipping, png_device = png_device, res = res,
                 alpha = alpha, linetype = linetype, size = size,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_rose <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    # work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
    default.units <- "bigpts"
    boundary_df <- convert_polygon_df_units(boundary_df, default.units)
    params <- convert_params_units(params, default.units)
    vpm <- get_vp_measurements(default.units)

    spacing <- params$pattern_spacing
    grid <- params$pattern_grid

    # create grid of points large enough to cover viewport no matter the angle
    grid_xy <- get_xy_grid(params, vpm)

    # construct grobs using subsets if certain inputs are vectorized
    fill <- alpha(params$pattern_fill, params$pattern_alpha)
    col  <- alpha(params$pattern_colour, params$pattern_alpha)
    lwd  <- params$pattern_size * .pt
    lty  <- params$pattern_linetype

    density <- params$pattern_density
    rot <- params$pattern_rot
    frequency <- params$pattern_frequency

    n_par <- max(lengths(list(fill, col, lwd, lty, density, rot, frequency)))

    fill <- rep(fill, length.out = n_par)
    col <- rep(col, length.out = n_par)
    lwd <- rep(lwd, length.out = n_par)
    lty <- rep(lty, length.out = n_par)
    density <- rep(density, length.out = n_par)
    rot <- rep(rot, length.out = n_par)
    frequency <- rep(frequency, length.out = n_par)

    density_max <- max(density)

    # compute regular polygon relative coordinates which we will center on points
    radius_mult <- switch(grid, hex = 0.578, 0.5)
    radius_max <- radius_mult * spacing * density_max

    # compute pattern matrix of graphical elements (e.g. fill colors)
    if (is.null(params$pattern_type) || is.na(params$pattern_type))
        params$pattern_type <- switch(grid, square = "square", "hex")
    m_pat <- get_pattern_matrix(params$pattern_type, params$pattern_subtype, grid_xy, n_par)

    gl <- gList()
    for (i_par in seq(n_par)) {
        radius_outer <- radius_mult * spacing * density[i_par]
        xy_rose <- get_xy_rose(frequency[i_par], params, radius_outer, rot[i_par])
        xy_par <- get_xy_par(grid_xy, i_par, m_pat, grid, spacing)
        if (length(xy_par$x) == 0) next

        # rotate by 'angle'
        xy_par <- rotate_xy(xy_par$x, xy_par$y, params$pattern_angle, vpm$x, vpm$y)

        gp <- gpar(fill = fill[i_par], col = col[i_par], lwd = lwd[i_par], lty = lty[i_par])

        name <- paste0("rose.", i_par)
        grob <- points_to_rose_grob(xy_par, xy_rose, gp, default.units, name)
        gl <- append_gList(gl, grob)
    }
    clippee <- gTree(children = gl)
    clipper <- convert_polygon_df_to_polygon_grob(boundary_df, default.units = "bigpts")
    clippingPathGrob(clippee, clipper,
                     use_R4.1_clipping = params$pattern_use_R4.1_clipping,
                     png_device = params$pattern_png_device,
                     res = params$pattern_res, name = "rose")
}

get_xy_rose <- function(frequency, params, radius_outer, rot) {
    theta <- to_radians(seq.int(from = 0, to = 16 * 360, by = 1))
    x <- radius_outer * cos(frequency * theta) * cos(theta)
    y <- radius_outer * cos(frequency * theta) * sin(theta)
    rose_angle <- rot + params$pattern_angle
    rotate_xy(x, y, rose_angle, 0, 0)
}

points_to_rose_grob <- function(xy_par, xy_rose, gp, default.units, name) {
    points_mat <- as.data.frame(xy_par)
    df_polygon <- as.data.frame(xy_rose)
    l_xy <- lapply(seq(nrow(points_mat)),
                   function(i_r) {
                       x0 <- points_mat[i_r, 1]
                       y0 <- points_mat[i_r, 2]
                       df <- df_polygon
                       df$x <- df$x + x0
                       df$y <- df$y + y0
                       df
                   })
    df <- do.call(rbind, l_xy)
    if (is.null(df)) {
        nullGrob()
    } else {
        df$id <- rep(seq(nrow(points_mat)), each = nrow(df_polygon))
        pathGrob(x = df$x, y = df$y, id = df$id,
                    default.units = default.units, gp = gp, name = name)
    }
}
