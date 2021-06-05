#' Regular polygon patterned grobs
#'
#' `grid.pattern_regular_polygon()` draws a regular polygon pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param rot Angle to rotate regular polygon (degrees, counter-clockwise).
#' @param scale For star polygons, multiplier (between 0 and 1)
#'              applied to exterior radius to get interior radius.
#' @param shape Either "convex" or "star" followed by the number of exterior vertices
#'              or alternatively "circle" or "square".
#'              For example `"convex5"` (default) corresponds to a pentagon
#'              and `"star6"` corresponds to a six-pointed star.
#'              The `"square"` shape is larger than the `"convex4"` shape and is rotated an extra 45 degrees,
#'              it can be used to generate a multi-colored "checkers" effect when density is 1.
#'
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @seealso [grid.pattern_circle()] for a special case of this pattern.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'
#'     # 'density', 'rot', and 'shape' are vectorized
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, colour = "black",
#'                                  fill = c("blue", "yellow", "red"),
#'                                  shape = c("convex4", "star8", "circle"),
#'                                  density = c(0.45, 0.42, 0.4),
#'                                  spacing = 0.08, angle = 0)
#'
#'     # checker pattern using "square" shape
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, shape = "square",
#'                                  colour = "transparent",
#'                                  fill = c("black", "red", "blue", "yellow"),
#'                                  angle = 0, density = 1.0, spacing = 0.2)
#'
#'     # checker pattern using the default "convex4" shape
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, density = 1.0,
#'                                  colour = "black", fill = "blue")
#'
#'     # hexagon tiling
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, color = "transparent",
#'                                  fill = c("white", "grey", "black"),
#'                                  density = 1.0, spacing = 0.1,
#'                                  shape = "convex6", type = "hex")
#'
#'     # triangle tiling
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, fill = "green",
#'                                  density = 1.0, spacing = 0.1,
#'                                  shape = "convex3", type = "hex")
#'
#'   }
#' @export
grid.pattern_regular_polygon <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                         colour = gp$col %||% "grey20",
                                         fill = gp$fill %||% "grey80",
                                         angle = 30, density = 0.2,
                                         spacing = 0.05, xoffset = 0, yoffset = 0,
                                         scale = 0.5, shape = "convex4", type = "square", rot = 0,
                                         alpha = gp$alpha %||% NA_real_, linetype = gp$lty %||% 1,
                                         size = gp$lwd %||% 1,
                                         default.units = "npc", name = NULL,
                                         gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("regular_polygon", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 scale = scale, shape = shape, type = type, rot = rot,
                 alpha = alpha, linetype = linetype, size = size,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_regular_polygon_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    # work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
    default.units <- "bigpts"
    boundary_df <- convert_polygon_df_units(boundary_df, default.units)
    params <- convert_params_units(params, default.units)
    vpm <- get_vp_measurements(default.units)

    spacing <- params$pattern_spacing

    # create grid of points large enough to cover viewport no matter the angle
    grid_xy <- get_xy_grid(params, vpm)

    # construct grobs using subsets if certain inputs are vectorized
    fill <- alpha(params$pattern_fill, params$pattern_alpha)
    col  <- alpha(params$pattern_colour, params$pattern_alpha)
    lwd  <- params$pattern_size * .pt
    lty  <- params$pattern_linetype

    density <- params$pattern_density
    rot <- params$pattern_rot
    shape <- params$pattern_shape

    n_par <- max(lengths(list(fill, col, lwd, lty, density, rot, shape)))

    fill <- rep(fill, length.out = n_par)
    col <- rep(col, length.out = n_par)
    lwd <- rep(lwd, length.out = n_par)
    lty <- rep(lty, length.out = n_par)
    density <- rep(density, length.out = n_par)
    rot <- rep(rot, length.out = n_par)
    shape <- rep(shape, length.out = n_par)

    density <- ifelse(shape == "square", 1.414 * density, density)
    # avoid overlap errors when density == 1 due to machine precision issues
    if (params$pattern_type == "square")
        density <- ifelse(abs(density - 1) < .Machine$double.eps^0.5, 0.9999, density)
    if (params$pattern_type == "hex" && n_par < 2)
        density <- ifelse(abs(density - 1) < .Machine$double.eps^0.5, 0.994, density)
    density_max <- max(density)

    # compute regular polygon relative coordinates which we will center on points
    radius_mult <- switch(params$pattern_type, hex = 0.578, 0.5)
    radius_max <- radius_mult * spacing * density_max

    #### add fudge factor?
    boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)
    expanded_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = radius_max)
    contracted_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = -radius_max)

    gl <- gList()
    for (i_par in seq(n_par)) {
        radius_outer <- radius_mult * spacing * density[i_par]
        xy_polygon <- get_xy_polygon(shape[i_par], params, radius_outer, rot[i_par])
        xy_par <- get_xy_par(grid_xy, i_par, n_par, spacing, params$pattern_type)
        if (length(xy_par$x) == 0) next

        # rotate by 'angle'
        xy_par <- rotate_xy(xy_par$x, xy_par$y, params$pattern_angle, vpm$x, vpm$y)

        # test if polygons within/near boundary
        points_sf    <- sf::st_multipoint(as.matrix(as.data.frame(xy_par)))
        all_points_sf      <- sf::st_intersection(expanded_sf, points_sf)
        interior_points_sf <- sf::st_intersection(all_points_sf, contracted_sf)
        exterior_points_sf <- sf::st_difference(all_points_sf, contracted_sf)

        gp <- gpar(fill = fill[i_par], col = col[i_par], lwd = lwd[i_par], lty = lty[i_par])

        # create grob for interior polygons
        name <- paste0("interior.", i_par)
        if (shape[i_par] == "circle") {
            grob <- sf_points_to_circle_grob(interior_points_sf, radius_outer,
                                             gp, default.units, name)
        } else {
            grob <- sf_points_to_polygon_grob(interior_points_sf, xy_polygon,
                                              gp, default.units, name)
        }
        gl <- append_gList(gl, grob)

        # create grob for exterior polygons
        polygons_sf <- sf_points_to_sf_multipolygon(exterior_points_sf, xy_polygon)
        exterior_multipolygon <- sf::st_intersection(polygons_sf, boundary_sf)
        name <- paste0("boundary.", i_par)
        grob <- sf_multipolygon_to_polygon_grob(exterior_multipolygon,
                                                gp, default.units, name)
        gl <- append_gList(gl, grob)
    }
    gTree(children = gl, name = "regular_polygon")
}

get_xy_par <- function(grid_xy, i_par, n_par, spacing = 1, type = "square") {
    x <- numeric(0)
    y <- numeric(0)
    seq_par <- seq(n_par)
    seq_x <- seq_along(grid_xy$x)
    skip <- 0
    for (i_y in seq_along(grid_xy$y)) {
        if (type == "square") {
            n_cycle <- i_y - 1L
            i_start <- cycle_elements(seq_par, -n_cycle)[i_par]
            indices_x <- seq_robust(i_start, length(grid_xy$x), n_par)
            x <- c(x, grid_xy$x[indices_x])
        } else {
            n_cycle <- skip + i_y - 1
            i_start <- cycle_elements(seq_par, -n_cycle)[i_par]
            indices_x <- seq_robust(i_start, length(grid_xy$x), n_par)
            if (i_y %% 2) {
                x_offset <- 0
            } else {
                x_offset <-  -0.5 * spacing
                skip <- skip + 1
            }
            indices_x <- seq_robust(i_start, length(grid_xy$x), n_par)
            x <- c(x,  x_offset + grid_xy$x[indices_x])
        }
        y <- c(y, rep(grid_xy$y[i_y], length(indices_x)))
    }
    list(x = x, y = y)
}

# create grid of points large enough to cover viewport no matter the angle
get_xy_grid <- function(params, vpm) {
    spacing <- params$pattern_spacing
    xoffset <- params$pattern_xoffset
    yoffset <- params$pattern_yoffset

    gm <- 1.00 # seems to need to be this big so {ggpattern} legends render correctly
    x_adjust <- switch(params$pattern_type, hex = 0.5 * spacing, 0)
    x_min <- vpm$x - (gm * vpm$length + x_adjust)
    x_max <- vpm$x + (gm * vpm$length + x_adjust)
    x <- xoffset + seq_robust(from = x_min, to = x_max, by = spacing)

    # adjust vertical spacing for "hex" pattern
    v_spacing <- switch(params$pattern_type, square = 1.0, 0.868) * spacing
    y_min <- vpm$y - gm * vpm$length
    y_max <- vpm$y + gm * vpm$length
    y <- yoffset + seq_robust(from = y_min, to = y_max, by = v_spacing)

    list(x = x, y = y,
         x_min = x_min, x_max = x_max, y_min = y_min, y_max = y_max,
         h_spacing = spacing, v_spacing = v_spacing
    )
}

get_xy_polygon <- function(shape, params, radius_outer, rot) {
    if (shape == "square") {
        shape <- "convex4"
        rot <- rot + 45
    }
    polygon_angle <- 90 + rot + params$pattern_angle
    if (shape == "circle") {
        # grid::grobPoints.circle() defaults to regular polygon with 100 vertices
        convex_xy(100, polygon_angle, radius_outer)
    } else if (grepl("convex", shape)) {
        n_vertices <- get_n_vertices(shape)
        convex_xy(n_vertices, polygon_angle, radius_outer)
    } else {
        n_vertices <- get_n_vertices(shape)
        radius_inner <- params$pattern_scale * radius_outer
        concave_xy(n_vertices, polygon_angle, radius_outer, radius_inner)
    }
}
