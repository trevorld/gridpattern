#' Regular polygon patterned grobs
#'
#' `grid.pattern_regular_polygon()` draws a regular polygon pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param rot Angle to rotate regular polygon (degrees, counter-clockwise).
#' @param scale For star polygons, multiplier (between 0 and 1)
#'              applied to exterior radius to get interior radius.
#' @param shape Either "convex" or "star" followed by the number of exterior vertices.
#'              For example `"convex4"` (default) corresponds to a square
#'              whereas `"star6"` corresponds to a six-pointed star.
#' @param type Either `"square"` (default) or `"hex"`.
#'             Adjusts layout, density, and repeating of certain aesthetics to aid in achieving a "tiling" effect.
#'             Note `"hex"` is also good for a layout of triangles.
#'
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     # basic checker pattern
#'     grid.pattern_regular_polygon(x_hex, y_hex, density = 1.0,
#'                                  colour = "black", fill = "blue")
#'     # alternative checker patten
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex,
#'                                  colour = "transparent",
#'                                  fill = c("black", "red", "blue", "yellow"),
#'                                  angle = 0, rot = 45, density = 1.414, spacing = 0.2)
#'     # eight-pointed star tiling
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, colour = "black",
#'                                  fill = c("blue", "yellow"),
#'                                  density = 1.0, spacing = 0.1, shape = "star8")
#'     # hexagon tiling
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, color = "transparent",
#'                                  fill = c("white", "grey", "black"),
#'                                  density = 1.0, spacing = 0.1,
#'                                  shape = "convex6", type = "hex")
#'     # three-pointed star
#'     grid.newpage()
#'     grid.pattern_regular_polygon(x_hex, y_hex, fill = "green",
#'                                  density = 1.0, spacing = 0.1,
#'                                  shape = "star3", type = "hex")
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

    # work in 'bgpoints' instead 'npc' units
    width <- as.numeric(convertWidth(unit(1, "npc"), "bigpts"))
    height <- as.numeric(convertHeight(unit(1, "npc"), "bigpts"))
    longer_dimension <- max(width, height)
    x_vp <- as.numeric(convertX(unit(0.5, "npc"), "bigpts"))
    y_vp <- as.numeric(convertY(unit(0.5, "npc"), "bigpts"))
    bigpts_boundary_df <- boundary_df
    bigpts_boundary_df$x <- as.numeric(convertX(unit(boundary_df$x, "npc"), "bigpts"))
    bigpts_boundary_df$y <- as.numeric(convertY(unit(boundary_df$y, "npc"), "bigpts"))
    spacing <- as.numeric(convertX(unit(params$pattern_spacing, "snpc"), "bigpts"))
    xoffset <- as.numeric(convertX(unit(params$pattern_xoffset, "npc"), "bigpts"))
    yoffset <- as.numeric(convertX(unit(params$pattern_yoffset, "npc"), "bigpts"))

    # create grid of points large enough to cover viewport no matter the angle
    gm <- 0.72 # A bit bigger than sqrt(0.5) for worst case of 45 degree rotation
    x_adjust <- ifelse(params$pattern_type == "hex", 0.5 * spacing, 0)
    x_centers <- xoffset + seq_robust(from = x_vp - gm * longer_dimension,
                                      to = x_vp + gm * longer_dimension + x_adjust,
                                      by = spacing)
    v_spacing <- switch(params$pattern_type, square = 1.0, 0.868) * spacing
    y_centers <- yoffset + seq_robust(from = y_vp - gm * longer_dimension,
                                      to = y_vp + gm * longer_dimension,
                                      by = v_spacing)

    # do this by subsets if certain inputs are vectorized
    fill <- alpha(params$pattern_fill, params$pattern_alpha)
    col  <- alpha(params$pattern_colour, params$pattern_alpha)
    lwd  <- params$pattern_size * .pt
    lty  <- params$pattern_linetype
    n_par <- max(lengths(list(fill, col, lwd, lty)))
    n_par <- max(n_par, round(params$pattern_density + 1, 0)) # prevent overlap error
    fill <- rep(fill, length.out = n_par)
    col <- rep(col, length.out = n_par)
    lwd <- rep(lwd, length.out = n_par)
    lty <- rep(lty, length.out = n_par)

    # create regular polygons centered on points
    radius_outer <- switch(params$pattern_type, square = 0.5, 0.578) * spacing * params$pattern_density
    n_vertices <- get_n_vertices(params$pattern_shape)
    polygon_angle <- 90 + params$pattern_rot + params$pattern_angle
    if (grepl("convex", params$pattern_shape)) {
        xy_polygon <- convex_xy(n_vertices, polygon_angle, radius_outer)
    } else {
        radius_inner <- params$pattern_scale * radius_outer
        xy_polygon <- concave_xy(n_vertices, polygon_angle, radius_outer, radius_inner)
    }

    #### add fudge factor?
    boundary_sf <- convert_polygon_df_to_polygon_sf(bigpts_boundary_df, buffer_dist = 0)
    expanded_sf <- convert_polygon_df_to_polygon_sf(bigpts_boundary_df, buffer_dist = radius_outer)
    contracted_sf <- convert_polygon_df_to_polygon_sf(bigpts_boundary_df, buffer_dist = -radius_outer)

    gl <- gList()
    for (i_par in seq(n_par)) {
        xy_par <- get_xy_par(x_centers, y_centers, i_par, n_par, spacing, params$pattern_type)
        if (length(xy_par$x) == 0) next

        # rotate by 'angle'
        xy_par <- rotate_xy(xy_par$x, xy_par$y, params$pattern_angle, x_vp, y_vp)

        # test if polygons within/near boundary?
        points_sf    <- sf::st_multipoint(as.matrix(as.data.frame(xy_par)))

        all_points_sf      <- sf::st_intersection(expanded_sf  , points_sf)
        interior_points_sf <- sf::st_intersection(contracted_sf, all_points_sf)
        exterior_points_sf <- sf::st_difference(all_points_sf, contracted_sf)

        gp <- gpar(fill = fill[i_par], col = col[i_par], lwd = lwd[i_par], lty = lty[i_par])
        # create grob for interior polygons
        grob <- sf_points_to_polygon_grob(interior_points_sf, xy_polygon, gp)
        gl <- append_gList(gl, grob)

        # create grob for exterior polygons
        polygons_sf <- sf_points_to_sf_multipolygon(exterior_points_sf, xy_polygon)
        exterior_multipolygon <- sf::st_intersection(polygons_sf, boundary_sf)
        grob <- sf_multipolygon_to_polygon_grob(exterior_multipolygon, gp)
        gl <- append_gList(gl, grob)
    }

    gl
}

sf_multipolygon_to_polygon_grob <- function(multipolygons_sf, gp = gpar()) {
    df <- convert_polygon_sf_to_polygon_df(multipolygons_sf)
    if (is.null(df))
        nullGrob()
    else
        polygonGrob(x = df$x, y = df$y, id = df$id, default.units = "bigpts", gp = gp)
}

sf_points_to_polygon_grob <- function(sf_points, xy_polygon, gp = gpar()) {
    points_mat <- as.matrix(sf_points)
    df_polygon <- as.data.frame(xy_polygon)
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
        polygonGrob(x = df$x, y = df$y, id = df$id, default.units = "bigpts", gp = gp)
    }
}

sf_points_to_sf_multipolygon <- function(sf_points, xy_polygon) {
    points_mat <- as.matrix(sf_points)
    df_polygon <- as.data.frame(xy_polygon)
    df_polygon <- rbind(df_polygon, df_polygon[1L, ])
    l_xy <- lapply(seq(nrow(points_mat)),
                   function(i_r) {
                       x0 <- points_mat[i_r, 1]
                       y0 <- points_mat[i_r, 2]
                       df <- df_polygon
                       df$x <- df$x + x0
                       df$y <- df$y + y0
                       list(as.matrix(df))
                   })
    sf::st_multipolygon(l_xy)
}

get_xy_par <- function(x_centers, y_centers, i_par, n_par, spacing = 1, type = "square") {
    x <- numeric(0)
    y <- numeric(0)
    seq_par <- seq(n_par)
    seq_x <- seq_along(x_centers)
    skip <- 0
    for (i_y in seq_along(y_centers)) {
        if (type == "square") {
            i_start <- cycle_elements(seq_par, i_y - 1L)[i_par]
            indices_x <- seq_robust(i_start, length(x_centers), n_par)
            x <- c(x, x_centers[indices_x])
        } else {
            i_start <- cycle_elements(seq_par, skip + i_y - 1)[i_par]
            indices_x <- seq_robust(i_start, length(x_centers), n_par)
            if (i_y %% 2) {
                x_offset <- 0
            } else {
                x_offset <-  -0.5 * spacing
                skip <- skip + 1
            }
            indices_x <- seq_robust(i_start, length(x_centers), n_par)
            x <- c(x,  x_offset + x_centers[indices_x])
        }
        y <- c(y, rep(y_centers[i_y], length(indices_x)))
    }
    list(x = x, y = y)
}
