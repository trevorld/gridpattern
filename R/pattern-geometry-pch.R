#' Plotting character patterned grobs
#'
#' `grid.pattern_pch()` draws a plotting character pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_regular_polygon
#' @param shape An integer from `0` to `25` or `NA`.
#'              See [graphics::points()] for more details.
#'              Note we only support these shapes and do not
#'              support arbitrary ASCII / Unicode characters.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @seealso [grid.pattern_regular_polygon()] which is used to implement this pattern.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     gp <- gpar(col = "black", fill = "lightblue")
#'
#'     # pch 0-6 are simple shapes with no fill
#'     grid.pattern_pch(x_hex, y_hex, shape = 0:6, gp = gp,
#'                      spacing = 0.1, density = 0.4, angle = 0)
#'
#'     # pch 7-14 are compound shapes with no fill
#'     grid.newpage()
#'     grid.pattern_pch(x_hex, y_hex, shape = 7:14, gp = gp,
#'                      spacing = 0.1, density = 0.4, angle = 0)
#'
#'     # pch 15-20 are filled with 'col'
#'     grid.newpage()
#'     grid.pattern_pch(x_hex, y_hex, shape = 15:20, gp = gp,
#'                      spacing = 0.1, density = 0.4, angle = 0)
#'
#'     # pch 21-25 are filled with 'fill'
#'     grid.newpage()
#'     grid.pattern_pch(x_hex, y_hex, shape = 21:25, gp = gp,
#'                      spacing = 0.1, density = 0.4, angle = 0)
#'
#'     # using a 'basket' weave `type` with two shapes
#'     grid.newpage()
#'     grid.pattern_pch(x_hex, y_hex, shape = c(1,4), gp = gp,
#'                      type = "basket",
#'                      spacing = 0.1, density = 0.4, angle = 0)
#'   }
#' @export
grid.pattern_pch <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                         colour = gp$col %||% "grey20",
                                         fill = gp$fill %||% "grey80",
                                         angle = 30, density = 0.2,
                                         spacing = 0.05, xoffset = 0, yoffset = 0,
                                         scale = 0.5, shape = 1L,
                                         grid = "square", type = NULL, subtype = NULL, rot = 0,
                                         alpha = gp$alpha %||% NA_real_,
                                         linetype = gp$lty %||% 1,
                                         linewidth = size %||% gp$lwd %||% 1,
                                         size = NULL,
                                         default.units = "npc", name = NULL,
                                         gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("pch", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 scale = scale, shape = shape,
                 grid = grid, type = type, subtype = subtype, rot = rot,
                 alpha = alpha, linetype = linetype, linewidth = linewidth,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

# each pch will be represented by two regular polygons (although one may be "null")
create_pattern_pch <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    # vectorize fill, col, lwd, lty, density, rot, and shape
    fill <- update_alpha(params$pattern_fill, params$pattern_alpha)
    col  <- update_alpha(params$pattern_colour, params$pattern_alpha)
    lwd  <- params$pattern_linewidth
    lty  <- params$pattern_linetype
    params$pattern_alpha <- NA_real_

    density <- params$pattern_density
    rot <- params$pattern_rot
    shape <- params$pattern_shape

    n_par <- max(lengths(list(fill, col, lwd, lty, density, rot, shape)))

    fill <- rep_len_fill(fill, n_par)
    col <- rep_len(col, n_par)
    lwd <- rep_len(lwd, n_par)
    lty <- rep_len(lty, n_par)
    density <- rep_len(density, n_par)
    rot <- rep_len(rot, n_par)
    shape <- rep_len(shape, n_par)

    # setup bottom and top regular polygons
    pint <- as.integer(shape)
    if (!all(is.na(pint)))
        stopifnot(any(na_omit(pint) >= 0), any(na_omit(pint) <= 25))
    pch <- ifelse(is.na(pint), "26", as.character(pint))
    pint <- ifelse(is.na(pint), 26L, pint)

    density1 <- ifelse(pint == 4L, 1.414 * density, density)
    density1 <- ifelse(pint == 20L, 2/3 * density, density1)

    density2 <- ifelse(pint == 7L | pint == 13L, 1.414 * density, density)

    fill <- ifelse(pint < 21L, col, fill)
    fill <- ifelse(pint < 15L, NA_character_, fill)

    col <- ifelse(pint > 14L & pint < 19L, NA_character_, col)

    rot1 <- rot + sapply(pch, get_rot_base)
    rot2 <- rot + sapply(pch, get_rot_top)

    shape1 <- sapply(pch, get_shape_base)
    shape2 <- sapply(pch, get_shape_top)

    params$pattern_fill <- fill
    params$pattern_col <- col
    params$pattern_linewidth <- lwd
    params$pattern_linetype <- lty
    params$pattern_scale <- 0.001
    params_base <- params_top <- params

    # bottom regular polygon
    params_base$pattern_shape <- shape1
    params_base$pattern_rot <- rot1
    params_base$pattern_density <- density1
    grob_base <- create_pattern_regular_polygon_via_sf(params_base, boundary_df, aspect_ratio, legend)
    grob_base <- editGrob(grob_base, name = "pch_base")

    # top regular polygon
    params_top$pattern_shape <- shape2
    params_top$pattern_rot <- rot2
    params_top$pattern_density <- density2
    grob_top <- create_pattern_regular_polygon_via_sf(params_top, boundary_df, aspect_ratio, legend)
    grob_top <- editGrob(grob_top, name = "pch_top")

    gl <- gList(grob_base, grob_top)

    gTree(children = gl, name = "pch")
}

get_rot_base <- function(pch) {
    switch(pch,
           "4" = 45,
           "6" = 180,
           "25" = 180,
            0)
}

get_rot_top <- function(pch) {
    switch(pch,
           "7" = 45,
           "11" = 180,
           "13" = 45,
            0)
}

get_shape_base <- function(pch) {
    switch(pch,
           "0" = "square",
           "2" = "convex3",
           "3" = "star4",
           "4" = "star4",
           "5" = "convex4",
           "6" = "convex3",
           "7" = "square",
           "9" = "convex4",
           "8" = "star8",
           "11" = "convex3",
           "12" = "square",
           "14" = "square",
           "15" = "square",
           "17" = "convex3",
           "18" = "convex4",
           "22" = "square",
           "23" = "convex4",
           "24" = "convex3",
           "25" = "convex3",
           "26" = "null",
           "circle")
}

get_shape_top <- function(pch) {
    switch(pch,
           "7" = "star4",
           "9" = "star4",
           "10" = "star4",
           "11" = "convex3",
           "12" = "star4",
           "13" = "star4",
           "14" = "convex3",
           "null")
}

na_omit <- function(x) Filter(Negate(is.na), x)
