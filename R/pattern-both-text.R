#' Text character patterned grobs
#'
#' `grid.pattern_text()` draws a text character pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_regular_polygon
#' @inheritParams clippingPathGrob
#' @param shape A character or expression vector.
#'              See `label` argument of [grid::textGrob()] for more details.
#' @param fontfamily The font family.  See [grid::gpar()] for more details.
#' @param fontface The font face.  See [grid::gpar()] for more details.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid") && capabilities("png")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'
#'     playing_card_symbols <- c("\u2660", "\u2665", "\u2666", "\u2663")
#'     grid.newpage()
#'     grid.pattern_text(x_hex, y_hex,
#'                      shape = playing_card_symbols,
#'                      colour = c("black", "red", "red", "black"),
#'                      size = 18, spacing = 0.1, angle = 0)
#'   }
#' @export
grid.pattern_text <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                              colour = gp$col %||% "grey20",
                              angle = 30,
                              spacing = 0.05,
                              xoffset = 0, yoffset = 0,
                              scale = 0.5,
                              shape = "X",
                              grid = "square", type = NULL, subtype = NULL, rot = 0,
                              alpha = gp$alpha %||% NA_real_,
                              size = gp$fontsize %||% 12,
                              fontfamily = gp$fontfamily %||% "sans",
                              fontface = gp$fontface %||% "plain",
                              use_R4.1_clipping = getOption("ggpattern_use_R4.1_clipping",
                                                            getOption("ggpattern_use_R4.1_features")),
                              png_device = NULL, res = getOption("ggpattern_res", 72),
                              default.units = "npc", name = NULL,
                              gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("text", x, y, id,
                 colour = colour, angle = angle,
                 spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 scale = scale, shape = shape,
                 grid = grid, type = type, subtype = subtype, rot = rot,
                 alpha = alpha, size = size, fontfamily = fontfamily, fontface = fontface,
                 use_R4.1_clipping = use_R4.1_clipping, png_device = png_device, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_text <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    # work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
    default.units <- "bigpts"
    boundary_df <- convert_polygon_df_units(boundary_df, default.units)
    params <- convert_params_units(params, default.units)
    vpm <- get_vp_measurements(default.units)

    spacing <- params$pattern_spacing
    grid <- params$pattern_grid

    # create grid of points large enough to cover viewport no matter the angle
    grid_xy <- get_xy_grid(params, vpm)

    # vectorize fill, col, lwd, lty, density, rot, and shape
    col  <- alpha(params$pattern_colour, params$pattern_alpha)
    fontsize  <- params$pattern_size
    fontfamily <- params$pattern_fontfamily
    fontface <- params$pattern_fontface

    rot <- params$pattern_rot + params$pattern_angle
    shape <- params$pattern_shape

    n_par <- max(lengths(list(col, fontsize, fontfamily, fontface, rot, shape)))

    col <- rep(col, length.out = n_par)
    fontsize <- rep(fontsize, length.out = n_par)
    fontfamily <- rep(fontfamily, length.out = n_par)
    fontface <- rep(fontface, length.out = n_par)
    rot <- rep(rot, length.out = n_par)
    shape <- rep(shape, length.out = n_par)

    # compute pattern matrix of graphical elements (e.g. fill colors)
    if (is.null(params$pattern_type) || is.na(params$pattern_type))
        params$pattern_type <- switch(grid, square = "square", "hex")
    m_pat <- get_pattern_matrix(params$pattern_type, params$pattern_subtype, grid_xy, n_par)

    gl <- gList()
    for (i_par in seq(n_par)) {
        if (shape[i_par] == "null") next
        xy_par <- get_xy_par(grid_xy, i_par, m_pat, grid, spacing)
        if (length(xy_par$x) == 0) next

        # rotate by 'angle'
        xy_par <- rotate_xy(xy_par$x, xy_par$y, params$pattern_angle, vpm$x, vpm$y)

        gp <- gpar(col = col[i_par], fontsize = fontsize[i_par],
                   fontfamily = fontfamily[i_par], fontface = fontface[i_par])

        # create grob for interior polygons
        name <- paste0("text.", i_par)

        grob <- textGrob(label = shape[i_par], x = xy_par$x, y = xy_par$y,
                         rot = rot[i_par], just = "center", default.units = "bigpts",
                         name = name, gp = gp)

        gl <- append_gList(gl, grob)
    }
    clippee <- gTree(children = gl)
    clipper <- convert_polygon_df_to_polygon_grob(boundary_df, default.units = "bigpts")
    png_device <- params$pattern_png_device
    if (is.null(png_device) && requireNamespace("ragg", quietly = TRUE))
        png_device <- ragg::agg_png # more robust cross-platform Unicode support
    clippingPathGrob(clippee, clipper,
                     use_R4.1_clipping = params$pattern_use_R4.1_clipping,
                     png_device = png_device,
                     res = params$pattern_res, name = "text")
}
