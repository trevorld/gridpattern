#' Polygon tiling patterned grobs
#'
#' `grid.pattern_polygon_tiling()` draws a specified polygon tiling pattern onto the graphic device.
#'  `names_polygon_tiling` lists all supported types.
#'
#' `grid.pattern_polygon_tiling()` supports 1, 2, or 3 `fill` colors with the first colors (weakly)
#'  covering a larger area.  Size of the pattern is controlled by `spacing`.
#'  We support the following polygon tiling `type`s:
#'
#' \describe{
#' \item{herringbone}{Creates a herringbone tiling made of rectangles.}
#' \item{hexagonal}{Creates a hexagonal tiling made of hexagons.}
#' \item{pythagorean}{Creates a Pythagorean tiling made of squares of two different sizes.}
#' \item{rhombille}{Creates a rhombille tiling made of rhombi.}
#' \item{rhombitrihexagonal}{Creates a rhombitrihexagonal tiling made out of
#'                           dodecagons, hexagons, and squares.}
#' \item{snub_square}{Creates a snub square tiling made of squares and triangles.}
#' \item{snub_trihexagonal}{Creates a snub trihexagonal tiling made of hexagons and triangles.}
#' \item{square}{Creates a square tiling made of squares.}
#' \item{tetrakis_square}{Creates a tetrakis square tiling made of isosceles right triangles.}
#' \item{triangular}{Creates a triangular tiling made of equilateral triangles.}
#' \item{trihexagonal}{Creates a trihexagonal tiling made of hexagons and triangles.}
#' \item{truncated_square}{Creates a truncated square tiling made of octagons and squares.}
#' \item{truncated_hexagonal}{Creates a truncated hexagonal tiling made of dodecagons and triangles.}
#' \item{truncated_trihexagonal}{Creates a truncated trihexagonal tiling made of hexagons, squares, and triangles.}
#' \item{`3.3.8*15.3.4.3.8*15`}{Creates a regular (star) polygon tiling made of triangles, squares, and eight-pointed stars.}
#' \item{`3.4.8.3.8*15`}{Creates a regular (star) polygon tiling made of triangles, squares, octagons, and eight-pointed stars.}
#' \item{`4.6*30.4.6*30.4.6*30`}{Creates a regular (star) polygon tiling made of squares and six-pointed stars.}
#' }
#'
#' @inheritParams grid.pattern_circle
#' @param alpha Alpha (between 0 and 1) or `NA` (default, preserves colors' alpha value).
#'              Not supported for all polygon tiling `type`.
#' @param type Name of polygon tiling to draw.  See Details.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @seealso The tiling vignette `vignette("tiling", package = "gridpattern")` for more
#'          information about these tilings as well as more
#'          examples of polygon tiling using the [grid.pattern_regular_polygon()] function.
#' @examples
#'  print(names_polygon_tiling)
#'  if (require("grid")) {
#'    x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    gp1 <- gpar(fill = "yellow", col = "black")
#'    gp2 <- gpar(fill = c("yellow", "red"), col = "black")
#'    gp3 <- gpar(fill = c("yellow", "red", "blue"), col = "black")
#'
#'    grid.pattern_polygon_tiling(x_hex, y_hex, type = "herringbone", gp = gp1)
#'
#'    grid.newpage()
#'    grid.pattern_polygon_tiling(x_hex, y_hex, type = "hexagonal", gp = gp3)
#'
#'    grid.newpage()
#'    grid.pattern_polygon_tiling(x_hex, y_hex, type = "pythagorean", gp = gp2)
#'
#'    grid.newpage()
#'    grid.pattern_polygon_tiling(x_hex, y_hex, type = "truncated_square", gp = gp3)
#'
#'    grid.newpage()
#'    grid.pattern_polygon_tiling(x_hex, y_hex, type = "rhombille", gp = gp3)
#'  }
#'
#' @export
grid.pattern_polygon_tiling <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                        colour = gp$col %||% "grey20",
                                        fill = gp$fill %||% "grey80",
                                        angle = 30, spacing = 0.05, xoffset = 0, yoffset = 0,
                                        type = "square",
                                        alpha = gp$alpha %||% NA_real_, linetype = gp$lty %||% 1,
                                        size = gp$lwd %||% 1,
                                        default.units = "npc", name = NULL,
                                        gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("polygon_tiling", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 type = type,
                 alpha = alpha, linetype = linetype, size = size,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#' @rdname grid.pattern_polygon_tiling
#' @export
names_polygon_tiling <- c("herringbone",
                          "hexagonal",
                          "pythagorean",
                          "rhombitrihexagonal",
                          "rhombille",
                          "snub_square",
                          "snub_trihexagonal",
                          "square",
                          "tetrakis_square",
                          "triangular",
                          "trihexagonal",
                          "truncated_square",
                          "truncated_hexagonal",
                          "truncated_trihexagonal",
                          "3.3.8*15.3.4.3.8*15",
                          "3.4.8.3.8*15",
                          "4.6*30.4.6*30.4.6*30")

create_pattern_polygon_tiling <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    type <- tolower(params$pattern_type)

    xyi <- boundary_df

    fill <- alpha(params$pattern_fill, params$pattern_alpha)
    col <- alpha(params$pattern_colour, params$pattern_alpha)
    lwd <- params$pattern_size
    lty <- params$pattern_linetype
    stopifnot(length(fill) < 4L, max(lengths(list(col, lwd, lty))) == 1L)
    gp <- gpar(fill = fill, col = col, lwd = lwd, lty = lty)

    angle <- params$pattern_angle
    spacing <- params$pattern_spacing

    gl <- switch(type,
                 herringbone = create_herringbone_tiling(xyi, gp, spacing, angle),
                 hexagonal = create_hexagonal_tiling(xyi, gp, spacing, angle),
                 pythagorean = create_pythagorean_tiling(xyi, gp, spacing, angle),
                 snub_square = create_snub_square_tiling(xyi, gp, spacing, angle),
                 snub_trihexagonal = create_snub_trihex_tiling(xyi, gp, spacing, angle),
                 square = create_square_tiling(xyi, gp, spacing, angle),
                 rhombille = create_rhombille_tiling(xyi, gp, spacing, angle),
                 rhombitrihexagonal = create_rhombitrihexagonal_tiling(xyi, gp, spacing, angle),
                 tetrakis_square = create_tetrakis_tiling(xyi, gp, spacing, angle),
                 triangular = create_triangular_tiling(xyi, gp, spacing, angle),
                 trihexagonal = create_trihexagonal_tiling(xyi, gp, spacing, angle),
                 truncated_hexagonal = create_trunc_hex_tiling(xyi, gp, spacing, angle),
                 truncated_square = create_trunc_square_tiling(xyi, gp, spacing, angle),
                 truncated_trihexagonal = create_trunc_trihex_tiling(xyi, gp, spacing, angle),
                 `3.3.8*15.3.4.3.8*15` = create_3.3.8_15.3.4.3.8_15_tiling(xyi, gp, spacing, angle),
                 `3.4.8.3.8*15` = create_3.4.8.3.8_15_tiling(xyi, gp, spacing, angle),
                 `4.6*30.4.6*30.4.6*30` = create_4.6_30.4.6_30.4.6_30_tiling(xyi, gp, spacing, angle),
                 abort(paste("Don't know how to do tiling", type)))
    gTree(children = gl, name = "polygon_tiling")
}

create_3.3.8_15.3.4.3.8_15_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_star <- gp_oct <- gp_bg <- gp
    if (n_col == 2L) {
        gp_bg$fill <- gp_star$fill <- gp$fill[1L]
        gp_oct$fill <- gp$fill[2L]
    } else if (n_col == 3L) {
        gp_bg$fill <- gp$fill[3]
        gp_oct$fill <- gp$fill[1]
        gp_star$fill <- gp$fill[2]
    }
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    octagons <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                            shape = "convex8", density = 1.082, rot = 22.5,
                            spacing = spacing, angle = angle, gp = gp_oct,
                            name = "octagons")
    scale <- star_scale(8, 60, external = TRUE)
    stars <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                         shape = "star8", density = 1.082, rot = 22.5,
                         spacing = spacing, angle = angle, gp = gp_star,
                         scale = scale, name = "stars")
    gList(bg, octagons, stars)
}

create_3.4.8.3.8_15_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_star <- gp_oct <- gp_bg <- gp
    if (n_col == 2L) {
        gp_bg$fill <- gp_star$fill <- gp$fill[2L]
        gp_oct$fill <- gp$fill[1L]
    } else if (n_col == 3L) {
        gp_bg$fill <- gp$fill[2]
        gp_oct$fill <- gp$fill[c(1,3)]
        gp_star$fill <- gp$fill[2]
    }
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    octagons <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                            shape = "convex8", density = 1.082, rot = 22.5,
                            spacing = spacing, angle = angle, gp = gp_oct,
                            name = "octagons")
    scale <- star_scale(8, 60, external = TRUE)
    stars <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                         shape = c("null", "star8"), density = 1.082, rot = 22.5,
                         spacing = spacing, angle = angle, gp = gp_star,
                         scale = scale, name = "stars")
    gList(bg, octagons, stars)
}

create_4.6_30.4.6_30.4.6_30_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_star <- gp_bg <- gp
    gp_bg$fill <- gp$fill[1L]
    if (n_col > 1L)
        gp_star$fill <- rev(gp$fill[-1L])
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    scale <- star_scale(6, 30)
    stars <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "star6", density = 1, grid = "hex",
                        spacing = spacing, angle = angle, gp = gp_star,
                        scale = scale, name = "stars")
    gList(bg, stars)
}

create_herringbone_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    fill2 <- gp$fill[1L]
    if (n_col > 1L)
        gp$fill <- gp$fill[-1L]
    grob <- patternGrob("weave", xyi$x, xyi$y, xyi$id,
                        type = "twill", subtype = "2/2(1)",
                        density = 1, fill2 = fill2,
                        spacing = spacing, angle = angle + 45, gp = gp,
                        name = "rectangles")
    gList(grob)
}

create_hexagonal_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    if (n_col == 2L)
        gp$fill <- rev(gp$fill)
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex6", density = 1, grid = "hex",
                        spacing = spacing, angle = angle, gp = gp,
                        name = "hexagons")
    gList(grob)
}

create_pythagorean_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_bg$fill <- gp$fill[n_col]
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    if (n_col == 2L)
        gp$fill <- gp$fill[1L]
    else if (n_col == 3L)
        gp$fill <- gp$fill[2:1]
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex4", density = 1.222, rot = 15,
                        spacing = spacing, angle = angle - 60, gp = gp,
                        name = "squares_larger")
    gList(bg, grob)
}

create_rhombitrihexagonal_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_sq <- gp_bg
    gp_dd <- gp_bg
    gp_bg$fill <- gp$fill[n_col]
    if (n_col == 2L) {
        gp_sq$fill <- gp$fill[1L]
        gp_dd$fill <- gp$fill[1L]
    }
    else if (n_col == 3L) {
        gp_bg$fill <- gp$fill[2L]
        gp_sq$fill <- gp$fill[3L]
        gp_dd$fill <- gp$fill[1L]
    }
    # triangles
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    # squares
    stripe1 <- patternGrob("stripe", xyi$x, xyi$y, xyi$id,
                           angle = angle,
                           grid = "hex_circle", density = 0.25,
                           spacing = spacing,
                           gp = gp_sq, name = "square_stripes.1")
    stripe2 <- patternGrob("stripe", xyi$x, xyi$y, xyi$id,
                           angle = angle + 60,
                           grid = "hex_circle", density = 0.25,
                           spacing = spacing,
                           gp = gp_sq, name = "square_stripes.2")
    stripe3 <- patternGrob("stripe", xyi$x, xyi$y, xyi$id,
                           angle = angle - 60,
                           grid = "hex_circle", density = 0.25,
                           spacing = spacing,
                           gp = gp_sq, name = "square_stripes.3")
    # hexagons
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex12", density = 0.82, rot = 15,
                        grid = "hex_circle",
                        spacing = spacing, angle = angle, gp = gp_dd,
                        name = "dodecagons")
    gList(bg, stripe1, stripe2, stripe3, grob)
}

create_snub_square_tiling <- function(xyi, gp, spacing, angle) {
    scale_star <- star_scale(4, 90 + 60, external = TRUE)
    n_col <- length(gp$fill)
    gp_sq <- gp_sq2 <- gp_tri <- gp
    if (n_col == 2) {
        gp_sq2$fill <- gp_sq$fill <- gp$fill[1L]
        gp_tri$fill <- gp$fill[2L]
    } else if (n_col == 3) {
        gp_tri$fill <- gp$fill[1L]
        gp_sq$fill <- gp$fill[2L]
        gp_sq2$fill <- gp$fill[3L]
    }
    sq1 <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc",
                       gp = gp_sq, name = "squares.1")
    tri <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                       shape = "star4", scale = scale_star,
                       angle = angle, rot = 15, spacing = spacing,
                       density = 1.41, gp = gp_tri, name = "triangles")
    sq2 <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                       shape = "convex4", scale = scale_star,
                       angle = angle, rot = 60, spacing = spacing,
                       density = scale_star * 1.41, gp = gp_sq2, name = "squares.2")
    gList(sq1, tri, sq2)
}

create_snub_trihex_tiling <- function(xyi, gp, spacing, angle) {
    scale_star <- star_scale(6, 60 + 60, external = TRUE)
    n_col <- length(gp$fill)
    gp_tri1 <- gp_tri2 <- gp_hex <- gp
    if (n_col == 2) {
        gp_tri1$fill <- gp_tri2$fill <- gp$fill[1L]
        gp_hex$fill <- gp$fill[2L]
    } else if (n_col == 3) {
        gp_tri2$fill <- gp$fill[1L]
        gp_hex$fill <- gp$fill[2L]
        gp_tri1$fill <- gp$fill[3L]
    }
    tri1 <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc",
                        gp = gp_tri1, name = "triangles.1")
    tri2 <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "star6", scale = scale_star,
                        grid = "hex_circle",
                        angle = angle, rot = 19, spacing = spacing,
                        density = 1.305, gp = gp_tri2, name = "triangles.2")
    hex <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                       shape = "convex6", grid = "hex_circle",
                       angle = angle, rot = 49, spacing = spacing,
                       density = scale_star * 1.305, gp = gp_hex, name = "hexagons")
    gList(tri1, tri2, hex)
}

create_square_tiling <- function(xyi, gp, spacing, angle) {
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "square", density = 1,
                        spacing = spacing, angle = angle, gp = gp,
                        name = "squares")
    gList(grob)
}
create_rhombille_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_rh1 <- gp_rh2 <- gp_rh3 <- gp
    if (n_col == 2) {
        gp_rh1$fill <- gp_rh2$fill <- gp$fill[1L]
        gp_rh3$fill <- gp$fill[2L]
    } else if (n_col == 3) {
        gp_rh1$fill <- gp$fill[1L]
        gp_rh2$fill <- gp$fill[2L]
        gp_rh3$fill <- gp$fill[3L]
    }
    rh1 <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                       shape = "rhombille_rhombus", grid = "hex",
                       angle = angle, rot = -120, spacing = spacing, density = 1,
                       gp = gp_rh1, name = "rhombi.1")
    rh2 <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                       shape = "rhombille_rhombus", grid = "hex",
                       angle = angle, rot = 120, spacing = spacing, density = 1,
                       gp = gp_rh2, name = "rhombi.2")
    rh3 <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                       shape = "rhombille_rhombus", grid = "hex",
                       angle = angle, rot = 0, spacing = spacing, density = 1,
                       gp = gp_rh3, name = "rhombi.3")
    gList(rh1, rh2, rh3)
}
create_tetrakis_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_tri1 <- gp_tri2 <- gp_tri3 <- gp
    if (n_col == 2) {
        gp_tri1$fill <- gp$fill[1L]
        gp_tri2$fill <- gp_tri3$fill <- gp$fill[2L]
    } else if (n_col == 3) {
        gp_tri1$fill <- gp$fill[1L]
        gp_tri2$fill <- gp$fill[2L]
        gp_tri3$fill <- gp$fill[3L]
    }
    tri1.a <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_left",
                        angle = angle, rot = 0,
                        spacing = spacing, density = 1,
                        gp = gp_tri1, name = "triangles.1.a")
    tri1.b <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_left",
                        angle = angle, rot = 90,
                        spacing = spacing, density = 1,
                        gp = gp_tri1, name = "triangles.1.b")
    tri1.c <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_left",
                        angle = angle, rot = 180,
                        spacing = spacing, density = 1,
                        gp = gp_tri1, name = "triangles.1.c")
    tri1.d <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_left",
                        angle = angle, rot = 270,
                        spacing = spacing, density = 1,
                        gp = gp_tri1, name = "triangles.1.d")
    tri2.a <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_right",
                        angle = angle, rot = 0,
                        spacing = spacing, density = 1,
                        gp = gp_tri2, name = "triangles.2.a")
    tri2.b <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_right",
                        angle = angle, rot = 180,
                        spacing = spacing, density = 1,
                        gp = gp_tri2, name = "triangles.2.b")
    tri3.a <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_right",
                        angle = angle, rot = 90,
                        spacing = spacing, density = 1,
                        gp = gp_tri3, name = "triangles.3.a")
    tri3.b <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "tetrakis_right",
                        angle = angle, rot = 270,
                        spacing = spacing, density = 1,
                        gp = gp_tri3, name = "triangles.3.b")
    gList(tri1.a, tri1.b, tri1.c, tri1.d, tri2.a, tri2.b, tri3.a, tri3.b)
}

create_triangular_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_bg$fill <- gp$fill[1]
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    if (n_col == 2L)
        gp$fill <- gp$fill[2L]
    else if (n_col == 3L)
        gp$fill <- gp$fill[3:2]
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex3", density = 1, grid = "hex", rot = 180,
                        spacing = spacing, angle = angle, gp = gp,
                        name = "triangles")
    gList(bg, grob)
}

create_trunc_hex_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_bg$fill <- gp$fill[n_col]
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    if (n_col == 2L)
        gp$fill <- gp$fill[1L]
    else if (n_col == 3L)
        gp$fill <- gp$fill[2:1]

    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex12", density = 1.034, rot = 15,
                        grid = "hex_circle",
                        spacing = spacing, angle = angle, gp = gp,
                        name = "dodecagons")
    gList(bg, grob)
}

create_trunc_square_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_bg$fill <- gp$fill[n_col]
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    if (n_col == 2L)
        gp$fill <- gp$fill[1L]
    else if (n_col == 3L)
        gp$fill <- gp$fill[1:2]

    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex8", density = 1.082, rot = 22.5,
                        spacing = spacing, angle = angle, gp = gp,
                        name = "octagons")
    gList(bg, grob)
}

create_trunc_trihex_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_sq <- gp_bg
    gp_hx <- gp_bg
    gp_bg$fill <- gp$fill[n_col]
    if (n_col == 2L) {
        gp_bg$fill <- gp$fill[1L]
        gp_sq$fill <- gp$fill[2L]
        gp_hx$fill <- gp$fill[1L]
    }
    else if (n_col == 3L) {
        gp_sq$fill <- gp$fill[2L]
        gp_hx$fill <- gp$fill[1L]
    }
    # triangles
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    # squares
    stripe1 <- patternGrob("stripe", xyi$x, xyi$y, xyi$id,
                           angle = angle,
                           grid = "hex_circle", density = 0.42,
                           spacing = spacing,
                           gp = gp_sq, name = "square_stripes.1")
    stripe2 <- patternGrob("stripe", xyi$x, xyi$y, xyi$id,
                           angle = angle + 60,
                           grid = "hex_circle", density = 0.42,
                           spacing = spacing,
                           gp = gp_sq, name = "square_stripes.2")
    stripe3 <- patternGrob("stripe", xyi$x, xyi$y, xyi$id,
                           angle = angle - 60,
                           grid = "hex_circle", density = 0.42,
                           spacing = spacing,
                           gp = gp_sq, name = "square_stripes.3")
    # hexagons
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex6", density = 0.75,
                        grid = "hex_circle",
                        spacing = spacing, angle = angle, gp = gp_hx,
                        name = "hexagons")
    gList(bg, stripe1, stripe2, stripe3, grob)
}

create_trihexagonal_tiling <- function(xyi, gp, spacing, angle) {
    n_col <- length(gp$fill)
    gp_bg <- gp
    gp_bg$fill <- gp$fill[n_col]
    bg <- polygonGrob(xyi$x, xyi$y, xyi$id, default.units = "npc", gp = gp_bg,
                      name = "background_color")
    if (n_col == 2L)
        gp$fill <- gp$fill[1L]
    else if (n_col == 3L)
        gp$fill <- gp$fill[2:1]
    grob <- patternGrob("regular_polygon", xyi$x, xyi$y, xyi$id,
                        shape = "convex6", density = 1, rot = 30,
                        grid = "hex_circle",
                        spacing = spacing, angle = angle, gp = gp,
                        name = "hexagons")
    gList(bg, grob)
}
