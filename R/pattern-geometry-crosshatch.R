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
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html>
#' @export
#' @seealso
grid.pattern_crosshatch <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                    colour = gp$col %||% "grey20",
                                    fill = gp$fill %||% "grey80", fill2 = fill,
                                    angle = 30, density = 0.2,
                                    spacing = 0.05, xoffset = 0, yoffset = 0,
                                    alpha = gp$alpha %||% 1, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                    default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("crosshatch", x, y, id,
                 colour = colour, fill = fill, fill2 = fill2, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 alpha = alpha, linetype = linetype, size = size,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @rdname create_pattern_none
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_crosshatch_via_sf <- function(params, boundary_df, aspect_ratio,
                                             legend = FALSE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create stripes in 1 direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripes_sf <- create_stripes_sf(
    angle     = params$pattern_angle,
    spacing   = params$pattern_spacing,
    density   = params$pattern_density,
    xoffset   = params$pattern_xoffset,
    yoffset   = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )

  boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)

  striped_area     <- st_intersection(stripes_sf, boundary_sf)
  stripe_polygons1 <- convert_polygon_sf_to_polygon_df(striped_area)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create stripes in other direction
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripes_sf <- create_stripes_sf(
    angle    = params$pattern_angle + 90,
    spacing  = params$pattern_spacing,
    density  = params$pattern_density,
    xoffset  = params$pattern_xoffset,
    yoffset  = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )
  striped_area     <- st_intersection(stripes_sf, boundary_sf)
  stripe_polygons2 <- convert_polygon_sf_to_polygon_df(striped_area)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  under_hatch <- as_crosshatch_grob(stripe_polygons1, params, fill = params$pattern_fill)
  over_hatch  <- as_crosshatch_grob(stripe_polygons2, params, fill = params$pattern_fill2)
  gList(under_hatch, over_hatch)
}

as_crosshatch_grob <- function(stripe_polygons, params, fill = params$pattern_fill) {
    if (is.null(stripe_polygons)) return(grid::nullGrob())
    grid::polygonGrob(x = unit(stripe_polygons$x, "npc"),
      y = unit(stripe_polygons$y, "npc"),
      id = stripe_polygons$id,
      gp = gpar(
        col     = alpha(params$pattern_colour, params$pattern_alpha),
        fill    = alpha(fill, params$pattern_alpha),
        lwd     = params$pattern_size * .pt,
        lty     = params$pattern_linetype,
        lineend = 'square'
      )
    )
}
