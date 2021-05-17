#' Crosshatch patterned grobs
#'
#' `grid.pattern_crosshatch()` draws a crosshatch pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     grid.newpage()
#'     grid.pattern_crosshatch(colour = "green", fill = "blue", density = 0.5)
#'     grid.newpage()
#'     grid.pattern_crosshatch(density = 0.3, gp = gpar(col = "blue", fill = "yellow"))
#'   }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html>
#' @export
#' @seealso
grid.pattern_crosshatch <- function(x = c(0.5, 0.067, 0.067, 0.5, 0.933, 0.933),
                                    y = c(1.0, 0.75, 0.25, 0.0, 0.25, 0.75), id = 1L, ...,
                                    colour = gp$col %||% "grey20", fill = gp$fill %||% "grey80", angle = 30,
                                    density = 0.2, spacing = 0.05, xoffset = 0, yoffset = 0,
                                    alpha = gp$alpha %||% 1, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                    default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("crosshatch", x, y, id,
                 colour = colour, fill = fill, angle = angle,
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
  # Join all stripe polygons, and make sure the IDs don't overlap
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(stripe_polygons1) && is.null(stripe_polygons2)) {
    return(grid::nullGrob())
  } else if (is.null(stripe_polygons1)) {
    crosshatch_polygons  <- stripe_polygons2
  } else if (is.null(stripe_polygons2)) {
    crosshatch_polygons  <- stripe_polygons1
  } else {
    stripe_polygons2$id <- stripe_polygons2$id + max(stripe_polygons1$id)
    crosshatch_polygons  <- rbind(stripe_polygons1, stripe_polygons2)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the grob and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  crosshatch_grob <- grid::polygonGrob(
    x = unit(crosshatch_polygons$x, "npc"),
    y = unit(crosshatch_polygons$y, "npc"),
    id = crosshatch_polygons$id,
    gp = gpar(
      col     = alpha(params$pattern_colour, params$pattern_alpha),
      fill    = alpha(params$pattern_fill  , params$pattern_alpha),
      lwd     = params$pattern_size * .pt,
      lty     = params$pattern_linetype,
      lineend = 'square'
    )
  )

  crosshatch_grob
}
