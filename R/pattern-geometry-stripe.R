#' Stripe patterned grobs
#'
#' `grid.pattern_stripe()` draws a strip pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     grid.newpage()
#'     grid.pattern_stripe(colour = "green", fill = "blue", density = 0.5)
#'     grid.newpage()
#'     grid.pattern_stripe(density = 0.3, gp = gpar(col = "blue", fill = "yellow"))
#'   }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html>
#' @export
grid.pattern_stripe <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = 1L, ...,
                                colour = gp$col %||% "grey20", fill = gp$fill %||% "grey80", angle = 30,
                                density = 0.2, spacing = 0.05, xoffset = 0, yoffset = 0,
                                alpha = gp$alpha %||% 1, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("stripe", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 alpha = alpha, linetype = linetype, size = size,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create a sf MULTIPOLYGON object where each polygon is an individual stripe.
##
## The stripes are created as polygons so that when clipped to rects/polygons,
## the ends of the stripe are clipped correctly to the boundary.
##
## @inheritParams create_circles_grob
##
## @return `sf` multipolygon object
##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_stripes_sf <- function(angle, spacing, density, xoffset=0, yoffset=0,
                                               aspect_ratio) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clearly distinguise:
  #    - The user supplies the stripe angle.
  #    - We determine the spine angle
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripe_angle <- ( angle       %% 180)
  spine_angle  <- ((angle + 90) %% 180)
  angle        <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Should the spine start at the bottom left or the bottom right?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (spine_angle < 90) {
    spine_origin <- c(xoffset, yoffset)
  } else {
    spine_origin <- c(1 + xoffset, yoffset)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert angles to radians
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripe_angle <- stripe_angle * pi/180
  spine_angle  <- spine_angle  * pi/180

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # aspect ratio will determine maximum line length
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (aspect_ratio < 1) {
    ll <- 1/aspect_ratio
  } else {
    ll <- aspect_ratio
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Hypotenuse
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ll <- sqrt(ll*ll + 1)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We are going to create stripes that begin and end outside the viewport
  # no matter what the angle.  Make them long, and then we'll truncate them
  # later when we intersect with the area to be shaded
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rib_offset <- c(ll * cos(stripe_angle), ll * aspect_ratio * sin(stripe_angle))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a spine. As we walk along the spine we'll extend stripes out
  # at right angles.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spine_end   <- c(spine_origin[1] + ll * cos(spine_angle),
                   spine_origin[2] + ll * aspect_ratio * sin(spine_angle))

  Nribs <- ll/spacing

  spine_mat <- cbind(
    seq(spine_origin[1], spine_end[1], length.out = Nribs),
    seq(spine_origin[2], spine_end[2], length.out = Nribs)
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # One edge of the stripe is positioned x% of the way along, where this
  # percent is controlled by the pattern_density
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  spine_offset  <- c(density * spacing * cos(spine_angle),
                     density * spacing * sin(spine_angle) * aspect_ratio)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # We have enough now to construct a polygon for each stripe element
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stripe_corner_a <- t(t(spine_mat) + rib_offset)
  stripe_corner_b <- t(t(spine_mat) - rib_offset)
  stripe_corner_c <- t(t(spine_mat) - rib_offset + spine_offset)
  stripe_corner_d <- t(t(spine_mat) + rib_offset + spine_offset)

  stripe_coords_full <- lapply(seq(nrow(stripe_corner_a)), function(i) {
    rbind(
      stripe_corner_a[i,],
      stripe_corner_b[i,],
      stripe_corner_c[i,],
      stripe_corner_d[i,],
      stripe_corner_a[i,]
    )
  })

  stripe_coords <- lapply(stripe_coords_full, function(x) {list(x)})
  stripes_as_sfg_multipolygon <- sf::st_multipolygon(stripe_coords)

  stripes_as_sfg_multipolygon
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @rdname create_pattern_none
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_stripes_via_sf <- function(params, boundary_df, aspect_ratio,
                                          legend = FALSE) {

  stopifnot(is_polygon_df(boundary_df))

  stripes_sf <- create_stripes_sf(
    angle   = params$pattern_angle,
    spacing = params$pattern_spacing,
    density = params$pattern_density,
    xoffset = params$pattern_xoffset,
    yoffset = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )

  boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)

  striped_area    <- st_intersection(stripes_sf, boundary_sf)
  stripe_polygons <- convert_polygon_sf_to_polygon_df(striped_area)

  if (is.null(stripe_polygons)) {
    return(grid::nullGrob())
  }

  stripes_grob <- grid::polygonGrob(
    x = unit(stripe_polygons$x, "npc"),
    y = unit(stripe_polygons$y, "npc"),
    id = stripe_polygons$id,
    gp = gpar(
      col     = alpha(params$pattern_colour, params$pattern_alpha),
      fill    = alpha(params$pattern_fill  , params$pattern_alpha),
      lwd     = params$pattern_size * .pt,
      lty     = params$pattern_linetype,
      lineend = 'square'
    )
  )

  stripes_grob
}
