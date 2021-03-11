#' Circle patterned grobs
#'
#' \code{grid.pattern_circle} draws a circle pattern onto the graphic device.
#'
#' @inheritParams grid.pattern
#' @param ... Currently ignored
#' @param colour Stroke colour
#' @param fill Fill colour
#' @param angle Rotation angle in degrees
#' @param density Approx. fraction of area the pattern fills (between 0 and 1)
#' @param spacing Spacing between repetitions of pattern (between 0 and 1)
#' @param xoffset Shift pattern along x axis (between 0 and 1)
#' @param yoffset Shift pattern along y axis (between 0 and 1)
#' @param alpha Alpha (between 0 and 1)
#' @param linetype Stroke linetype
#' @param size Stroke linewidth
#' @examples
#'   if (require("grid")) {
#'     grid.newpage()
#'     grid.pattern_circle(colour = "green", fill = "blue", density = 0.5)
#'     grid.newpage()
#'     grid.pattern_circle(density = 0.3, gp = gpar(col = "blue", fill = "yellow"))
#'   }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-circle.html>
#' @export
grid.pattern_circle <- function(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = 1L, ...,
                                colour = gp$col %||% "grey20", fill = gp$fill %||% "grey80", angle = 30,
                                density = 0.2, spacing = 0.05, xoffset = 0, yoffset = 0,
                                alpha = gp$alpha %||% 1, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("circle", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 alpha = alpha, linetype = linetype, size = size,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create a cicleGrob object for a set of points
##
## Use 'sf' to help with the point in polygon intersections.
##
## \itemize{
##   \item{make grid to cover entire space}
##   \item{rotate points into position}
##   \item{create expanded boundary by r}
##   \item{create contracted boundary by r}
##   \item{remove all points outside the expanded boundary}
##   \item{remove all points within contracted boundary -> internal circles}
##   \item{any remaining points become part of the intersection grob}
##   \item{total circles = treeGrob( internal_circls, intersection_circles)}
## }
##
## @param boundary_df polygon_df data.frame
## @param angle angle of orientation (degrees)
## @param spacing spacing in grid 'npc' coordinates. Usually in range [0, 1]
## @param density fill fraction. Number in range [0, 1]
## @param xoffset,yoffset offset the pattern creation origin.
## @param aspect_ratio aspect_ratio
## @param params params from the geom
##
## @return A grid::circleGrob
##
## @import grid
## @import sf
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_circles_grob <- function(boundary_df, params, angle=0, spacing=0.1, density=0.3,
                                xoffset=0, yoffset=0,
                                aspect_ratio) {

  angle <- angle %% 90

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate radius
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r <- spacing * density / 2
  if (aspect_ratio > 1) {
    r <- r * aspect_ratio
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Things get dicey at the boundaries, especially when there is very large
  # or small aspect ratio.   Include this fudge factor in buffering the
  # boundary to ensure that all partially ntersecting circles are kept
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fudge_factor <- aspect_ratio
  if (fudge_factor < 1) {
    fudge_factor <- 1/fudge_factor
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Generate a square grid of points
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rff <- r * fudge_factor
  yff <- 1 / aspect_ratio * 1

  yff <- max(yff, 2)


  point_coords <- expand.grid(
    x = seq(-rff    , yff+rff, spacing),
    y = seq(-yff-rff, yff+rff, spacing)
  )

  if (nrow(point_coords) == 0) {
    return(grid::nullGrob())
  }

  point_coords   <- rotate_polygon_df(point_coords, angle, aspect_ratio)
  point_coords$y <- point_coords$y * aspect_ratio

  points_sf    <- sf::st_multipoint(as.matrix(point_coords))

  boundary_sf   <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist =  0)
  expanded_sf   <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist =  r * fudge_factor)
  contracted_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = -r * fudge_factor)

  all_points_sf      <- sf::st_intersection(expanded_sf  , points_sf)
  interior_points_sf <- sf::st_intersection(contracted_sf, all_points_sf)
  exterior_points_sf <- sf::st_difference(all_points_sf, contracted_sf)

  interior_points_mat <- as.matrix(interior_points_sf)
  exterior_points_mat <- as.matrix(exterior_points_sf)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a grob for the internal circles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(interior_points_mat) || nrow(interior_points_mat) == 0) {
    interior_circles_grob <- nullGrob()
  } else {
    interior_circles_grob <- grid::circleGrob(
      x = interior_points_mat[,1],
      y = interior_points_mat[,2],
      r = r,
      gp = gpar(
        fill = alpha(params$pattern_fill  , params$pattern_alpha),
        col  = alpha(params$pattern_colour, params$pattern_alpha),
        lwd  = params$pattern_size * .pt,
        lty  = params$pattern_linetype
      )
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a grob for the intersecting circles
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(exterior_points_mat) || nrow(exterior_points_mat) == 0) {
    exterior_circles_grob <- nullGrob()
  } else {
    exterior_circles_grob <- grid::circleGrob(
      x = exterior_points_mat[,1],
      y = exterior_points_mat[,2],
      r = r
    )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Find the intersection of the boundary and the minimal set of points I
    # have called the 'exterior points'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    boundary_grob <- convert_polygon_df_to_polygon_grob(boundary_df)
    exterior_circles_grob <- gridGeometry::polyclipGrob(
      boundary_grob, exterior_circles_grob,
      gp = gpar(
        fill = alpha(params$pattern_fill  , params$pattern_alpha),
        col  = alpha(params$pattern_colour, params$pattern_alpha),
        lwd  = params$pattern_size * .pt,
        lty  = params$pattern_linetype
      )
    )
  }


  grid::grobTree(
    interior_circles_grob,
    exterior_circles_grob
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## @rdname create_pattern_none
## @importFrom gridGeometry polyclipGrob
## @import scales
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_circles <- function(params, boundary_df, aspect_ratio, legend = FALSE) {

  stopifnot(is_polygon_df(boundary_df))

  boundary_grob <- convert_polygon_df_to_polygon_grob(boundary_df)
  bbox          <- calculate_bbox_polygon_df(boundary_df)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an SF object with points covering the entire viewpoint
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  circle_grob <- create_circles_grob(
    boundary_df  = boundary_df,
    params       = params,
    angle        = params$pattern_angle,
    spacing      = params$pattern_spacing,
    density      = params$pattern_density,
    xoffset      = params$pattern_xoffset,
    yoffset      = params$pattern_yoffset,
    aspect_ratio = aspect_ratio
  )

  circle_grob
}
