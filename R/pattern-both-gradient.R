#' Gradient patterned grobs
#'
#' `grid.pattern_gradient()` draws a gradient pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param fill2 Second colour
#' @param orientation vertical, horizontal, or radial
#' @param aspect_ratio Override aspect ratio
#' @param use_R4.1_gradients Whether to use the gradient feature introduced in R v4.1
#'                           or use a `rasterGrob` approximation.
#'                           Note not all graphic devices support the grid gradient feature.
#' @param key_scale_factor Additional scale factor for legend
#' @param res Assumed resolution (in pixels per graphic device inch) to use when creating array pattern.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'  if (require("grid") && require("magick") && capabilities("png")) {
#'    x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    grid.pattern_gradient(x_hex, y_hex, fill = "green")
#'    grid.newpage()
#'    grid.pattern_gradient(x_hex, y_hex, fill = "green", orientation = "radial")
#'  }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-gradient.html>
#' @export
grid.pattern_gradient <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                  fill = gp$fill %||% "grey80", fill2 = "#4169E1",
                                  orientation = "vertical", alpha = gp$alpha %||% NA_real_,
                                  use_R4.1_gradients = getOption("ggpattern_use_R4.1_gradients",
                                                                 getOption("ggpattern_use_R4.1_features")),
                                  aspect_ratio = 1, key_scale_factor = 1, res = 72,
                                  default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("gradient", x, y, id,
                 fill = fill, fill2 = fill2,
                 orientation = orientation, alpha = alpha,
                 use_R4.1_gradients = use_R4.1_gradients,
                 aspect_ratio = aspect_ratio, key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#' create a gradient image as an array
#'
#' @param width,height image dimensions
#' @param colour1,colour2 gradient colours
#' @param orientation vertical, horizontal or radial
#'
#' @return magick image
#'
#' @noRd
create_gradient_img <- function(width       = 100,
                                height      = 100,
                                colour1     = 'red',
                                colour2     = 'blue',
                                orientation = 'vertical') {

  colour1     <- convert_r_colour_to_magick_colour(colour1)
  colour2     <- convert_r_colour_to_magick_colour(colour2)
  colour_spec <- paste0(colour2, "-", colour1)

  if (orientation == 'radial') {
    colour_spec <- paste0(colour1, "-", colour2)
    pseudo <- paste0('radial-gradient:', colour_spec)
    img <- magick::image_blank(width, height, pseudo_image = pseudo)
  } else if (orientation == 'vertical') {
    pseudo <- paste0('gradient:', colour_spec)
    img <- magick::image_blank(width, height, pseudo_image = pseudo)
  } else if (orientation == 'horizontal') {
    pseudo <- paste0('gradient:', colour_spec)
    img <- magick::image_blank(height, width, pseudo_image = pseudo)
    img <- magick::image_rotate(img, 90)
  } else {
    abort(paste0("create_gradient_img() - Orientation not supported: ", orientation))
  }

  img
}

create_pattern_gradient <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    if (params$pattern_use_R4.1_gradients) {
        create_gradient_as_geometry(params, boundary_df, aspect_ratio, legend)
    } else {
        create_pattern_array(params, boundary_df, aspect_ratio, legend, create_gradient_as_array)
    }
}

create_gradient_as_geometry <- function(params, boundary_df, aspect_ratio, legend) {
  orientation <- check_default(params$pattern_orientation,
                               options = c('vertical', 'horizontal', 'radial'))
  colour1     <- params$pattern_fill
  colour2     <- params$pattern_fill2

  x_min <- min(boundary_df$x)
  x_max <- max(boundary_df$x)
  x_med <- 0.5 * (x_min + x_max)
  y_min <- min(boundary_df$y)
  y_max <- max(boundary_df$y)
  y_med <- 0.5 * (y_min + y_max)
  x_range <- convertX(unit(x_max - x_min, "npc"), "in")
  y_range <- convertY(unit(y_max - y_min, "npc"), "in")
  gradient <- switch(orientation,
     horizontal = linearGradient(c(colour1, colour2),
                                 x1 = x_min, y1 = y_med,
                                 x2 = x_max, y2 = y_med),
     radial = radialGradient(c(colour1, colour2),
                             cx1 = x_med, cy1 = y_med,
                             cx2 = x_med, cy2 = y_med,
                             r2 = 0.5 * max(x_range, y_range),
                             extend = "none"
                             ),
     vertical = linearGradient(c(colour1, colour2),
                               x1 = x_med, y1 = y_min,
                               x2 = x_med, y2 = y_max)
  )

  gp <- gpar(col = NA, fill = gradient)

  convert_polygon_df_to_polygon_grob(boundary_df, gp = gp)
}

#' A shim to go between the main pattern function for an image, and the
#' specific pattern functon for an image.
#'
#' This function really just unwraps the 'params' into better arguments and
#' passes them to the actual img/array creation function
#'
#' @param width,height image dimensions
#' @param params geom parameters from ggplot
#' @param legend logical. TRUE if call comes during legend creation, otherwise FALSE.
#'
#' @return RGBA array
#'
#' @noRd
create_gradient_as_array <- function(width, height, params, legend) {

  assert_suggested("magick", "gradient")

  orientation <- check_default(params$pattern_orientation,
                               options = c('vertical', 'horizontal', 'radial'))
  colour1     <- params$pattern_fill
  colour2     <- params$pattern_fill2

  img <- create_gradient_img(
    width       = width,
    height      = height,
    colour1     = colour1,
    colour2     = colour2,
    orientation = orientation
  )

  convert_img_to_array(img)
}
