#' Gradient patterned grobs
#'
#' `grid.pattern_gradient()` draws a gradient pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param fill2 Second colour
#' @param orientation vertical, horizontal, or radial
#' @param aspect_ratio Override aspect ratio
#' @param key_scale_factor Additional scale factor for legend
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   \dontrun{
#'   if (require("grid")) {
#'     grid.newpage()
#'     grid.pattern_gradient(fill = "green")
#'     grid.newpage()
#'     grid.pattern_gradient(fill = "green", orientation = "radial")
#'   }
#'   }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-gradient.html>
#' @export
grid.pattern_gradient <- function(x = c(0.5, 0.067, 0.067, 0.5, 0.933, 0.933),
                                  y = c(1.0, 0.75, 0.25, 0.0, 0.25, 0.75), id = 1L, ...,
                                  fill = gp$fill %||% "grey80", fill2 = "#4169E1",
                                  orientation = "vertical", alpha = gp$alpha %||% 1,
                                  aspect_ratio = 1, key_scale_factor = 1,
                                  default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("gradient", x, y, id,
                 fill = fill, fill2 = fill2,
                 orientation = orientation, alpha = alpha,
                 aspect_ratio = 1, key_scale_factor = 1,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create a gradient image as an array
##
## @param width,height image dimensions
## @param colour1,colour2 gradient colours
## @param orientation vertical, horizontal or radial
##
## @return magick image
##
## @import magick
## @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    abort("create_gradient_img() - Orientation not supported: ", orientation)
  }

  img
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## A shim to go between the main pattern function for an image, and the
## specific pattern functon for an image.
##
## This function really just unwraps the 'params' into better arguments and
## passes them to the actual img/array creation function
##
## @param width,height image dimensions
## @param params geom parameters from ggplot
## @param legend logical. TRUE if call comes during legend creation, otherwise FALSE.
##
## @return RGBA array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_gradient_as_array <- function(width, height, params, legend) {

  orientation <- check_default(params$pattern_orientation, options = c('vertical', 'horizontal', 'radial'))
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
