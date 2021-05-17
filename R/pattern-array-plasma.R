#' Plasma patterned grobs
#'
#' `grid.pattern_plasma()` draws a plasma pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_gradient
#' @param scale Extra scaling
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'   y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'   grid.pattern_plasma(fill = "green")
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-plasma.html>
#' @export
grid.pattern_plasma <- function(x = c(0.5, 0.067, 0.067, 0.5, 0.933, 0.933),
                                y = c(1.0, 0.75, 0.25, 0.0, 0.25, 0.75), id = 1L, ...,
                                fill = gp$fill %||% "grey80", scale = 1, alpha = gp$alpha %||% 1,
                                aspect_ratio = 1, key_scale_factor = 1,
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("plasma", x, y, id,
                 fill = fill, scale = scale, alpha = alpha,
                 aspect_ratio = 1, key_scale_factor = 1,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read a user specified filename as an image
##
## @inheritParams create_gradient_as_array
##
## @return array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_plasma_as_array <- function(width, height, params, legend) {

  colour <- as.character(params$pattern_fill)

  img <- create_magick_plasma_img(
    width   = width,
    height  = height,
    colour  = colour
  )

  convert_img_to_array(img)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create plasma using imagemagick
##
## Ref: \url{https://www.imagemagick.org/Usage/canvas/}
##
## @param width,height image dimensions
## @param colour colour
##
## @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_plasma_img <- function(width=100, height=100, colour) {

  colour <- convert_r_colour_to_magick_colour(colour)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a pattern image of the required size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pseudo <- paste0("plasma:")
  img    <- magick::image_blank(width, height, pseudo_image = pseudo)
  img    <- magick::image_convert(img, colorspace = 'gray', depth = 8)
  img    <- magick::image_blur(img, radius = 2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make the white transparent
  # Colourie the black pixels into the desired colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- magick::image_transparent(img, 'white')
  img <- magick::image_colorize(img, opacity = 50, colour)

  img
}
