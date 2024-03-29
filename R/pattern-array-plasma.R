#' Plasma patterned grobs
#'
#' `grid.pattern_plasma()` draws a plasma pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_gradient
#' @param scale Extra scaling
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (requireNamespace("magick")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     grid.pattern_plasma(x_hex, y_hex, fill = "green")
#'   }
#' @seealso [grid.pattern_ambient()] provides a noise pattern using the `ambient` package.
#'          Pseudorandom seeds for the plasma pattern may be set via [magick::magick_set_seed()].
#' @export
grid.pattern_plasma <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                fill = gp$fill %||% "grey80", scale = 1, alpha = gp$alpha %||% NA_real_,
                                aspect_ratio = 1, key_scale_factor = 1,
                                res = getOption("ggpattern_res", 72),
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("plasma", x, y, id,
                 fill = fill, scale = scale, alpha = alpha,
                 aspect_ratio = aspect_ratio, key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#' Read a user specified filename as an image
#'
#' @inheritParams create_gradient_as_array
#'
#' @return array
#' @noRd
create_magick_plasma_as_array <- function(width, height, params, legend) {

  assert_suggested("magick", "plasma")

  colour <- as.character(params$pattern_fill)

  img <- create_magick_plasma_img(
    width   = width,
    height  = height,
    colour  = colour
  )

  convert_img_to_array(img)
}

#' Create plasma using imagemagick
#'
#' Ref: \url{https://www.imagemagick.org/Usage/canvas/}
#'
#' @param width,height image dimensions
#' @param colour colour
#'
#' @noRd
create_magick_plasma_img <- function(width=100, height=100, colour) {

  colour <- convert_r_colour_to_magick_colour(colour)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a pattern image of the required size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pseudo <- "plasma:"
  img    <- magick::image_blank(width, height, pseudo_image = pseudo)
  img    <- magick::image_convert(img, colorspace = 'gray', depth = 8)
  img    <- magick::image_blur(img, radius = 2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make the white transparent
  # Colorize the black pixels into the desired colour
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- magick::image_transparent(img, 'white')
  img <- magick::image_colorize(img, opacity = 50, colour)

  img
}
