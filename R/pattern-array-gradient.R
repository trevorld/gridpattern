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
