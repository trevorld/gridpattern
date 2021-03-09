#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read a user specified filename as an image
##
## @inheritParams create_gradient_as_array
##
## @return array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_magick_pattern_as_array <- function(width, height, params, legend) {

  if (legend) {
      params$pattern_scale <- params$pattern_scale * params$pattern_key_scale_factor
  }
  type   <- check_default(as.character(params$pattern_type),
                          options = magick_pattern_names,
                          default = 'hexagons')

  scale  <- check_default(params$pattern_scale, default = 1, type = 'numeric')

  filter <- tolower(as.character(params$pattern_filter))
  filter <- check_default(filter, options = tolower(magick::filter_types()), default = 'box')

  colour <- as.character(params$pattern_fill)

  img <- create_magick_pattern_img_scaled(
    width  = width,
    height = height,
    type   = type,
    colour = colour,
    scale  = scale,
    filter = filter
  )

  convert_img_to_array(img)
}
