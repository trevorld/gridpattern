#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read a user specified filename/URL as an image
##
## @inheritParams create_gradient_as_array
##
## @return array
##
## @import magick
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
img_read_as_array_wrapper <- function(width, height, params, legend) {

  filename <- as.character(params$pattern_filename)

  fill_type <- tolower(as.character(params$pattern_type))
  fill_type <- check_default(fill_type, options = fill_types)

  gravity <- tolower(as.character(params$pattern_gravity))
  gravity <- check_default(gravity, tolower(magick::gravity_types()), 'center')

  filter <- tolower(as.character(params$pattern_filter))
  filter <- check_default(filter, tolower(magick::filter_types()), 'lanczos')

  scale  <- params$pattern_scale
  scale  <- check_default(scale, default = 1, type = 'numeric')

  img_read_as_array(
    filename    = filename,
    width       = width,
    height      = height,
    fill_type   = fill_type,
    gravity     = gravity,
    filter      = filter,
    scale       = scale
  )
}
