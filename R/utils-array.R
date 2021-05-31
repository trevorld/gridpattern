#' Generate an RGBA array and create a raster grob to fill the region
#'
#' the boundary of the shaded region will be used to generate a alpha mask
#' so that the array conforms to the shape.
#'
#' @inheritParams create_pattern_none
#' @param array_fn Array function to use
#'
#' @return rasterGrob
#'
#' @noRd
create_pattern_array <- function(params, boundary_df, aspect_ratio, legend,
                                 array_fn = create_magick_pattern_as_array) {

  if (anyNA(boundary_df$x) || anyNA(boundary_df$y)) {
    return(grid::nullGrob())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What is the size of the bounding box of the boundary for this pattern?
  # Calculate the centre (x,y) and (width,height)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  npc_xrange  <- range(boundary_df$x, na.rm = TRUE)
  npc_yrange  <- range(boundary_df$y, na.rm = TRUE)
  npc_width   <- abs(diff(npc_xrange))
  npc_height  <- abs(diff(npc_yrange))
  npc_x       <- mean(npc_xrange)
  npc_y       <- mean(npc_yrange)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the dimensions of the bounding box and use the integer
  # values of these to define the image dimensions
  # res of 72 DPI is the imagemagick default
  # Ideally could detect resolution of graphics device and use that as default...
  # NB. large pixel sizes can cause errors with getting 'placeholder' images
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- params$pattern_res # defaults to 72 DPI
  in_vp_width <- as.numeric(grid::convertWidth(unit(1, 'npc'), 'inches'))
  in_width <- npc_width * in_vp_width
  in_height <- npc_height * in_vp_width / aspect_ratio
  arr_width <- res * in_width
  arr_height <- res * in_height
  # enforce minimum height/width of 12 pixels
  if (arr_width < 12 || arr_height < 12) {
    mult <- 12 / min(in_width, in_height)
    arr_width  <- mult * arr_width
    arr_height <- mult * arr_height
  }
  arr_width  <- as.integer(arr_width)
  arr_height <- as.integer(arr_height)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is a legend, then draw a much smaller image.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (legend) {
    arr_width  <- as.integer( arr_width / 12)
    arr_height <- as.integer(arr_height / 12)

    # Override type for better looking legend when tiling
    if (params$pattern_type %in% c('tile', 'none')) {
      params$pattern_type <- 'fit'
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The image and the boundary_df bbox are coincident.  To mask the image,
  # scale the boundary_df to encompass the full 'npc' range from 0 to 1.
  # An alpha mask will be created to encompass the whole of the image, and
  # the resulting image will be placed at the bbox location.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scaled_boundary_df   <- boundary_df
  scaled_boundary_df$x <- (scaled_boundary_df$x - npc_xrange[1]) / npc_width
  scaled_boundary_df$y <- (scaled_boundary_df$y - npc_yrange[1]) / npc_height

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # check for issues e.g. Zero area regions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (any(is.nan(scaled_boundary_df$x)) || any(is.nan(scaled_boundary_df$y))) {
    return(grid::nullGrob())
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch an image of the required dimensions.
  # Create a mask of the required dimensions from the scaled boundary_df
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgba_arr <- array_fn(arr_width, arr_height, params, legend)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check the array conforms to what we want.  This is especially
  # important as we're allowing users to generate arrays for patterns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!(is.array(rgba_arr)             &&
        is.numeric(rgba_arr)           &&
        length(dim(rgba_arr) == 3)     &&
        dim(rgba_arr)[3] == 4          &&
        dim(rgba_arr)[1] == arr_height &&
        dim(rgba_arr)[2] == arr_width  &&
        !anyNA(rgba_arr)               &&
        min(rgba_arr) >= 0             &&
        max(rgba_arr) <= 1)) {

    warn(glue::glue("create_pattern_array(): Expecting a numeric RGBA array with dim = c({arr_height}, {arr_width}, 4) ",
                    "but instead got a {deparse(class(rgba_arr))} ",
                    "of type {typeof(rgba_arr)} with dimensions {deparse(dim(rgba_arr))}"), call. = FALSE)

    rgba_arr <- array(c(0, 1), dim = c(arr_height, arr_width, 4))
  }

  boundary_mask <- convert_polygon_df_to_alpha_channel(scaled_boundary_df, width = arr_width, height = arr_height)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Stack the current image array with an alpha channel.
  # Using a custom version of `abind::abind()` so I could avoid having it
  # as another package dependency
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.na(params$pattern_alpha))
      alpha <- 1
  else
      alpha <- params$pattern_alpha
  rgba_arr[,,4] <- rgba_arr[,,4] * boundary_mask * alpha

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a rasterGrob image at the location of the 'boundary_df' bounding box.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  raster_grob   <- rasterGrob(
    rgba_arr,
    x      = npc_x,
    y      = npc_y,
    width  = npc_width,
    height = npc_height
  )
  raster_grob
}
