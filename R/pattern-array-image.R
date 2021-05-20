#' Image patterned grobs
#'
#' `grid.pattern_image()` draws an image pattern onto the graphic device.
#'
#' Here is a description of the `type` arguments:
#' \describe{
#'   \item{expand}{Scale the image beyond the bounding box and crop it such that
#'                 the image fully covers the width and the height of the region.}
#'   \item{fit}{Scale the image such that either the width or the height of the image fits in the bounding box.
#'              Affected by `gravity`}
#'   \item{none}{Position a single image in the region without attempting to scale to the bounding box size.
#'               Affected by `scale` and `gravity`.}
#'   \item{squish}{Distort the image to cover the bounding box of the region.}
#'   \item{tile}{Repeat the image to cover the bounding box.  Affected by `tile`.}
#'  }
#'
#' @inheritParams grid.pattern_plasma
#' @param filename Image of filename or URL
#' @param type Image scaling type
#' @param gravity Position of image within area.  `magick::gravity_types()` returns a vector of supported values.
#' @param filter Filter to use when scaling. `magick::filter_types()` returns a vector of supported values.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'  x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'  y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'  logo_filename <- system.file("img", "Rlogo.png" , package = "png")
#'  grid.pattern_image(x_hex, y_hex, filename = logo_filename, type = "fit")
#'  \dontrun{
#'    # "tile" type image pattern depends on magick::magick_image_readpath()
#'    # which is not reliable across platforms
#'    grid::grid.newpage()
#'    grid.pattern_image(x_hex, y_hex, filename = logo_filename, type = "tile")
#'  }
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-image.html>
#' @export
grid.pattern_image <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                               filename = "", type = "fit", scale = 1,
                               gravity = "center", filter = "lanczos",
                               alpha = gp$alpha %||% 1, aspect_ratio = 1, key_scale_factor = 1, res = 72,
                               default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("image", x, y, id,
                 filename = filename, type = type, scale = scale,
                 gravity = gravity, filter = filter,
                 alpha = alpha, aspect_ratio = aspect_ratio, key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

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
