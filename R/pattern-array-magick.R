#' Magick patterned grobs
#'
#' `grid.pattern_image()` draws a `imagemagick` pattern onto the graphic device.
#' `magick_pattern_names`, `magick_pattern_intensity_names`, and
#' `magick_pattern_stripe_names` are character vectors of supported `type` values
#' plus subsets for shaded intensity and stripes.
#'
#' @inheritParams grid.pattern_image
#' @param type Magick pattern types.  `magick_pattern_names`, `magick_pattern_intensity_names`, and
#'             `magick_pattern_stripe_names` are character vectors of supported `type` values
#'             plus subsets for shaded intensity and stripes.
#' @param fill Fill colour
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   grid.pattern_magick(type="octagons", fill="blue", scale=2)
#'   print(magick_pattern_names)
#'   print(magick_pattern_intensity_names)
#'   print(magick_pattern_stripe_names)
#' @seealso The `ggpattern` documentation <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-magick.html>
#'          and `imagemagick` documentation <http://www.imagemagick.org/script/formats.php> for more information.
#' @export
grid.pattern_magick <- function(x = c(0.5, 0.067, 0.067, 0.5, 0.933, 0.933),
                                y = c(1.0, 0.75, 0.25, 0.0, 0.25, 0.75), id = 1L, ...,
                                type = "hexagons", fill = "grey20", scale = 1, filter = "box",
                                alpha = gp$alpha %||% 1, aspect_ratio = 1, key_scale_factor = 1, res = 72,
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("image", x, y, id,
                 type = type, fill = fill, scale = scale, scale = scale, filter = filter,
                 alpha = alpha, aspect_ratio = aspect_ratio,
                 key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Names of patterns available in image magick, plus subsets for shaded intensity and stripes
##
## See \url{http://www.imagemagick.org/script/formats.php} for more information.
##
#' @rdname grid.pattern_magick
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
magick_pattern_names <- c(
  "bricks", "checkerboard", "circles", "crosshatch", "crosshatch30",
  "crosshatch45", "fishscales", "gray0", "gray5", "gray10", "gray15",
  "gray20", "gray25", "gray30", "gray35", "gray40", "gray45", "gray50",
  "gray55", "gray60", "gray65", "gray70", "gray75", "gray80", "gray85",
  "gray90", "gray95", "gray100", "hexagons", "horizontal", "horizontal2",
  "horizontal3", "horizontalsaw", "hs_bdiagonal", "hs_cross", "hs_diagcross",
  "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "left45",
  "leftshingle", "octagons", "right30", "right45", "rightshingle",
  "smallfishscales", "vertical", "vertical2", "vertical3", "verticalbricks",
  "verticalleftshingle", "verticalrightshingle", "verticalsaw"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname grid.pattern_magick
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
magick_pattern_intensity_names <- c(
  "gray0", "gray5", "gray10", "gray15",
  "gray20", "gray25", "gray30", "gray35", "gray40", "gray45", "gray50",
  "gray55", "gray60", "gray65", "gray70", "gray75", "gray80", "gray85",
  "gray90", "gray95", "gray100"
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname grid.pattern_magick
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
magick_pattern_stripe_names <- c(
  "crosshatch", "crosshatch30", "crosshatch45",
  "horizontal", "horizontal2", "horizontal3",
  "hs_bdiagonal", "hs_cross", "hs_diagcross",
  "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "left45",
  "right30", "right45",
  "vertical", "vertical2", "vertical3"
)

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
