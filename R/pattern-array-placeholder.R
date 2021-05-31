#' Placeholder image patterned grobs
#'
#' `grid.pattern_placeholder()` draws a placeholder image pattern onto the graphic device.
#' `placeholder_names` are character vectors of supported placeholder types.
#'
#' @inheritParams grid.pattern_plasma
#' @param type Image source.  `placeholder_names` is a vector of supported values.
#'             If you would like only greyscale images append `bw` to the name.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   \dontrun{
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     # requires internet connection to download from placeholder image websites
#'     grid.pattern_placeholder(x_hex, y_hex, type="bear")
#'   }
#'   print(placeholder_names)
#' @seealso The `ggpattern` documentation: <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-placeholder.html>
#' @export
grid.pattern_placeholder <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                     type = "kitten", alpha = gp$alpha %||% NA_real_,
                                     aspect_ratio = 1, key_scale_factor = 1, res = 72,
                                     default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("placeholder", x, y, id,
                 type = type, alpha = alpha,
                 aspect_ratio = aspect_ratio, key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

## All placeholder names

#' @rdname grid.pattern_placeholder
#' @export
placeholder_names <- c(
  "kitten", "kittenbw", "picsum", "picsumbw", "murray", "murraybw",
  "cage", "cagebw", "bear", "bearbw", "seagal", "seagalbw", "placeholderbw",
  "placeholder", "dummybw", "dummy", "flickr", "flickrbw", "beard",
  "beardbw", "lorempixel", "lorempixelbw", "placeimg", "placeimgbw",
  'keanu', 'keanubw'
)

#' Fetch a placeholder image of the correct dimensions
#'
#' Fetch a placeholder image from one of the sites used by web designers.  Append
#' 'bw' to the name to fetch greyscale images instead of colour.
#'
#' \describe{
#'   \item{\code{kitten}}{ - Kittens from \url{https://placekitten.com/}}
#'   \item{\code{picsum}}{ - Random images from \url{https://picsum.photos/}}
#'   \item{\code{murray}}{ - Bill Murrary images from \url{http://www.fillmurray.com}}
#'   \item{\code{cage}}{ - Nicholas Cage images from \url{https://www.placecage.com/}}
#'   \item{\code{bear}}{ - Bears from \url{https://placebear.com/}}
#'   \item{\code{seagal}}{ - Steven Seagal images from \url{https://www.stevensegallery.com/}}
#'   \item{\code{flickr}}{ - Images from Flickr \url{https://loremflickr.com/}}
#'   \item{\code{beard}}{ - Beards! \url{http://placebeard.it/}}
#'   \item{\code{lorempixel}}{ - Random images from \url{http://lorempixel.com/}}
#'   \item{\code{placeimg}}{ - Random images from \url{http://placeimg.com/}}
#'   \item{\code{dummy}}{ - Numeric placeholder images from \url{https://dummyimage.com}}
#' }
#'
#' @param width,height image dimensions
#' @param type specify the server from which to fetch images. default: 'kitten'
#'
#' @noRd
fetch_placeholder_img <- function(width = 100, height = 100, type = 'kitten') {

  width  <- as.integer(width)
  height <- as.integer(height)

  img_url <- switch(
    type,
    kitten         = glue("https://placekitten.com/{width}/{height}"),
    kittenbw       = glue("https://placekitten.com/g/{width}/{height}"),
    picsum         = glue("https://picsum.photos/{width}/{height}"),
    picsumbw       = glue("https://picsum.photos/{width}/{height}?grayscale"),
    murray         = glue("http://www.fillmurray.com/{width}/{height}"),
    murraybw       = glue("http://www.fillmurray.com/g/{width}/{height}"),
    cage           = glue("https://www.placecage.com/{width}/{height}"),
    cagebw         = glue("https://www.placecage.com/g/{width}/{height}"),
    bear           = glue("https://placebear.com/{width}/{height}"),
    bearbw         = glue("https://placebear.com/g/{width}/{height}"),
    seagal         = glue("https://www.stevensegallery.com/{width}/{height}"),
    seagalbw       = glue("https://www.stevensegallery.com/g/{width}/{height}"),
    placeholderbw  = ,
    placeholder    = glue("https://via.placeholder.com/{width}x{height}.png"),
    dummybw        = ,
    dummy          = glue("https://dummyimage.com/{width}x{height}"),
    flickr         = glue("https://loremflickr.com/{width}/{height}"),
    flickrbw       = glue("https://loremflickr.com/g/{width}/{height}/all"),
    beard          = glue("http://placebeard.it/{width}/{height}"),
    beardbw        = glue("http://placebeard.it/g/{width}/{height}"),
    lorempixel     = glue("http://lorempixel.com/{width}/{height}"),
    lorempixelbw   = glue("http://lorempixel.com/g/{width}/{height}"),
    placeimg       = glue("http://placeimg.com/{width}/{height}/any"),
    placeimgbw     = glue("http://placeimg.com/{width}/{height}/any/grayscale"),
    keanu          = glue("https://placekeanu.com/{width}/{height}"),
    keanubw        = glue("https://placekeanu.com/{width}/{height}/g"),
    {
      # warn("fetch_placeholder_image_as_array(): Unknown pattern_type (", type, ") ",
      #         "using 'kitten'")
      glue("https://placekitten.com/{width}/{height}")
    }
  )

  img_read_memoised(filename = img_url)
}

#' Fetch a placeholder image of the correct size and return as an array
#'
#' @inheritParams create_gradient_as_array
#'
#' @return RGBA array
#'
#' @noRd
fetch_placeholder_array <- function(width, height, params, legend) {
  if (legend) {
    img <- magick::image_blank(width, height)
    return(convert_img_to_array(img))
  }

  placeholder_type <- check_default(as.character(params$pattern_type), default = 'kitten', type = 'char')
  img <- fetch_placeholder_img(width = width, height = height, type = placeholder_type)

  convert_img_to_array(img)
}
