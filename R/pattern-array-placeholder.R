#' Placeholder image patterned grobs
#'
#' `grid.pattern_placeholder()` draws a placeholder image pattern onto the graphic device.
#' `names_placeholder` are character vectors of supported placeholder types.
#'
#' @inheritParams grid.pattern_plasma
#' @param type Image source.  `names_placeholder` is a vector of supported values.
#'             If you would like only greyscale images append `bw` to the name.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (requireNamespace("magick")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     # requires internet connection to download from placeholder image websites
#'     try(grid.pattern_placeholder(x_hex, y_hex, type="bear"))
#'   }
#'
#'   print(names_placeholder)
#' @seealso [reset_image_cache()] resets the image cache used by [grid.pattern_image()] and `grid.pattern_placeholder()`.
#' @export
grid.pattern_placeholder <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                     type = "bear", alpha = gp$alpha %||% NA_real_,
                                     aspect_ratio = 1, key_scale_factor = 1,
                                     res = getOption("ggpattern_res", 72),
                                     default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("placeholder", x, y, id,
                 type = type, alpha = alpha,
                 aspect_ratio = aspect_ratio, key_scale_factor = key_scale_factor, res = res,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

## All placeholder names

#' @rdname grid.pattern_placeholder
#' @export
names_placeholder <- c(
  "bear", "bearbw",
  "beard", "beardbw",
  "cage", "cagebw",
  "dummy", "dummybw",
  "flickr", "flickrbw",
  "keanu", "keanubw",
  "kitten", "kittenbw",
  "murray", "murraybw",
  "picsum", "picsumbw",
  "placeholder", "placeholderbw",
  "seagal", "seagalbw"
)

#' Fetch a placeholder image of the correct dimensions
#'
#' Fetch a placeholder image from one of the sites used by web designers.  Append
#' 'bw' to the name to fetch greyscale images instead of colour.
#'
#' \describe{
#'   \item{`bear`}{ - Bears from \url{https://placebear.com/}}
#'   \item{`beard`}{ - Beards! \url{https://placebeard.it/}}
#'   \item{`cage`}{ - Nicholas Cage images from \url{https://placecage.lucidinternets.com}}
#'   \item{`dummy`}{ - Numeric placeholder images from \url{https://dummyimage.com}}
#'   \item{`flickr`}{ - Images from Flickr \url{https://loremflickr.com/}}
#'   \item{`keanu`}{ - Keanu Reeves images from \url{https://placekeanu.com}}
#'   \item{`kitten`}{ - Kittens from \url{https://placecats.com/}}
#'   \item{`murray`}{ - Bill Murrary images from \url{https://fillmurray.lucidinternets.com}}
#'   \item{`picsum`}{ - Images from \url{https://picsum.photos/}}
#'   \item{`placeholder`}{ - Numeric placeholder images from \url{placehold.co}}
#'   \item{`seagal`}{ - Steven Seagal images from \url{https://stevensegallery.lucidinternets.com/}}
#' }
#'
#' @param width,height image dimensions
#' @param type specify the server from which to fetch images. default: 'kitten'
#'
#' @noRd
fetch_placeholder_img <- function(width = 100, height = 100, type = 'dummy') {

  width  <- as.integer(width)
  height <- as.integer(height)

  img_url <- switch(
    type,
    bear           = glue("https://placebear.com/{width}/{height}"),
    bearbw         = glue("https://placebear.com/g/{width}/{height}"),
    beard          = glue("https://placebeard.it/{width}/{height}"),
    beardbw        = glue("https://placebeard.it/g/{width}/{height}"),
    cage           = glue("https://placecage.lucidinternets.com/{width}/{height}"),
    cagebw         = glue("https://placecage.lucidinternets.com/g/{width}/{height}"),
    dummy          = glue("https://dummyimage.com/{width}x{height}"),
    dummybw        = glue("https://dummyimage.com/{width}x{height}/fff/000"),
    flickr         = glue("https://loremflickr.com/{width}/{height}"),
    flickrbw       = glue("https://loremflickr.com/g/{width}/{height}/all"),
    keanu          = glue("https://placekeanu.com/{width}/{height}"),
    keanubw        = glue("https://placekeanu.com/{width}/{height}/g"),
    kitten         = glue("https://placecats.com/{width}/{height}"),
    kittenbw       = glue("https://placecats.com/g/{width}/{height}"),
    murray         = glue("https://fillmurray.lucidinternets.com/{width}/{height}"),
    murraybw       = glue("https://fillmurray.lucidinternets.com/g/{width}/{height}"),
    picsum         = glue("https://picsum.photos/{width}/{height}"),
    picsumbw       = glue("https://picsum.photos/{width}/{height}?grayscale"),
    placeholderbw  = glue("https://placehold.co/{width}x{height}/000000/FFFFFF/png"),
    placeholder    = glue("https://placehold.co/{width}x{height}/png"),
    seagal         = glue("https://www.stevensegallery.lucidinternets.com/{width}/{height}"),
    seagalbw       = glue("https://www.stevensegallery.lucidinternets.com/g/{width}/{height}"),
    {
      warn(glue("Unknown placeholder type '{type}' using 'bear' instead"))
      glue("https://dummyimage.com/{width}x{height}")
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

  assert_suggested("magick", "placeholder")

  if (legend) {
    img <- magick::image_blank(width, height)
    return(convert_img_to_array(img))
  }

  placeholder_type <- check_default(as.character(params$pattern_type), default = 'kitten', type = 'char')
  img <- fetch_placeholder_img(width = width, height = height, type = placeholder_type)

  convert_img_to_array(img)
}
