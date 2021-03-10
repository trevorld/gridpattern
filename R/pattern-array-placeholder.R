#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fetch a placeholder image of the correct dimensions
##
## Fetch a placeholder image from one of the sites used by web designers.  Append
## 'bw' to the name to fetch greyscale images instead of colour.
##
## \describe{
## \item{\code{kitten}}{ - Kittens from \url{https://placekitten.com/}}
## \item{\code{picsum}}{ - Random images from \url{https://picsum.photos/}}
## \item{\code{murray}}{ - Bill Murrary images from \url{http://www.fillmurray.com}}
## \item{\code{cage}}{ - Nicholas Cage images from \url{https://www.placecage.com/}}
## \item{\code{bear}}{ - Bears from \url{https://placebear.com/}}
## \item{\code{seagal}}{ - Steven Seagal images from \url{https://www.stevensegallery.com/}}
## \item{\code{flickr}}{ - Images from Flickr \url{https://loremflickr.com/}}
## \item{\code{beard}}{ - Beards! \url{http://placebeard.it/}}
## \item{\code{lorempixel}}{ - Random images from \url{http://lorempixel.com/}}
## \item{\code{placeimg}}{ - Random images from \url{http://placeimg.com/}}
## \item{\code{dummy}}{ - Numeric placeholder images from \url{https://dummyimage.com}}
## }
##
##
## @param width,height image dimensions
## @param type specify the server from which to fetch images. default: 'kitten'
##
##
## @import magick
## @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fetch_placeholder_img <- function(width = 100, height = 100, type = 'kitten') {

  width  <- as.integer(width)
  height <- as.integer(height)

  img_url <- switch(
    type,
    kitten         = glue::glue("https://placekitten.com/{width}/{height}"),
    kittenbw       = glue::glue("https://placekitten.com/g/{width}/{height}"),
    picsum         = glue::glue("https://picsum.photos/{width}/{height}"),
    picsumbw       = glue::glue("https://picsum.photos/{width}/{height}?grayscale"),
    murray         = glue::glue("http://www.fillmurray.com/{width}/{height}"),
    murraybw       = glue::glue("http://www.fillmurray.com/g/{width}/{height}"),
    cage           = glue::glue("https://www.placecage.com/{width}/{height}"),
    cagebw         = glue::glue("https://www.placecage.com/g/{width}/{height}"),
    bear           = glue::glue("https://placebear.com/{width}/{height}"),
    bearbw         = glue::glue("https://placebear.com/g/{width}/{height}"),
    seagal         = glue::glue("https://www.stevensegallery.com/{width}/{height}"),
    seagalbw       = glue::glue("https://www.stevensegallery.com/g/{width}/{height}"),
    placeholderbw  = ,
    placeholder    = glue::glue("https://via.placeholder.com/{width}x{height}.png"),
    dummybw        = ,
    dummy          = glue::glue("https://dummyimage.com/{width}x{height}"),
    flickr         = glue::glue("https://loremflickr.com/{width}/{height}"),
    flickrbw       = glue::glue("https://loremflickr.com/g/{width}/{height}/all"),
    beard          = glue::glue("http://placebeard.it/{width}/{height}"),
    beardbw        = glue::glue("http://placebeard.it/g/{width}/{height}"),
    lorempixel     = glue::glue("http://lorempixel.com/{width}/{height}"),
    lorempixelbw   = glue::glue("http://lorempixel.com/g/{width}/{height}"),
    placeimg       = glue::glue("http://placeimg.com/{width}/{height}/any"),
    placeimgbw     = glue::glue("http://placeimg.com/{width}/{height}/any/grayscale"),
    keanu          = glue::glue("https://placekeanu.com/{width}/{height}"),
    keanubw        = glue::glue("https://placekeanu.com/{width}/{height}/g"),
    {
      # warn("fetch_placeholder_image_as_array(): Unknown pattern_type (", type, ") ",
      #         "using 'kitten'")
      glue::glue("https://placekitten.com/{width}/{height}")
    }
  )

  img_read_memoised(filename = img_url)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## All placeholder names
## @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
placeholder_names <- c(
  "kitten", "kittenbw", "picsum", "picsumbw", "murray", "murraybw",
  "cage", "cagebw", "bear", "bearbw", "seagal", "seagalbw", "placeholderbw",
  "placeholder", "dummybw", "dummy", "flickr", "flickrbw", "beard",
  "beardbw", "lorempixel", "lorempixelbw", "placeimg", "placeimgbw",
  'keanu', 'keanubw'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fetch a placeholder image of the correct size and return as an array
##
## @inheritParams create_gradient_as_array
##
## @return RGBA array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fetch_placeholder_array <- function(width, height, params, legend) {
  if (legend) {
    img <- magick::image_blank(width, height)
    return(convert_img_to_array(img))
  }

  placeholder_type <- check_default(as.character(params$pattern_type), default = 'kitten', type = 'char')
  img <- fetch_placeholder_img(width = width, height = height, type = placeholder_type)

  convert_img_to_array(img)
}
