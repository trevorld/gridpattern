#' Fetch a given path or URL as a magick image
#'
#' @param filename filename or URL
#'
#' @return magick image
#' @noRd
img_read <- function(filename) {
  if (identical(filename, '')) {
    return(magick::image_blank(100, 100, color = 'none'))
  }
  if (is.null(filename) || length(filename)== 0 || is.na(filename) || filename == '') {
    abort(paste0("bad filename: ", deparse(filename)))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fetch the URL as an image
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  img <- tryCatch(
    {magick::image_read(filename)},
    error = function(cond) {
      msg <- c(glue("couldn't read {shQuote(filename)}"),
               i = cond$message)
      abort(msg)
    }
  )
  img
}

#' Fetch a given path or URL as a 3D RGB array of values
#'
#' @param filename filename or URL
#' @param width,height if specified scale the image to these dimensions before
#'        converting to an array
#' @param fill_type how to fill the image area. 'none', 'fit',
#'        'squish', 'expand', 'tile'
#' @param gravity imagemagick gravity option on how to position an image during
#'                a resize/fill
#' @param scale scale image prior to tiling. Only for fill_type == 'tile'
#' @param filter filter for scaling. default: lanczos. only for fill_type = 'tile'
#'
#' @return 3D array. If the image was pure gray, then it will be promoted to
#'         be an unsaturated RGB image.
#'
#' @noRd
img_read_as_array <- function(filename, width = NULL, height = NULL,
                              fill_type = 'squish', gravity = 'Center',
                              scale = 1, filter = 'lanczos') {

  img <- img_read_memoised(filename)

  if (is.null(img)) {
    abort(glue("couldn't read {shQuote(filename)}"))
  }

  img <- fill_area_with_img(img, width, height, type = fill_type, gravity = gravity,
                            filter = filter, scale = scale)

  convert_img_to_array(img)
}
