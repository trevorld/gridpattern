#' Convert an R colour to a magick colour
#'
#' @param col may be a built in colour, like 'tomato' or a hex colour
#'
#' @return always returns a hex colour, except if col is NA when it returns a special
#' magick colour 'none', which means transparent
#'
#' @noRd
convert_r_colour_to_magick_colour <- function(col) {
	if (is.null(col) || is.na(col) || length(col) == 0 || col == "transparent") {
		return('none')
	}
	col_rgb <- col2rgb(col, alpha = TRUE)
	rgb(col_rgb[1, ], col_rgb[2, ], col_rgb[3, ], col_rgb[4, ], maxColorValue = 255)
}

#' Convert a magick image to an RGBA array.
#'
#' This will promote gray or RGB images to RGBA arrays.
#'
#' @param img magick image
#'
#' @return RGBA array with all values in range `[0, 1]`
#'
#' @noRd
convert_img_to_array <- function(img) {
	stopifnot(inherits(img, 'magick-image'))
	# `channels = "rgba"` promotes gray (with or without alpha) and RGB images to RGBA
	as.numeric(magick::image_data(img, channels = "rgba"))
}
