#' Reset 'gridpattern' image cache
#'
#' [grid.pattern_image()] and [grid.pattern_placeholder()] store images in a cache
#' (so we won't download image URLs over and over).
#' `reset_image_cache()` resets this cache.
#' @export
reset_image_cache <- function() {
    memoise::forget(img_read_memoised)
}
