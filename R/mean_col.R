#' Compute average color
#'
#' `mean_col()` computes an average color.
#'
#' We currently compute an average color
#' by using the quadratic mean of the colors' RGBA values.
#'
#' @param ... Colors to average
#' @examples
#'  mean_col("black", "white")
#'  mean_col(c("black", "white"))
#'  mean_col("red", "blue")
#' @export
mean_col <- function(...) {
    cols <- unlist(list(...))
    m <- grDevices::col2rgb(cols, alpha=TRUE) / 255.0
    # quadratic mean suggested at https://stackoverflow.com/a/29576746
    v <- apply(m, 1, quadratic_mean)
    grDevices::rgb(v[1], v[2], v[3], v[4])
}

quadratic_mean <- function(x) sqrt(mean(x^2))
