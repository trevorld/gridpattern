#' Compute average color
#'
#' `mean_col()` computes an average color.
#'
#' We currently compute an average color
#' by using the quadratic mean of the colors' RGBA values.
#'
#' @param ... Colors to average
#' @return A color string of 9 characters: `"#"` followed by the
#'         red, blue, green, and alpha values in hexadecimal.
#' @examples
#'  mean_col("black", "white")
#'  mean_col(c("black", "white"))
#'  mean_col("red", "blue")
#' @seealso [mix_col()] for subtractive (pigment) mixing via Munsell color space.
#' @export
mean_col <- function(...) {
	cols <- unlist(list(...))
	m <- grDevices::col2rgb(cols, alpha = TRUE) / 255.0
	# quadratic mean suggested at https://stackoverflow.com/a/29576746
	v <- apply(m, 1, quadratic_mean)
	grDevices::rgb(v[1], v[2], v[3], v[4])
}

quadratic_mean <- function(x) sqrt(mean(x^2))

#' Mix colors via Munsell color space
#'
#' `mix_col()` simulates subtractive (pigment) color mixing by converting
#' input colors to Munsell notation via [aqp::col2Munsell()], mixing them with
#' [aqp::mixMunsell()], and converting the result back to an R color string
#' via [aqp::parseMunsell()].
#'
#' @param ... Colors to mix.  Can be individual color strings or character vectors;
#'   all are combined into a single vector.
#' @param w A numeric vector of weights or proportions the same length as the combined color vector.
#'   Defaults to equal weights.
#' @param mixingMethod Mixing method passed to [aqp::mixMunsell()].
#' @return A single R color string in hex notation.
#' @examples
#' should_run_ex <- requireNamespace("aqp", quietly = TRUE) &&
#'     (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true"))
#' if (should_run_ex) {
#'   mix_col(c("red", "blue"))
#' }
#' if (should_run_ex) {
#'   mix_col("red", "yellow", "blue", w = c(2, 1, 1))
#' }
#' @seealso [mean_col()] for a simpler quadratic-mean RGB approach (no extra packages required).
#' @export
mix_col <- function(..., w = NULL, mixingMethod = "adaptive") {
	assert_suggested("aqp", fn = "mix_col")
	cols <- unlist(list(...))
	w <- w %||% rep.int(1, length(cols))
	df <- aqp::col2Munsell(cols)
	s <- aqp::formatMunsell(df$hue, df$value, df$chroma)
	mixed <- suppressMessages(aqp::mixMunsell(s, w = w, mixingMethod = mixingMethod))
	aqp::parseMunsell(mixed$munsell[1L])
}
