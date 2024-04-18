# The code for this function is in `standalone-update_alpha.R`

#' Update colour and/or pattern transparency
#'
#' `update_alpha()` modifies the transparency of colours and/or patterns.
#'
#' * This is a fork of pattern utilities mainly added to `{ggplot2}` by Teun van den Brand.
#' * `update_alpha()` does not depend on `{ggplot2}` or `{scales}`.
#' * Like [ggplot2::fill_alpha()] but unlike [scales::alpha()] it also attempts
#'   to set the transparency of `<GridPattern>` objects.
#' * Unlike [ggplot2::fill_alpha()] it will work on a list of length one
#'   containing a vector of color strings.
#'
#' @section Usage in other packages:
#'
#' To avoid taking a dependency on `gridpattern` you may copy the source of `update_alpha()`
#' into your own package under the permissive MIT license.  Either use
#' `usethis::use_standalone("trevorld/gridpattern", "standalone-update_alpha.R")`
#' or copy the file `update_alpha.R` into your `R` directory and
#' add `grDevices`, `grid`, and `rlang` to the `Imports` of your `DESCRIPTION` file.
#'
#' @param fill A fill colour given as a `character` or `integer` vector, or as a
#'   (list of) `<GridPattern>` object(s) and/or colour(s).
#' @param alpha A transparency value between 0 (transparent) and 1 (opaque),
#'   parallel to `fill`.
#' @return A `character` vector of colours or list of `<GridPattern>` objects.
#' @examples
#' # Typical color input
#' update_alpha("red", 0.5)
#'
#' # Pattern input
#' if (getRversion() >= "4.2" && requireNamespace("grid", quietly = TRUE)) {
#'   update_alpha(list(grid::linearGradient()), 0.5)
#' }
#' @export
update_alpha <- update_alpha
