# The code for this function is in `standalone-guess_has_R4.1_features.R`

#' Guess whether "active" graphics device supports
#' the grid graphics features introduced in R v4.1.
#'
#' `guess_has_R4.1_features()` guesses whether "active" graphics device supports
#' the grid graphics features introduced in R v4.1.  If it guesses it does
#' it returns `TRUE` else `FALSE`.
#'
#' @section Usage in other packages:
#'
#' To avoid taking a dependency on `gridpattern` you may copy the source of `guess_has_R4.1_features()`
#' into your own package under the permissive MIT No Attribution (MIT-0) license.  Either use
#' `usethis::use_standalone("trevorld/gridpattern", "standalone-guess_has_R4.1_features.R")`
#' or copy the file `standalone-guess_has_R4.1_features.R` into your `R` directory and
#' add `grDevices` and `utils` to the `Imports` of your `DESCRIPTION` file.
#'
#' @param features Character vector of features to guess support for.
#'                 Will return `TRUE` only if guesses support for all requested features.\describe{
#'                 \item{"clippingPaths"}{Supports clipping path feature}
#'                 \item{"gradients"}{Supports (both linear and radial) gradient feature}
#'                 \item{"masks"}{Supports (alpha) mask feature}
#'                 \item{"patterns"}{Supports (tiling) pattern feature}
#'                 }
#' @seealso \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html} for more info about the new grid graphics
#'         features introduced in R v4.1.
#' @return `TRUE` if we guess all `features` are supported else `FALSE`
#' @examples
#'   # If R version (weakly) greater than 4.1 should be TRUE
#'   pdf(tempfile(fileext = ".pdf"))
#'   print(guess_has_R4.1_features())
#'   invisible(dev.off())
#'
#'   # Should be FALSE
#'   postscript(tempfile(fileext = ".ps"))
#'   print(guess_has_R4.1_features())
#'   invisible(dev.off())
#'
#' @export
guess_has_R4.1_features <- guess_has_R4.1_features
