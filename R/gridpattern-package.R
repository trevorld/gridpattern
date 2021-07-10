#' @section Package options:
#' The following `gridpattern` options may be set globally via [base::options()]:
#'  \describe{
#'    \item{ggpattern_geometry_funcs}{Set custom \dQuote{geometry} pattern functions.}
#'    \item{ggpattern_array_funcs}{Set custom \dQuote{array} pattern functions.}
#'    \item{ggpattern_use_R4.1_clipping}{If `TRUE` use the grid clipping path feature introduced in R v4.1.0
#'                          else do a `rasterGrob` approximation of the clipped pattern.
#'                          Note not all graphic devices support the grid clipping path feature.}
#'  }
#' @keywords internal
"_PACKAGE"
