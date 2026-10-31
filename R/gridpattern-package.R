#' @section Package options:
#' The following `gridpattern` options may be set globally via [base::options()]:
#'  \describe{
#'    \item{ggpattern_array_funcs}{Legacy option to set custom \dQuote{array} pattern functions.
#'                                  Should use `register_pattern(kind = "array")` instead.}
#'    \item{ggpattern_geometry_funcs}{Legacy option to set custom \dQuote{geometry} pattern functions.
#'                                     Should use [register_pattern()] instead.}
#'    \item{ggpattern_res}{Set custom raster image resolution (pixels per inch) for certain patterns
#'                         (and the raster fallbacks of [alphaMaskGrob()] and [clippingPathGrob()]).
#'                         If unset defaults to (a little more than) the resolution of the graphics device.}
#'    \item{ggpattern_use_R4.1_clipping}{If `TRUE` use the grid clipping path feature introduced in R v4.1.0.
#'                          If `FALSE` do a `rasterGrob` approximation of the clipped pattern.
#'                          If `NULL` try to guess an appropriate choice.}
#'    \item{ggpattern_use_R4.1_features}{If `TRUE` sets the default for all the other
#'                          `ggpattern_use_R4.1_*` options arguments to `TRUE`.
#'                          If `FALSE` sets them to `FALSE`.}
#'    \item{ggpattern_use_R4.1_gradients}{If `TRUE` use the grid gradient feature introduced in R v4.1.0.
#'                          If `FALSE` do a `rasterGrob` approximation of the gradient pattern.
#'                          If `NULL` try to guess an appropriate choice.}
#'    \item{ggpattern_use_R4.1_masks}{If `TRUE` use the grid mask feature introduced in R v4.1.0.
#'                          If `FALSE` do a `rasterGrob` approximation of the masked pattern.
#'                          If `NULL` try to guess an appropriate choice.}
#'    \item{ggpattern_use_R4.1_patterns}{If `TRUE` use the grid pattern feature introduced in R v4.1.0.
#'                          Currently only used by a couple of examples.}
#'  }
#'  Note not all graphic devices support any/all of the R v4.1.0 features.
#'  See \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html} for more information on these features.
"_PACKAGE"
