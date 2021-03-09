#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sanity check an argument value
##
## This is a milder, more explicit version of \code{match.arg} which gives a warning
## and returns a good default.  match.arg() would throw errors which makes
## geom/pattern development a bit trickier.
##
## @param x argument value
## @param options valid options this value could take. If this is not NULL, then
##        x is checked to be a member of this set.  Default: NULL.
## @param default the default value to use if the argument fails the checks.
##        If default is not NULL, then use it as the fallback value.
##        If default is NULL and options is not NULL, then use the first value
##        in \code{options}.
## @param type NULL, 'num' or 'char'
## @param prefix prefix of warning message.
## @param verbose default: FALSE
##
## @return original \code{x} value if there are no issues, otherwise the \code{default}
##         if given, otherwise the first element in \code{options}.
#' @importFrom rlang %||% warn abort
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
check_default <- function(x, options = NULL, default = NULL, type = NULL, prefix = "", verbose = FALSE) {

  stopifnot(is.null(options) || is.atomic(options))

  default <- default %||% (options[1])
  if (is.null(default) || length(default) != 1) {
    abort("check_default(): Must specify 'default' or 'options'")
  }


  if (length(x) != 1) {
    if (verbose) {
      warn("check_default(): ", prefix,
           " Value should be length 1, but got ", deparse(x),
           ". Using default: ", default, call.=FALSE)
    }
    res <- default
  } else if (!is.null(options) && !x %in% options) {
    if (verbose) {
      warn("check_default(): ", prefix,
           " Value should be one of ", deparse(options),
           " but got ", deparse(x),
           ". Using default: ", default, call.=FALSE)
    }
    res <- default
  } else {
    res <- x
  }

  if (!is.null(type)) {
    res <- switch(
      type,
      numeric   =,
      number    =,
      float     =,
      num       = ifelse(is.numeric  (res), res, default),
      character = ,
      chr       = ,
      char      = ifelse(is.character(res), res, default),
      {
        abort("check_default(): Don't know how to check for type: ", type)
      }
    )
  }

  res
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## abind clone for adding a matrix to an array
##
## A very cut-down version of \code{abind::abind()} that will only stack a
## matrix onto an array.
##
## @param arr,mat array and matrix to be stacked
##
## @return new array with matrix added as a new plane at the end of the array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_abind <- function(arr, mat) {

  stopifnot(is.array(arr))
  stopifnot(is.matrix(mat))
  if (!identical(utils::head(dim(arr), -1), dim(mat))) {
    abort("Dimension missmatch. Array: ", deparse(dim(arr)), "  Matrix: ", deparse(dim(mat)))
  }

  new_dim    <- dim(arr)
  new_dim[3] <- new_dim[3] + 1

  array(c(arr, mat), dim = new_dim)
}
