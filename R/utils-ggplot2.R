# replacement for scales::alpha that only depends on grDevices
alpha <- function(colour, alpha = NA_real_) {
    n <- max(lengths(list(colour, alpha)))
    colour <- rep(colour, length.out = n)
    alpha <- rep(alpha, length.out = n)
    m <- grDevices::col2rgb(colour, alpha=TRUE) / 255.0
    m[4, ] <- ifelse(is.na(alpha), m[4, ], alpha)
    apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3], x[4]))
}

# Added to ggplot2 by Thomas Lin Pedersen
# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}
data_frame <- function(...) {
  new_data_frame(list(...))
}
.pt <- 2.845276 # ggplot2 constant

# Pattern utilities mainly added to ggplot2 by Teun van den Brand

#' Update fill colour and/or pattern transparency
#'
#' `update_alpha()` modifies the transparency of fill colours and/or patterns.
#'
#' * Like [ggplot2::fill_alpha()] but unlike [scales::alpha()] it also attempts
#'   to set the transparency of `<GridPattern>` objects.
#' * Unlike [ggplot2::fill_alpha()] it will work on a list of length one
#'   with more than one color.
#' * `update_alpha()` does not depend on `ggplot2` or `scales`.
#'
#' @param fill A fill colour given as a `character` or `integer` vector, or as a
#'   (list of) `<GridPattern>` object(s) and/or colour(s).
#' @param alpha A transparency value between 0 (transparent) and 1 (opaque),
#'   parallel to `fill`.
#'
#' @return A `character` vector of colours or list of `<GridPattern>` objects.
#' @export
#'
#' @examples
#' # Typical colour input
#' update_alpha("red", 0.5)
#'
#' if (utils::packageVersion("grid") > "4.1") {
#'   # Pattern input
#'   update_alpha(list(grid::linearGradient()), 0.5)
#' }
update_alpha <- function(fill, alpha) {
  if (!is.list(fill)) {
    # Happy path of no patterns
    alpha(fill, alpha)
  } else if (is_pattern(fill) || any(vapply(fill, is_pattern, logical(1)))) {
    # Path with patterns
    update_pattern_alpha(fill, alpha)
  } else if (is.list(fill) && length(fill) == 1L && !any(vapply(fill, is_pattern, logical(1)))) {
    # List of length one of (possibly multiple) colours
    alpha(fill[[1L]], alpha)
  } else {
    # We are either dealing with faulty fill specification
    stop("`fill` must be a vector of colours or list of <GridPattern> objects.")
  }
}

# Similar to grid:::is.pattern
is_pattern <- function(x) {
  inherits(x, "GridPattern")
}

#' Modify transparency for patterns
#'
#' This generic allows you to add your own methods for adding transparency to
#' pattern-like objects.
#'
#' @param x Object to be interpreted as pattern.
#' @param alpha A `numeric` vector between 0 and 1. If `NA`, alpha values
#'   are preserved.
#'
#' @return `x` with modified transparency
#' @noRd
update_pattern_alpha <- function(x, alpha, ...) {
  UseMethod("update_pattern_alpha")
}

#' @export
update_pattern_alpha.default <- function(x, alpha, ..., name = NULL) {
  if (!is.atomic(x)) {
    stop("Can't apply `update_pattern_alpha()` to this object.")
  }
  pattern(rectGrob(name = name), gp = gpar(fill = alpha(x, alpha)))
}

#' @export
update_pattern_alpha.GridPattern <- function(x, alpha, ...) {
  x$colours <- alpha(x$colours, alpha[1])
  x
}

#' @export
update_pattern_alpha.GridTilingPattern <- function(x, alpha, ...) {
  if (all(is.na(alpha) | alpha == 1)) {
    return(x)
  }
  grob <- rlang::env_get(environment(x$f), "grob")
  mask <- as.mask(rectGrob(gp = gpar(fill = alpha("white", alpha))))
  if (is.null(grob$vp)) {
    grob$vp <- viewport(mask = mask)
  } else {
    grob$vp <- editViewport(grob$vp, mask = mask)
  }
  new_env <- new.env(parent = environment(x$f))
  rlang::env_bind(new_env, grob = grob)
  environment(x$f) <- new_env
  x
}

#' @export
update_pattern_alpha.list <- function(x, alpha, ...) {
  Map(update_pattern_alpha, x = x, alpha = alpha)
}
