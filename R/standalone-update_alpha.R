# ---
# repo: trevorld/gridpattern
# file: standalone-update_alpha.R
# last-updated: 2024-04-18
# license: https://spdx.org/licenses/MIT.html
# imports: [grDevices, grid, rlang]
# ---
#
# nocov start
#
# You may copy this source into your own R package
# by either using `usethis::use_standalone("trevorld/gridpattern", "standalone-update_alpha.R")`
# or simply copying this file into your `R` directory and adding `grDevices`, `grid`, and `rlang` to
# the `Imports` of your `DESCRIPTION` file.

# The MIT License (MIT)
# =====================
#
# Copyright © 2024 Trevor L. Davis
# Copyright © 2020 mikefc@coolbutuseless.com
# Copyright © 2023 ggplot2 authors
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the “Software”), to deal in the Software without
# restriction, including without limitation the rights to use,
# copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following
# conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.

# Pattern utilities mainly added to ggplot2 by Teun van den Brand
# Tweaked by Trevor L. Davis to remove external dependencies
# and work better for {ggpattern} / {gridpattern} use cases.
update_alpha <- function(fill, alpha) {
  if (!is.list(fill)) {
    # Happy path of no patterns
    update_alpha_col(fill, alpha)
  } else if (is_pattern(fill) || any(vapply(fill, is_pattern, logical(1)))) {
    # Path with patterns
    update_pattern_alpha(fill, alpha)
  } else if (is.list(fill) && length(fill) == 1L && !any(vapply(fill, is_pattern, logical(1)))) {
    # List of length one of (possibly multiple) colours
    update_alpha_col(fill[[1L]], alpha)
  } else {
    # We are either dealing with faulty fill specification
    stop("`fill` must be a vector of colours or list of <GridPattern> objects.")
  }
}

# Similar to grid:::is.pattern
is_pattern <- function(x) {
  inherits(x, "GridPattern")
}

# replacement for `scales::alpha()` that only depends on {grDevices}
update_alpha_col <- function(colour, alpha = NA_real_) {
    n <- max(lengths(list(colour, alpha)))
    colour <- rep_len(colour, n)
    alpha <- rep_len(alpha, n)
    m <- grDevices::col2rgb(colour, alpha = TRUE) / 255.0
    m[4, ] <- ifelse(is.na(alpha), m[4, ], alpha)
    apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3], x[4]))
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
  grid::pattern(grid::rectGrob(name = name),
                gp = grid::gpar(fill = update_alpha_col(x, alpha)))
}

#' @export
update_pattern_alpha.GridPattern <- function(x, alpha, ...) {
  x$colours <- update_alpha_col(x$colours, alpha[1])
  x
}

#' @export
update_pattern_alpha.GridTilingPattern <- function(x, alpha, ...) {
  if (all(is.na(alpha) | alpha == 1)) {
    return(x)
  }
  grob <- rlang::env_get(environment(x$f), "grob")
  gp <- grid::gpar(fill = update_alpha_col("white", alpha))
  mask <- grid::as.mask(grid::rectGrob(gp = gp))
  if (is.null(grob$vp)) {
    grob$vp <- grid::viewport(mask = mask)
  } else {
    grob$vp <- grid::editViewport(grob$vp, mask = mask)
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

# nocov end
