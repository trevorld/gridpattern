# dependency-free replacement for scales::alpha
alpha <- function(colour, alpha = NA_real_) {
    n <- max(lengths(list(colour, alpha)))
    colour <- rep(colour, length.out = n)
    alpha <- rep(alpha, length.out = n)
    m <- grDevices::col2rgb(colour, alpha=TRUE) / 255.0
    m[4, ] <- ifelse(is.na(alpha), m[4, ], alpha)
    apply(m, 2, function(x) grDevices::rgb(x[1], x[2], x[3], x[4]))
}

augment_list <- function(l, name, default) {
    name <- paste0("pattern_", name)
    if (!hasName(l, name)) l[[name]] <- default
    l
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

# ggplot2 constant
.pt <- 2.845276
