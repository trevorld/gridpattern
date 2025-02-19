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

ggplot2pat <- function(gg) {
    stopifnot(getRversion() >= "4.1.0",
              requireNamespace("ggplot2", quietly = TRUE),
              requireNamespace("gtable", quietly = TRUE))
    gg <- suppressMessages(gg +
                           ggplot2::scale_x_continuous(expand=c(0, 0)) +
                           ggplot2::scale_y_continuous(expand=c(0, 0)))
    grob <- gtable::gtable_filter(ggplot2::ggplotGrob(gg), "panel")
    pat <- grid::pattern(grob)
    pat
}
