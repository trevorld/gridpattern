#' Hex pattern matrix
#'
#' `pattern_hex()` returns an integer matrix indicating where each
#'  color (or other graphical element) should be drawn on a (horizontal) hex grid
#'  for a specified hex pattern type and subtype.
#'  `names_hex` lists the currently supported hex `type`s.
#'
#'  Currently we cycle through elements on the horizontal line and "main" (downward-sloping) diagonal line.
#'  If `subtype` is `3L` this provides a 3-color hexagonal tiling pattern.
#'  If `subtype` is strictly greater than `2L` then a hexagon should never touch another hexagon of the same color.
#'
#' @param type Either "hex" or "hex_circle".
#'             However for this function there is currently no difference in
#'             behaviour between the two (unlike in [grid.pattern_regular_polygon()]).
#' @param subtype An integer indicating number of colors (or other graphical elements).
#' @param nrow Number of rows (height).
#' @param ncol Number of columns (width).
#' @return A matrix of integer values indicating where the each color
#'         or other graphical elements should be drawn on a horizontal hex grid
#'         (i.e. hexagons are assumed to be pointy side up).
#'         Indices `[1,1]` of the matrix corresponds to the bottom-left of the grid
#'         while indices `[1,ncol]` corresponds to the bottom-right of the grid.
#'         The even rows are assumed to be on the **left** of the ones on the odd rows
#'         (for those in the same column in the matrix).
#'         This matrix has a "pattern_hex" subclass which supports a special `print()` method.
#' @examples
#'  # supported hex names
#'  print(names_hex)
#'
#'  hex_3color <- pattern_hex("hex", 3L, nrow = 7L, ncol = 9L)
#'  print(hex_3color)
#'
#' @seealso [grid.pattern_regular_polygon()] for drawing to a graphics device
#'           hexagons, triangles, circles, etc. in hexagon patterns.
#'
#' @export
pattern_hex <- function(type = "hex", subtype = NULL, nrow = 5L, ncol = 5L) {
    if (is.null(subtype) || is.na(subtype)) subtype <- 3L
    stopifnot(is_integer(subtype))
    m <- matrix(1L, nrow = nrow, ncol = ncol)
    n <- as.integer(subtype)
    s <- seq.int(n)
    skip <- 0L
    for (i in seq.int(nrow)) {
        step <- skip + i - 1L
        v <- rep(cycle_elements(s, step), length.out = ncol)
        m[i, ] <- v
        if (i %% 2 == 0) skip <- skip + 1L
    }
    class(m) <- c("pattern_hex", class(m))
    m
}

#' @rdname pattern_hex
#' @export
names_hex <- c("hex", "hex_circle")

#' @export
print.pattern_hex <- function(x, ...) {
    d <- dim(x)
    x <- matrix(int_to_char(x), nrow = d[1], ncol = d[2])
    cat("/", rep("-", 2 * ncol(x)), "\\", "\n", sep = "")
    for (i in rev(seq_len(nrow(x)))) {
        if (i %% 2 == 1)
            cat("|", paste0(" ", x[i, ]), "|", "\n", sep = "")
        else
            cat("|", paste0(x[i, ], " "), "|", "\n", sep = "")
    }
    cat("\\", rep("-", 2 * ncol(x)), "/", "\n", sep = "")
    invisible(NULL)
}
