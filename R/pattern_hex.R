#' Hex pattern matrix
#'
#' `pattern_hex()` returns an integer matrix indicating where each
#'  color (or other graphical element) should be drawn on a (horizontal) hex grid
#'  for a specified hex pattern type and subtype.
#'  `names_hex` lists the currently supported hex `type`s.
#'
#' \describe{
#' \item{"hex"}{Attempts to use a uniform coloring if it exists.
#'              For subtype `1L`, `2L`, and `3L` we use the "hex1" pattern.
#'              For subtype `4L` we use the "hex2" pattern.
#'              For subtype `7L` we use the "hex3" pattern.
#'              Else a uniform coloring does not exist and we use the "hex_skew" pattern.}
#' \item{"hex_skew"}{For the "hex_skew" `type` we cycle through `subtype` elements on the horizontal line and "main" diagonal line.
#'    For some `subtype` numbers this may lead to noticeable color repeats on the "skew" diagonal line.
#'    If `subtype` is strictly greater than `2L` then a hexagon should never touch another hexagon of the same color.}
#' \item{"hex1"}{Provides the 1-uniform colorings of a hexagonal tiling.  Only exists for `subtype` `1L`, `2L`, or `3L`.
#'               Only the `2L` `subtype` gives a different answer than the default "hex" type.}
#' \item{"hex2"}{Provides the 2-uniform colorings of a hexagonal tiling.  Only exists for `subtype` `2L` or `4L`.}
#' \item{"hex3"}{Provides the 3-uniform colorings of a hexagonal tiling.  Only exists for `subtype` `2L` or `7L`.}
#' }
#'
#' @param type Currently just supports "hex".
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
#'  # 1-uniform 3-color
#'  hex_3color <- pattern_hex("hex1", 3L, nrow = 7L, ncol = 9L)
#'  print(hex_3color)
#'
#'  # 2-uniform 4-color
#'  hex_4color <- pattern_hex("hex2", 4L, nrow = 7L, ncol = 9L)
#'  print(hex_4color)
#'
#' @seealso [grid.pattern_regular_polygon()] for drawing to a graphics device
#'           hexagons, triangles, circles, etc. in hexagon patterns.
#'          The tiling vignette features several examples of regular polygon tiling using
#'          this both the "hex" and "hex_circle" types `vignette("tiling", package = "gridpattern")`.
#'          For more information on uniform colorings of a hexagonal tiling see
#'          \url{https://en.wikipedia.org/wiki/Hexagonal_tiling#Uniform_colorings}.
#' @export
pattern_hex <- function(type = "hex", subtype = NULL, nrow = 5L, ncol = 5L) {
    if (is.null(subtype) || is.na(subtype)) subtype <- 2L
    stopifnot(is_integer(subtype))
    n <- as.integer(subtype)
    if (type == "hex") {
        if (n < 4L) {
            type <- "hex1"
        } else if (n == 4L) {
            type <- "hex2"
        } else if (n == 7L) {
            type <- "hex3"
        } else {
            type <- "hex_skew"
        }
    }
    m <- switch(type,
                hex_skew = pattern_hex_skew(n, nrow, ncol),
                hex1 = pattern_hex1(n, nrow, ncol),
                hex2 = pattern_hex2(n, nrow, ncol),
                hex3 = pattern_hex3(n, nrow, ncol),
                abort(paste("Don't know hex pattern type", type)))
    class(m) <- c("pattern_hex", "matrix", "array")
    m
}

pattern_hex_skew <- function(n = NULL, nrow = 5L, ncol = 5L) {
    m <- matrix(1L, nrow = nrow, ncol = ncol)
    if (n == 1L) return(m)
    s <- seq.int(n)
    skip <- 0L
    for (i in seq.int(nrow)) {
        step <- skip + i - 1L
        v <- rep_len(cycle_elements(s, step), ncol)
        m[i, ] <- v
        if (i %% 2 == 0) skip <- skip + 1L
    }
    m
}

pattern_hex1 <- function(n, nrow, ncol) {
    stopifnot(n < 4L)
    if (n == 2L) {
        m <- pattern_hex_skew(3L, nrow, ncol)
        indices <- which(m == 3L)
        m[indices] <- 2L
        m
    } else {
        pattern_hex_skew(n, nrow, ncol)
    }
}

pattern_hex2 <- function(n, nrow, ncol) {
    stopifnot(n == 2L || n == 4L)
    m <- matrix(2L, nrow = nrow, ncol = ncol)
    v1 <- rep_len(1:2, ncol)
    v3 <- rep_len(2:1, ncol)
    for (i in seq.int(nrow)) {
        im <- i %% 4L
        if (im == 1L) {
            m[i, ] <- v1
        } else if (im == 3L) {
            m[i, ] <- v3
        }
    }
    if (n == 4L) {
        v2 <- rep_len(3:4, ncol)
        v4 <- rep_len(4:3, ncol)
        for (i in seq.int(nrow)) {
            im <- i %% 4L
            if (im == 2L) {
                m[i, ] <- v2
            } else if (im == 0L) {
                m[i, ] <- v4
            }
        }
    }
    m
}

pattern_hex3 <- function(n, nrow, ncol) {
    stopifnot(n == 2L || n == 7L)
    m <- matrix(1L, nrow = nrow, ncol = ncol)
    if (n == 2L)
        s <- c(1L, rep_len(2L, 6L))
    else
        s <- seq.int(7L)
    skip <- 0L
    for (i in seq.int(nrow)) {
        step <- skip + 4 * (i - 1L)
        v <- rep_len(cycle_elements(s, step), ncol)
        m[i, ] <- v
        if (i %% 2 == 0) skip <- skip + 1L
    }
    m
}

#' @rdname pattern_hex
#' @export
names_hex <- c("hex", "hex1", "hex2", "hex3", "hex_skew")

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
