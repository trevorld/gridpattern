#' Square pattern matrix
#'
#' `pattern_square()` returns an integer matrix indicating where each
#' color (or other graphical element) should be drawn on a rectangular grid
#' for a specified square pattern type and subtype.
#' `names_square` lists the currently supported square `type`s (excluding those in `names_weave`).
#'
#' "diagonal" and "diagonal_skew" simply cycle through the colors
#' both horizontally and vertically.
#' If two colors are requested this provides the standard two-color checkerboard pattern.
#' If there are more than three colors than "diagonal" will have colored diagonals
#' going from top left to bottom right while "diagonal_skew" will have them
#' going form bottom left to top right.
#'
#' The weave patterns simply convert the logical matrix returned by [pattern_weave()] into an
#' integer matrix by having `TRUE` set to `1L` and `FALSE` set to `2L`.
#' Hence the various weave patterns only support (up to) two-color patterns.
#'
#' @param type Either "diagonal" or "diagonal_skew" or any of the weave patterns
#'             documented in [pattern_weave()].
#' @param subtype For the "diagonal" and "diagonal_skew" patterns
#'                an integer indicating number of colors (or other graphical elements).
#'                See [pattern_weave()] for supported `subtype` arguments for the various weave patterns.
#' @param nrow Number of rows (height).
#' @param ncol Number of columns (width).
#' @return A matrix of integer values indicating where the each color
#'         (or other graphical element) should be drawn on a rectangular grid.
#'         Indices `[1,1]` of the matrix corresponds to the bottom-left of the grid
#'         while indices `[1,ncol]` corresponds to the bottom-right of the grid.
#'         This matrix has a "pattern_square" subclass which supports a special `print()` method.
#' @examples
#'  # supported square names
#'  print(names_square)
#'
#'  # (main) diagonal has colors going from top left to bottom right
#'  diagonal <- pattern_square("diagonal", 4L, nrow = 7L, ncol = 9L)
#'  print(diagonal)
#'
#'  # skew diagonal has colors going from bottom left to top right
#'  skew <- pattern_square("diagonal_skew", 4L, nrow = 7L, ncol = 9L)
#'  print(skew)
#'
#'  # also supports the various 'weave' patterns
#'  zigzag <- pattern_square("twill_zigzag", nrow = 15L, ncol = 9L)
#'  print(zigzag)
#'
#' @seealso [grid.pattern_regular_polygon()] for drawing to a graphics device
#'           polygons in multiple color/size/shape patterns.
#'           [pattern_weave()] for more information on "weave" patterns.
#' @export
pattern_square <- function(type = "diagonal", subtype = NULL, nrow = 5L, ncol = 5L) {
    if (type %in% names_weave) {
        v <- as.integer(!pattern_weave(type, subtype, nrow, ncol)) + 1L
        m <- matrix(v, nrow = nrow, ncol = ncol)
    } else if (type == "diagonal") {
        m <- pattern_diagonal(subtype, nrow, ncol)
    } else if (type == "diagonal_skew") {
        m <- pattern_diagonal(subtype, nrow, ncol, skew = TRUE)
    } else {
        abort(paste("Don't recognize square pattern type", type))
    }
    class(m) <- c("pattern_square", class(m))
    m
}

#' @rdname pattern_square
#' @export
names_square <- c("diagonal", "diagonal_skew")

pattern_diagonal <- function(subtype = NULL, nrow = 5L, ncol = 5L, skew = FALSE) {
    if (is.null(subtype) || is.na(subtype)) subtype <- 3L
    stopifnot(is_integer(subtype))
    m <- matrix(1L, nrow = nrow, ncol = ncol)
    n <- as.integer(subtype)
    s <- seq.int(n)
    for (e in s) {
        step <- ifelse(skew, -(e - 1L), e - 1L)
        v <- rep(cycle_elements(s, step), length.out = ncol)
        for (i in seq(e, nrow, n)) {
            m[i, ] <- v
        }
    }
    m
}

#' @export
print.pattern_square <- function(x, ...) {
    d <- dim(x)
    x <- matrix(int_to_char(x), nrow = d[1], ncol = d[2])
    cat("/", rep("-", ncol(x)), "\\", "\n")
    for (i in rev(seq_len(nrow(x)))) {
        cat("|", x[i, ], "|", "\n")
    }
    cat("\\", rep("-", ncol(x)), "/", "\n")
    invisible(NULL)
}

is_pattern_square <- function(type) {
    (type %in% names_weave) || (type %in% names_square)
}

int_to_char <- function(x) {
    stopifnot(max(x) < 36L)
    char <- as.character(x)
    indices <- which(x > 9L)
    char[indices] <- LETTERS[x[indices] - 9L]
    char
}
