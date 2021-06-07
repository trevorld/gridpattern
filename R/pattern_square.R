#' Square pattern matrix
#'
#' `pattern_square()` returns an integer matrix indicating where each
#' color (or other graphical element) should be drawn on a rectangular grid
#' for a specified square pattern type and subtype.
#' `names_square` lists the currently supported square `type`s (excluding those in `names_weave`).
#'
#' \describe{
#' \item{"horizontal", "vertical"}{"horizontal" and "vertical" simply cycle through the colors
#'   either horizontally or vertically.
#'   Use `subtype` to indicate the (integer) number of colors (or other graphical elements).
#'   "horizontal" will produce horizontal stripes of color whereas "vertical" will produce vertical stripes.}
#' \item{"diagonal", "diagonal_skew"}{"diagonal" and "diagonal_skew" simply cycle through the colors
#'   both horizontally and vertically.
#'   Use `subtype` to indicate the (integer) number of colors (or other graphical elements).
#'   If two colors are requested this provides the standard two-color checkerboard pattern.
#'   If there are more than three colors than "diagonal" will have colored diagonals
#'   going from top left to bottom right while "diagonal_skew" will have them
#'   going form bottom left to top right.}
#' \item{any pattern from `names_weave`}{
#'   We simply convert the logical matrix returned by [pattern_weave()] into an
#'   integer matrix by having any `TRUE` set to `1L` and `FALSE` set to `2L`.
#'   Hence the various weave patterns only support (up to) two-color patterns.
#'   See [pattern_weave()] for more details about supported `type` and `subtype`.}
#' }
#'
#' @param type Either "diagonal" (default), "diagonal_skew", "horizontal", "vertical",
#'             or any `type` in `names_weave`.  See Details.
#' @param subtype See Details.  For "diagonal", "diagonal_skew", "horizontal", or "vertical"
#'                an integer of the desired number of colors (or other graphical elements).
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
#'  horizontal <- pattern_square("horizontal", 4L, nrow = 8L, ncol = 8L)
#'  print(horizontal)
#'
#'  vertical <- pattern_square("vertical", 4L, nrow = 8L, ncol = 8L)
#'  print(vertical)
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
    } else if (type == "horizontal") {
        m <- pattern_horizontal(subtype, nrow, ncol)
    } else if (type == "vertical") {
        m <- pattern_vertical(subtype, nrow, ncol)
    } else {
        abort(paste("Don't recognize square pattern type", type))
    }
    class(m) <- c("pattern_square", class(m))
    m
}

#' @rdname pattern_square
#' @export
names_square <- c("diagonal", "diagonal_skew", "horizontal", "vertical")

pattern_diagonal <- function(subtype = NULL, nrow = 5L, ncol = 5L, skew = FALSE) {
    if (is.null(subtype) || is.na(subtype)) subtype <- 3L
    stopifnot(is_integer(subtype))
    m <- matrix(1L, nrow = nrow, ncol = ncol)
    n <- as.integer(subtype)
    if (n == 1L) return(m)
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

pattern_horizontal <- function(subtype = NULL, nrow = 5L, ncol = 5L) {
    if (is.null(subtype) || is.na(subtype)) subtype <- 3L
    stopifnot(is_integer(subtype))
    n <- as.integer(subtype)
    s <- seq.int(n)
    v <- rep(s, length.out = ncol)
    v <- rep(v, length.out = nrow)
    matrix(v, nrow = nrow, ncol = ncol)
}

pattern_vertical <- function(subtype = NULL, nrow = 5L, ncol = 5L) {
    if (is.null(subtype) || is.na(subtype)) subtype <- 3L
    stopifnot(is_integer(subtype))
    n <- as.integer(subtype)
    s <- seq.int(n)
    v <- rep(s, length.out = nrow)
    v <- rep(v, length.out = ncol)
    matrix(v, nrow = nrow, ncol = ncol, byrow = TRUE)
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
