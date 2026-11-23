# c() and append() don't directly work with grid::gList() lists
append_gList <- function(gl, grob) {
	gl[[length(gl) + 1L]] <- grob
	gl
}

# whether `x` is a list of grid patterns: either classed "GridPatternList" by `gpar()`
# or else a plain list assigned directly to `gp$fill` (which grid still draws the same)
# (unlike a bare `is.list()` this excludes a pattern itself, a grob, and a `gpar()` list `col` / `lty`)
is_pattern_list <- function(x) {
	is.list(x) && length(x) > 0L && all(vapply(x, is_pattern, logical(1L)))
}

rep_len_fill <- function(x, length.out) {
	if (inherits(x, "GridPattern")) {
		rep_len(list(x), length.out)
	} else {
		rep_len(x, length.out)
	}
}

# get width, height, length, and center cooordinates of the viewport in `units` units
get_vp_measurements <- function(units = "bigpts") {
	width <- convertWidth(unit(1, "npc"), units, valueOnly = TRUE)
	height <- convertHeight(unit(1, "npc"), units, valueOnly = TRUE)
	length <- max(width, height)
	x <- convertX(unit(0.5, "npc"), units, valueOnly = TRUE)
	y <- convertY(unit(0.5, "npc"), units, valueOnly = TRUE)
	list(width = width, height = height, length = length, x = x, y = y)
}
