#' Create patterned grobs from grob boundaries
#'
#' `as.patternGrob()` creates a pattern grob (like [patternGrob()]) whose boundary is
#' the area filled by an existing grid grob instead of numeric `x`, `y`, `id` coordinates.
#'
#' The boundary grob is measured when the pattern grob is drawn (in the viewport it is drawn in).
#' How depends on the class of `boundary`:
#' \describe{
#'   \item{`polygon`}{Its `x`, `y`, and `id` (or `id.lengths`) are used directly
#'     so the result is the same as a [patternGrob()] call: its polygons are filled as one region.}
#'   \item{`pathgrob`}{Its rings (grouped by `pathId`) are filled according to its `rule`.
#'     Where a path has holes the pattern is also masked by the path with [alphaMaskGrob()]
#'     (whose `use_R4.1_masks`, `png_device`, and `res` are the pattern parameters of the same names
#'     e.g. using the R 4.1 mask feature if the graphics device supports it
#'     else a `rasterGrob` approximation at the resolution `pattern_res`).}
#'   \item{`rect`}{Each of its rectangles is a boundary polygon.}
#'   \item{`pattern`}{An existing pattern grob is returned unchanged
#'     (use [grid::editGrob()] to change its pattern or parameters).}
#'   \item{Any other grob}{Its boundary is computed by [grid::grobCoords()] (which requires R 4.2):
#'     a `circle`, `roundrect`, (closed) `xspline`, or the filled symbols of `points`
#'     (`pch` 21 to 25, whose interior is painted with `fill`) are approximated by polygons
#'     while a `gTree` or any other grob (e.g. `text`) is the union of its shapes' coordinates
#'     masked by the grob itself (so e.g. the glyphs of a text grob get the pattern).
#'     A grob without a filled area (e.g. lines, a `rastergrob`, or `points` of other symbols) draws nothing.}
#' }
#' A pattern grob with a boundary grob stores it as `boundary` (instead of `x`, `y`, and `id`)
#' so [grid::editGrob()] that element to change its boundary (a `polygon` edited in is also measured when drawn).
#' The `vp` of `boundary` (if any) is pushed within `vp` when the pattern grob is drawn
#' (as if `boundary` were drawn within a gTree with `vp`) while a `polygon` boundary's `vp`
#' is instead combined with `vp` by [grid::vpStack()] (as its coordinates are reused).
#' @param boundary A grid grob whose (filled) area is the boundary of the pattern.
#' @param ... Passed to [patternGrob()]: the `pattern` name, its pattern parameters,
#'            and its `legend`, `prefix`, `name`, `gp`, and `vp` arguments
#'            (but not `x`, `y`, nor `id` since the boundary is `boundary`).
#' @return A `pattern` grid grob object (as returned by [patternGrob()]).
#' @examples
#' run_examples <- require("grid", quietly = TRUE) &&
#'   getRversion() >= "4.2.0"
#' if (run_examples) {
#'   # a circle is approximated by a polygon
#'   grid.newpage()
#'   circle <- circleGrob(r = 0.4, gp = gpar(fill = NA, lwd = 4))
#'   grid.draw(as.patternGrob(circle, "weave", fill = "yellow", fill2 = "blue"))
#'   grid.draw(circle)
#'   # as are the filled symbols (pch 21 to 25) of points
#' }
#' if (run_examples) {
#'   grid.newpage()
#'   points <- pointsGrob(seq(0.1, 0.9, by = 0.2), rep(0.5, 5),
#'                        pch = 21:25, size = unit(0.18, "npc"),
#'                        default.units = "npc", gp = gpar(fill = NA, lwd = 4))
#'   grid.draw(as.patternGrob(points, "stripe", fill = c("red", "blue"), density = 0.5))
#'   grid.draw(points)
#' }
#' # the holes of a path are cut out by a mask
#' if (run_examples && "alpha" %in% grDevices::dev.capabilities()$masks) {
#'   angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6)
#'   x_hex <- 0.5 + 0.5 * cos(angle)
#'   y_hex <- 0.5 + 0.5 * sin(angle)
#'   path <- pathGrob(x = c(x_hex, 0.5 + 0.25 * cos(angle)),
#'                    y = c(y_hex, 0.5 + 0.25 * sin(angle)),
#'                    id = rep(1:2, each = 6), rule = "evenodd",
#'                    gp = gpar(fill = NA, lwd = 4))
#'   grid.newpage()
#'   grid.draw(as.patternGrob(path, "stripe", fill = c("red", "blue"), density = 0.5))
#'   grid.draw(path)
#' }
#' @seealso [patternGrob()] for a pattern grob with a numeric boundary
#'          and [alphaMaskGrob()] which masks any grob with another.
#' @export
as.patternGrob <- function(boundary, ...) {
	UseMethod("as.patternGrob")
}

#' @export
as.patternGrob.polygon <- function(boundary, ..., vp = NULL) {
	check_boundary_dots(...)
	# the coordinates (already `unit()`s) are the boundary: nothing to measure when drawn
	id <- grob_ids(boundary$id, boundary$id.lengths, length(boundary$x))
	patternGrob(x = boundary$x, y = boundary$y, id = id, ..., vp = stack_vp(vp, boundary$vp))
}

#' @export
as.patternGrob.pattern <- function(boundary, ...) {
	# a coercion is idempotent: a pattern grob is already a pattern grob
	# (but don't silently ignore a request to change it)
	if (...length() > 0L) {
		abort(c(
			"`as.patternGrob()` returns a `pattern` grob unchanged.",
			i = "Use `grid::editGrob()` to change its pattern, parameters, or viewport."
		))
	}
	boundary
}

#' @export
as.patternGrob.default <- function(boundary, ...) {
	if (!is.grob(boundary)) {
		abort(c(
			"`boundary` must be a grid grob.",
			i = "Use `patternGrob()` for a boundary of numeric `x`, `y`, and `id` coordinates."
		))
	}
	check_boundary_dots(...)
	# The pattern grob stores `boundary` to measure it when drawn (see `boundary_coords()`
	# and `makeContext.pattern()` which pushes the boundary's `vp` within the pattern's)
	# and keeps (`NULL`) `x` / `y` / `id` elements so editing them is caught by `validDetails.pattern()`
	# (which validates the `boundary` too e.g. that R 4.1 can measure it).
	# NB. A polygon boundary is deliberately stored as `x` / `y` / `id` instead (see `as.patternGrob.polygon()`)
	# so it is the same grob `patternGrob()` makes and code reading `$x` / `$y` / `$id` keeps working.
	editGrob(patternGrob(...), boundary = boundary, x = NULL, y = NULL, id = NULL)
}

# `as.patternGrob()`'s `...` are `patternGrob()`'s other arguments: the boundary is `boundary`
check_boundary_dots <- function(...) {
	if (any(c("x", "y", "id") %in% ...names())) {
		abort(c(
			"`x`, `y`, and `id` can't be given with a `boundary` grob (whose area is the boundary).",
			i = "Use `patternGrob()` for a boundary of numeric `x`, `y`, and `id` coordinates."
		))
	}
}
