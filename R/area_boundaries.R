#' Split a grob into the boundaries of its areas
#'
#' `area_boundaries()` splits a grid grob into one grob per area it paints with its `fill`:
#' one per polygon of a `polygon`, rectangle of a `rect`, path (its rings grouped by `pathId`)
#' of a `pathgrob`, circle of a `circle`, (closed) spline of an `xspline`,
#' the (single) rounded rectangle of a `roundrect`,
#' filled symbol (`pch` 21 to 25) of `points` (in point order),
#' and the (single) path of a [grid::fillStrokeGrob()] or [grid::fillGrob()]
#' (the shapes of its `path` grob, e.g. the glyphs of text, filled according to its `rule`).
#' Grobs without such an area (e.g. lines or text) have none
#' nor does a grob which only builds its shapes when drawn
#' (with a [grid::makeContext()] or [grid::makeContent()] method) until it is forced with [grid::forceGrob()].
#'
#' The areas are in a fixed order
#' (which the per-area pattern parameters and `group` of [with_pattern()] follow):
#' the polygons of a `polygon` or `xspline` by increasing `id`
#' and the paths of a `pathgrob` by increasing `pathId`
#' (grid stores these as integers; those given by `id.lengths` / `pathId.lengths` are
#' `1, 2, ...` in drawing order so sorting keeps that order
#' while an `id` such as `c(2, 2, 2, 1, 1, 1)` puts its second polygon first),
#' the rectangles of a `rect`, circles of a `circle`, and filled symbols of `points` in element order,
#' and all the areas of a child of a `gTree` (or `gList`) before those of its next child.
#'
#' The areas of a `gTree` (or `gList`) are those of its children in drawing order:
#' a child `gTree` is walked recursively
#' while any other child only has areas if it inherits from one of the `leaves` classes.
#' By default `leaves` leaves out `points`
#' so e.g. the (filled) outliers of a `{ggplot2}` boxplot have no areas
#' unless `"points"` is added to `leaves`.
#' A grob given as `grob` itself always has its areas
#' (`leaves` only selects the children of a `gTree` or `gList`).
#' Another package may add an `area_boundaries()` method for its own grob class
#' (and add the class to `leaves`)
#' or for its own `gTree` subclass (whose areas are then those of its method, which is passed `leaves`,
#' instead of its children's and which [with_pattern()] patterns as a whole like `layers = "grob"`).
#' Each boundary is a grob [as.patternGrob()] can fill with a pattern
#' and [with_pattern()] fills each area of a grob this way.
#'
#' Each boundary is drawn like its shape of `grob`:
#' it keeps its own elements of the graphical parameters (`gp`) of `grob`
#' (e.g. the second boundary of a `rect` with `gp = gpar(fill = c("red", "blue"))` is filled blue)
#' combined with those of the `gTree`s it is within
#' (e.g. a `fontsize` its "lines" units depend on, while `cex`, `alpha`, and `lex` multiply)
#' and the viewport (`vp`) its coordinates are relative to
#' (the `vp` of a `gTree` is pushed before those of its children's boundaries by [grid::vpStack()])
#' so a boundary drawn by itself is drawn where its area is.
#' The `childrenvp` of a `gTree` (viewports a child's `vp` may refer to by a [grid::vpPath()])
#' isn't handled.
#' The rings of a path (grouped by `pathId`) stay together so an area may have holes
#' while each polygon of a `polygon` is an area even where it overlaps another.
#'
#' @param grob A grid grob or a [grid::gList()] of grobs (whose areas are those of its grobs in order).
#' @param ... Passed to methods (the builtin methods don't use them).
#' @param leaves The classes of the (non-`gTree`) children of a `gTree` or `gList` which may have areas.
#' @return A [grid::gList()] of grobs (one per area, empty if `grob` has none).
#' @examples
#' if (require("grid", quietly = TRUE)) {
#'   polygon <- polygonGrob(x = c(0.1, 0.4, 0.4, 0.1, 0.6, 0.9, 0.75),
#'                          y = c(0.1, 0.1, 0.4, 0.4, 0.6, 0.6, 0.9),
#'                          id.lengths = c(4, 3))
#'   boundaries <- area_boundaries(polygon)
#'   length(boundaries) # 2
#'   grid.newpage()
#'   grid.draw(as.patternGrob(boundaries[[1]], "stripe", fill = "red"))
#'   grid.draw(as.patternGrob(boundaries[[2]], "circle", fill = "blue"))
#'
#'   # a gTree's areas are its children's (text has none)
#'   tree <- gTree(children = gList(rectGrob(x = c(0.25, 0.75), width = 0.4, height = 0.4),
#'                                  textGrob("label")))
#'   length(area_boundaries(tree)) # 2
#' }
#' @seealso [with_pattern()] which fills each area of a grob with a pattern
#'          and [as.patternGrob()] which fills a single boundary grob.
#' @export
area_boundaries <- function(grob, ...) {
	UseMethod("area_boundaries")
}

#' @rdname area_boundaries
#' @export
area_boundaries.gTree <- function(
	grob,
	...,
	leaves = c(
		"polygon",
		"pathgrob",
		"rect",
		"circle",
		"roundrect",
		"xspline",
		"GridFillStroke",
		"GridFill"
	)
) {
	leaves_boundaries(grob, leaves, ...)
}

# (a `gList`'s areas are those of its grobs in order)
#' @rdname area_boundaries
#' @export
area_boundaries.gList <- function(
	grob,
	...,
	leaves = c(
		"polygon",
		"pathgrob",
		"rect",
		"circle",
		"roundrect",
		"xspline",
		"GridFillStroke",
		"GridFill"
	)
) {
	leaves_boundaries(grob, leaves, ...)
}

# The boundaries of the leaves of a gTree or gList (see `walk_leaves()`):
# each boundary's viewport is pushed within those of the gTrees it is within
# and its graphical parameters are combined with theirs (see `stack_gp()`)
leaves_boundaries <- function(grob, leaves, ...) {
	boundaries <- list()
	walk_leaves(
		grob,
		leaves,
		function(leaf, vps, gps) {
			b <- lapply(leaf_split(leaf, leaves, ...)$boundaries, function(b) {
				b$vp <- Reduce(stack_vp, c(vps, list(b$vp)), right = TRUE)
				b$gp <- Reduce(stack_gp, c(gps, list(b$gp)), right = TRUE)
				b
			})
			boundaries[[length(boundaries) + 1L]] <<- b
			leaf
		},
		rebuild = FALSE
	)
	as_gList(unlist(boundaries, recursive = FALSE))
}

# the areas of a (builtin) grob are its shapes painted with `fill` (see `grob_shapes()`):
# lines, text, rasters, ... have none
#' @export
area_boundaries.default <- function(grob, ...) {
	if (!is.grob(grob)) {
		abort("`grob` must be a grid grob.")
	}
	shapes <- grob_shapes(grob)
	if (is.null(shapes)) {
		return(gList())
	}
	as_gList(shapes$grobs[shapes$area])
}

# the graphical parameters of a grob with `child` ones drawn within a gTree with `parent` ones:
# a `child` one replaces the `parent` one except `cex`, `alpha`, and `lex` which multiply it
stack_gp <- function(parent, child) {
	if (is.null(parent) || length(parent) == 0L) {
		return(child)
	}
	gp <- parent
	for (nm in names(child)) {
		gp[[nm]] <- if (nm %in% c("cex", "alpha", "lex") && !is.null(parent[[nm]])) {
			parent[[nm]] * child[[nm]]
		} else {
			child[[nm]]
		}
	}
	gp
}

as_gList <- function(grobs) {
	do.call(gList, unname(as.list(grobs)))
}
