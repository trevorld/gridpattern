#' Fill the areas of an object with patterns
#'
#' `with_pattern()` returns a modified version of its input with each area it paints with a `fill` also filled with a pattern.
#'
#' `with_pattern()` returns a modified version of the object `x` with each area it paints with its `fill` (see [area_boundaries()]) also filled with a pattern.
#' This package provides its methods for grid grobs
#' (another package may add a method for its own class of object).
#' Each grob with such areas (by default a [grid::polygonGrob()], [grid::rectGrob()], [grid::pathGrob()],
#' [grid::circleGrob()], [grid::roundrectGrob()], (closed) [grid::xsplineGrob()],
#' [grid::fillStrokeGrob()], or [grid::fillGrob()], see the `leaves` argument)
#' is replaced in place by a `gTree` (keeping its `name` and `vp`)
#' drawing each area's fill, its pattern (made by [as.patternGrob()]), and its border.
#'
#' `layers = "area"` (the default) keeps overlapping shapes in their original order
#' while `layers = "grob"` uses fewer grobs and gives the same output
#' unless the areas overlap or have borders of differing colours or widths
#' (a grob with a single shape or whose `fill` is a [grid::pattern()] relative to all its shapes
#' is always layered this way).
#' A `gTree` (or `gList`) is walked recursively
#' while a grob without areas (e.g. lines or text) is returned unchanged.
#'
#' By default the filled symbols of a [grid::pointsGrob()] within a `gTree` or `gList` aren't patterned
#' (e.g. the outliers of a `{ggplot2}` boxplot) unless `"points"` is added to `leaves`
#' but a [grid::pointsGrob()] given as `x` itself is (one pattern per filled symbol).
#' Areas other than those of a [grid::polygonGrob()], [grid::pathGrob()], or [grid::rectGrob()]
#' are measured by [grid::grobCoords()]
#' so patterning them requires R (>= 4.2.0).
#'
#' A `group` of several areas of the same grob is filled by one pattern
#' whose boundary is their union (each path keeping its holes)
#' e.g. so a gradient or image spans the polygons of each row of a `{sf}` MULTIPOLYGON geometry
#' (which [sf::st_as_grob()] draws as one path per polygon).
#'
#' The pattern grobs don't inherit the `col`, `fill`, etc. of the ancestors of `x`
#' (e.g. of a legend key), only their `alpha`.
#' `with_pattern()` splits the shapes a grob has when it is called
#' so a grob which only builds them when drawn
#' (with a [grid::makeContext()] or [grid::makeContent()] method, e.g. the dot stack of a `{ggplot2}` dotplot)
#' has no areas unless `delay = TRUE`:
#' the result is then a `gTree` which forces the grob with [grid::forceGrob()] when it is drawn
#' (in the viewport it is drawn in) and fills the areas it has then
#' (so an error about their number is raised then too).
#' @param x An object with filled areas such as a grid grob or a [grid::gList()].
#' @param pattern Name of the pattern (see [patternGrob()]):
#'                either one (for every area) or one per area (or `group`) of `x`.
#' @param ... Pattern parameters (see [patternGrob()]) each either of length one (for every area)
#'            or with one value per area of `x` (in the order of `area_boundaries(x)`)
#'            or per `group` (in the order of `unique(group)`).
#'            A multi-valued parameter of a single area must be wrapped in a list
#'            (e.g. `fill = list(c("red", "blue"))` for alternating stripes)
#'            while a [grid::pattern()] fill counts as one value.
#'            [patternGrob()]'s `y`, `id`, `name`, `gp`, `vp`, and `boundary` can't be given:
#'            each area's pattern grob takes its boundary, name, graphical parameters,
#'            and viewport from its area.
#' @param group If not `NULL`, one value per area of `x` (in the order of `area_boundaries(x)`)
#'              grouping the areas filled by the same pattern:
#'              the areas of a group must be areas of the same grob.
#' @param layers Either `"area"` to draw the fill, pattern, and border of each area in turn
#'               or `"grob"` to draw the fills of all the areas of a grob, then their patterns,
#'               then their borders.
#' @param delay If `TRUE` the areas of `x` are filled when it is drawn
#'              (after forcing it with [grid::forceGrob()]) rather than now
#'              and the result is a `gTree` (of class `"delayed_pattern"`) drawing them:
#'              for a grob which only builds its shapes when drawn.
#' @param leaves The classes of the (non-`gTree`) grobs within a `gTree` or `gList`
#'               which may have areas (passed to [area_boundaries()]).
#' @param legend Passed to [patternGrob()] (e.g. the numeric zoom of a legend key).
#' @param prefix Passed to [patternGrob()]: the prefix prepended to the names of the pattern parameters
#'               in `...` (e.g. `prefix = ""` for parameters already named `pattern_*`).
#' @return For a grid grob a [grid::gTree()] (or a [grid::gList()] if `x` is one)
#'         unless `x` has no fillable areas in which case `x` is returned unchanged
#'         (with `delay = TRUE` always a `gTree`).
#' @examples
#' if (require("grid", quietly = TRUE)) {
#'   tree <- gTree(children = gList(
#'     rectGrob(x = c(0.25, 0.75), y = 0.3, width = 0.35, height = 0.4, name = "rects",
#'              gp = gpar(fill = c("lightblue", "lightyellow"), col = "black", lwd = 3)),
#'     polygonGrob(x = c(0.3, 0.7, 0.5), y = c(0.6, 0.6, 0.95), name = "triangle",
#'                 gp = gpar(fill = "pink", col = "red", lwd = 3)),
#'     textGrob("text is left alone", y = 0.05, name = "label")
#'   ), name = "tree")
#'   # one value per area (in that order) or one for all
#'   patterned <- with_pattern(tree, c("stripe", "stripe", "crosshatch"),
#'                             fill = c("blue", "orange", "red"), angle = c(45, -45, 0),
#'                             density = 0.4)
#'   grid.newpage()
#'   grid.draw(patterned)
#' }
#' @seealso [area_boundaries()] for the areas of a grob
#'          and [as.patternGrob()] for the pattern grob of a single boundary grob.
#' @export
with_pattern <- function(x, pattern = "stripe", ...) {
	UseMethod("with_pattern")
}

#' @rdname with_pattern
#' @export
with_pattern.gTree <- function(
	x,
	pattern = "stripe",
	...,
	group = NULL,
	layers = c("area", "grob"),
	delay = FALSE,
	leaves = c(
		"polygon",
		"pathgrob",
		"rect",
		"circle",
		"roundrect",
		"xspline",
		"GridFillStroke",
		"GridFill"
	),
	legend = FALSE,
	prefix = "pattern_"
) {
	if (check_delay(delay, rlang::current_env())) {
		return(delayed_pattern(
			x,
			pattern,
			list(...),
			list(group = group, layers = layers, leaves = leaves, legend = legend, prefix = prefix),
			rlang::current_env()
		))
	}
	walk <- if (inherits(x, "gTree") && has_area_boundaries_method(x)) {
		# (a gTree subclass with its own `area_boundaries()` method is itself the only leaf)
		function(f, rebuild) f(x, list(), list())
	} else {
		function(f, rebuild) walk_leaves(x, leaves, f, rebuild)
	}
	pattern_leaves(
		x,
		walk,
		leaves,
		pattern,
		list(...),
		group,
		rlang::arg_match(layers),
		legend,
		prefix,
		rlang::current_env()
	)
}

#' @rdname with_pattern
#' @export
with_pattern.gList <- with_pattern.gTree

#' @rdname with_pattern
#' @export
with_pattern.grob <- function(
	x,
	pattern = "stripe",
	...,
	group = NULL,
	layers = c("area", "grob"),
	delay = FALSE,
	legend = FALSE,
	prefix = "pattern_"
) {
	if (check_delay(delay, rlang::current_env())) {
		return(delayed_pattern(
			x,
			pattern,
			list(...),
			list(group = group, layers = layers, legend = legend, prefix = prefix),
			rlang::current_env()
		))
	}
	# (`x` itself is the only leaf whatever its class)
	pattern_leaves(
		x,
		function(f, rebuild) f(x, list(), list()),
		NULL,
		pattern,
		list(...),
		group,
		rlang::arg_match(layers),
		legend,
		prefix,
		rlang::current_env()
	)
}

#' @export
with_pattern.default <- function(x, ...) {
	abort(c(
		glue("No `with_pattern()` method for an object of class `{class(x)[1L]}`."),
		i = "This package provides methods for grid grobs and gLists."
	))
}

check_delay <- function(delay, call) {
	if (!rlang::is_bool(delay)) {
		abort("`delay` must be `TRUE` or `FALSE`.", call = call)
	}
	delay
}

# A gTree (keeping the name and attributes of `x`) filling the areas `x` has when it is drawn
# (`delay = TRUE`): its `makeContent()` method forces `x` (in the viewport it is drawn in,
# see `grid::forceGrob()`) and patterns the forced grob with the `pattern`,
# pattern parameters (`params`), and other arguments (`args`) of `with_pattern()`.
# The checks which don't depend on the number of areas are made now
# (errors while drawing leave the graphics device locked).
delayed_pattern <- function(x, pattern, params, args, call) {
	area_state(pattern, params, NULL, list(), call)
	args$layers <- rlang::arg_match0(
		args$layers,
		c("area", "grob"),
		arg_nm = "layers",
		error_call = call
	)
	legend_scale(args$legend)
	if (!is.null(args$group) && (!is.atomic(args$group) || anyNA(args$group))) {
		abort("`group` must be an atomic vector without missing values.", call = call)
	}
	wrapper <- gTree(
		grob = x,
		args = c(list(pattern = pattern), params, args),
		name = x$name,
		cl = "delayed_pattern"
	)
	keep_attributes(wrapper, x)
}

#' @export
makeContent.delayed_pattern <- function(x) {
	forced <- force_grob(x$grob)
	patterned <- rlang::try_fetch(
		# (`quote`: the stored arguments are values, not expressions to evaluate)
		do.call("with_pattern", c(list(forced), x$args), quote = TRUE),
		error = function(cnd) {
			abort(
				"The areas of `x` couldn't be filled when it was drawn (`delay = TRUE`).",
				parent = cnd,
				call = quote(with_pattern())
			)
		}
	)
	setChildren(x, gList(patterned))
}

# `x` (a grob or gList) forced in the current viewport (see `grid::forceGrob()`)
# without the `.ORIGINAL` copies of the unforced grobs `forceGrob()` keeps for `grid::grid.revert()`
# (the shapes, layers, and boundaries split from the forced grobs would each carry one)
force_grob <- function(x) {
	if (inherits(x, "gList")) {
		return(as_gList(lapply(unclass(x), force_grob)))
	}
	forced <- forceGrob(x)
	if (!inherits(x, "gTree") && inherits(forced, "gTree")) {
		# (`forceGrob()`'s grob method doesn't force the children of a gTree
		# which a `makeContent()` method returned: its gTree method does)
		forced <- forceGrob(forced)
		class(forced) <- unique(class(forced))
	}
	drop_originals(forced)
}

drop_originals <- function(grob) {
	grob$.ORIGINAL <- NULL
	if (inherits(grob, "gTree") && length(grob$children) > 0L) {
		# (`setChildren()` names the children again)
		grob <- setChildren(grob, as_gList(lapply(grob$children, drop_originals)))
	}
	grob
}

# `x` with each of its leaves with areas patterned: `walk(f, rebuild)` applies `f()` to each leaf
# (see `walk_leaves()`), first to split them (see `leaf_split()`) and then to replace them.
pattern_leaves <- function(x, walk, leaves, pattern, params, group, layers, legend, prefix, call) {
	splits <- list()
	walk(
		function(leaf, ...) {
			splits[[length(splits) + 1L]] <<- leaf_split(leaf, leaves)
			leaf
		},
		rebuild = FALSE
	)
	state <- area_state(pattern, params, group, splits, call)
	if (is.null(state)) {
		return(x)
	}
	state$layers <- layers
	state$legend <- legend
	state$prefix <- prefix
	walk(function(leaf, ...) pattern_leaf(leaf, state), rebuild = TRUE)
}

# The state (an environment) of the walk replacing the leaves (split into `splits`) of `x`:
# the `params` of each pattern
# (one list per group of areas holding its `pattern` name and the (`...`) pattern parameters),
# the (integer) `group` of each area numbered by its first area, the `splits` and the index `i`
# of the next one, the number of areas (`offset`) and groups (`k`) consumed so far,
# and the `call` errors are reported for (or `NULL` if there are no areas).
area_state <- function(pattern, params, group, splits, call) {
	n <- sum(vapply(splits, function(s) length(s$boundaries), integer(1L)))
	nms <- names(params)
	if (length(params) > 0L && (is.null(nms) || !all(nzchar(nms)))) {
		abort("The pattern parameters in `...` must be named.", call = call)
	}
	if (!is.character(pattern) || length(pattern) == 0L || anyNA(pattern)) {
		abort("`pattern` must be a character vector of pattern names.", call = call)
	}
	if ("leaves" %in% nms) {
		abort(
			"`leaves` can only be given to `with_pattern()` if `x` is a gTree or gList.",
			call = call
		)
	}
	taken <- intersect(nms, c("y", "id", "name", "gp", "vp", "boundary"))
	if (length(taken) > 0L) {
		abort(
			c(
				glue("`{taken[1L]}` can't be given to `with_pattern()`."),
				i = "The pattern grobs take their boundaries, names, and viewports from the areas of `x`."
			),
			call = call
		)
	}
	if (is.null(group)) {
		group <- seq_len(n)
		unit <- "area"
	} else {
		if (!is.atomic(group) || length(group) != n || anyNA(group)) {
			abort(
				glue(
					"`group` must have one (non-missing) value per area ({n}) but has length {length(group)}."
				),
				call = call
			)
		}
		group <- match(group, unique(group))
		unit <- "group"
	}
	if (n == 0L) {
		return(NULL)
	}
	m <- max(group)
	params <- c(list(pattern = pattern), params)
	params <- Map(
		function(p, nm) {
			if (is.null(p) || is_pattern(p)) {
				# (a `grid::pattern()` fill is one value)
				p <- list(p)
			} else if (!is.list(p)) {
				p <- as.list(p)
			}
			if (!(length(p) %in% c(1L, m))) {
				abort(
					glue(
						"`{nm}` must have length one or one value per {unit} ({m}) but has length {length(p)}."
					),
					call = call
				)
			}
			rep_len(p, m)
		},
		params,
		names(params)
	)
	params <- lapply(seq_len(m), function(i) lapply(params, `[[`, i))
	state <- new.env(parent = emptyenv())
	state$params <- params
	state$group <- group
	state$splits <- splits
	state$i <- 1L
	state$offset <- 0L
	state$k <- 0L
	state$call <- call
	state
}

# the patterned leaf `grob` (if it has areas) whose areas take the next groups of the `state`
pattern_leaf <- function(grob, state) {
	split <- state$splits[[state$i]]
	state$i <- state$i + 1L
	n <- length(split$boundaries)
	if (n == 0L) {
		return(grob)
	}
	boundaries <- split$boundaries
	group <- state$group[state$offset + seq_len(n)]
	state$offset <- state$offset + n
	groups <- unique(group)
	# (groups are numbered by their first area so an earlier grob's are at most `k`
	# and this one's are the next ones)
	if (any(groups <= state$k)) {
		abort("The areas of a `group` must all be areas of the same grob.", call = state$call)
	}
	state$k <- max(groups)
	if (length(groups) < n) {
		boundaries <- unclass(boundaries)
		boundaries <- lapply(groups, function(g) {
			merge_shapes(boundaries[group == g], call = state$call)
		})
	}
	patterns <- area_patterns(boundaries, state$params[groups], state$legend, state$prefix)
	if (state$layers == "area") {
		area_layered_grob(grob, split$shapes, patterns, match(group, groups))
	} else {
		grob_layered_grob(grob, patterns)
	}
}

# grid's top-level graphical parameters so a pattern's content doesn't inherit those of its ancestors
# (e.g. a {ggplot2} legend key puts its `gp` on the parent `gTree`)
# except `alpha` which can't be reset (a grob's alpha multiplies its ancestors').
# Only its content (see `makeContent.pattern()`): its boundary is measured with the inherited ones
# (e.g. a `fontsize` for "lines" units).
NEUTRAL_GP <- gpar(
	col = "black",
	fill = "transparent",
	lty = "solid",
	lwd = 1,
	lex = 1,
	cex = 1,
	fontsize = 12,
	fontface = "plain",
	fontfamily = "",
	lineheight = 1.2,
	lineend = "round",
	linejoin = "round",
	linemitre = 10
)

# `grob` painting only its fill
fill_only <- function(grob) {
	if (inherits(grob, FILLED_PATHS)) {
		return(fill_only_path(grob))
	}
	gp <- grob$gp %||% gpar()
	gp$col <- NA
	gp$lwd <- 0
	grob$gp <- gp
	grob
}

# `grob` painting only its border
border_only <- function(grob) {
	gp <- grob$gp %||% gpar()
	gp$fill <- NA
	grob$gp <- gp
	grob
}

# `grob` as a layer named `name` of its replacement (whose wrapper takes its viewport)
# (assigned directly: neither change needs `editGrob()`'s validation)
as_layer <- function(grob, name) {
	grob$vp <- NULL
	grob$name <- name
	grob
}

# The pattern grob of each of the (merged) `boundaries` of a grob's areas (taking their `params`):
# its content is drawn with grid's default graphical parameters (see `NEUTRAL_GP`)
# and the area's own `alpha` (as are its fill and border).
area_patterns <- function(boundaries, params, legend, prefix) {
	lapply(seq_along(boundaries), function(i) {
		# (the wrapper below takes the grob's viewport)
		boundary <- boundaries[[i]]
		boundary$vp <- NULL
		args <- c(
			list(boundary, legend = legend, prefix = prefix, name = paste0("area.", i)),
			params[[i]]
		)
		pattern <- do.call(as.patternGrob, args)
		content_gp <- NEUTRAL_GP
		alpha <- boundary$gp$alpha
		if (length(alpha) > 0L) {
			# (a merged group's first shape's)
			content_gp$alpha <- alpha[1L]
		}
		pattern$content_gp <- content_gp
		pattern
	})
}

# the layer of the pattern grobs
pattern_layer <- function(patterns) {
	gTree(children = as_gList(patterns), name = "pattern")
}

# a grob with areas replaced by its fill / `patterns` / border layers (each drawn for all its areas)
grob_layered_grob <- function(grob, patterns) {
	children <- gList(
		as_layer(fill_only(grob), "fill"),
		pattern_layer(patterns),
		as_layer(border_only(grob), "border")
	)
	layered_wrapper(grob, children)
}

# A grob with areas (split into its `shapes`, see `grob_shapes()`) replaced by
# the fill / pattern / border layers of each area (or group of areas)
# in turn (in a gTree named "area.i" for the area's `i`th pattern) and its other shapes (named
# "shape.j" for its `j`th shape) so overlapping shapes are drawn in their original order.
# A group's areas are drawn together where its first area is.
# A grob that can't be split into its shapes, has only one, or whose fill is a grid pattern relative
# to all its shapes (e.g. a gradient spanning them) is layered by `grob_layered_grob()` instead.
area_layered_grob <- function(grob, shapes, patterns, area_group) {
	if (
		is.null(shapes) ||
			has_grouped_pattern_fill(grob) ||
			length(shapes$grobs) < 2L
	) {
		return(grob_layered_grob(grob, patterns))
	}
	area_shape <- which(shapes$area)
	# the group of each shape (`NA` if it isn't an area) and the shapes of each group
	shape_group <- rep_len(NA_integer_, length(shapes$grobs))
	shape_group[area_shape] <- area_group
	group_shapes <- split(area_shape, area_group)
	layer <- function(f, members, name) {
		if (length(members) == 1L) {
			return(as_layer(f(shapes$grobs[[members]]), name))
		}
		# (the shapes of a group are named "fill.k" / "border.k": siblings need distinct names)
		grobs <- lapply(seq_along(members), function(k) {
			as_layer(f(shapes$grobs[[members[k]]]), paste0(name, ".", k))
		})
		gTree(children = as_gList(grobs), name = name)
	}
	children <- list()
	for (j in seq_along(shapes$grobs)) {
		g <- shape_group[j]
		if (is.na(g)) {
			children[[length(children) + 1L]] <- as_layer(shapes$grobs[[j]], paste0("shape.", j))
			next
		}
		members <- group_shapes[[g]]
		if (j != members[1L]) {
			next
		}
		children[[length(children) + 1L]] <- gTree(
			children = gList(
				layer(fill_only, members, "fill"),
				pattern_layer(patterns[g]),
				layer(border_only, members, "border")
			),
			name = paste0("area.", g)
		)
	}
	layered_wrapper(grob, as_gList(children))
}

# a gTree of the `children` replacing `grob` (keeping its name, viewport, and attributes)
layered_wrapper <- function(grob, children) {
	keep_attributes(gTree(children = children, name = grob$name, vp = grob$vp), grob)
}

# `wrapper` with the attributes of the `grob` it replaces
# (e.g. the `width` / `height` of a {ggplot2} legend key)
keep_attributes <- function(wrapper, grob) {
	for (a in setdiff(names(attributes(grob)), c("names", "class"))) {
		attr(wrapper, a) <- attr(grob, a)
	}
	wrapper
}

# whether the `fill` of `grob` is (or has) a grid pattern relative to all its shapes
# (`group = TRUE`, the only behaviour in R 4.1) so its shapes can't be filled separately
has_grouped_pattern_fill <- function(grob) {
	fill <- grob$gp$fill
	patterns <- if (is_pattern(fill)) {
		list(fill)
	} else if (inherits(fill, "GridPatternList")) {
		fill
	}
	any(vapply(patterns, function(p) !isFALSE(p$group), logical(1L)))
}
