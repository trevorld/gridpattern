# Splitting a grob into its shapes (and merging them back) for `area_boundaries()` and `with_pattern()`.
# `shape_spec()` is the one place that knows how each builtin grob class is split.

# How a grob is split into its shapes (or `NULL` if it can't be):
# * `"id"`: one shape per (sorted) `id` taking the `slots` of its points
# * `"path"`: one shape per (sorted) `pathId` keeping its rings (and so its holes)
# * `"element"`: one shape per element of its (recycled) `slots`
# * `"single"`: the grob is a single shape
shape_spec <- function(grob) {
	if (inherits(grob, "polygon")) {
		list(by = "id", slots = c("x", "y"))
	} else if (inherits(grob, "xspline")) {
		list(by = "id", slots = c("x", "y", "shape"))
	} else if (inherits(grob, "pathgrob")) {
		list(by = "path", slots = c("x", "y"))
	} else if (inherits(grob, "rect")) {
		# (a vector `hjust` / `vjust` justifies each rectangle)
		list(by = "element", slots = c("x", "y", "width", "height", "hjust", "vjust"))
	} else if (inherits(grob, "circle")) {
		list(by = "element", slots = c("x", "y", "r"))
	} else if (inherits(grob, "points")) {
		list(by = "element", slots = c("x", "y", "pch", "size"))
	} else if (inherits(grob, c("roundrect", FILLED_PATHS))) {
		# (a `fillStrokeGrob()` or `fillGrob()` fills a single path: the union of its `path` grob's shapes)
		list(by = "single")
	} else {
		NULL
	}
}

# A grob split into its shapes in drawing order: a list of their `grobs`
# (each with its own elements of the grob's graphical parameters) and whether each is an `area`
# painted with `fill` (or `NULL` if the grob can't be split, see `shape_spec()`).
grob_shapes <- function(grob) {
	spec <- shape_spec(grob)
	if (is.null(spec)) {
		return(NULL)
	}
	if (spec$by == "single") {
		return(list(grobs = list(grob), area = TRUE))
	}
	if (spec$by == "element") {
		slots <- Filter(function(slot) !is.null(grob[[slot]]), spec$slots)
		# (points are drawn at their locations while e.g. rects recycle all their slots)
		n <- if (inherits(grob, "points")) length(grob$x) else max(lengths(grob[slots]))
		recycled <- lapply(grob[slots], function(v) rep(v, length.out = n))
		values <- lapply(seq_len(n), function(i) lapply(recycled, function(v) v[i]))
	} else {
		n <- length(grob$x)
		ring <- grob_ids(grob$id, grob$id.lengths, n)
		key <- if (spec$by == "id") ring else grob_ids(grob$pathId, grob$pathId.lengths, n)
		recycled <- lapply(grob[spec$slots], function(v) rep(v, length.out = n))
		values <- lapply(unname(split(seq_len(n), key)), function(i) {
			v <- lapply(recycled, function(s) s[i])
			if (spec$by == "id") {
				c(v, list(id = NULL, id.lengths = NULL))
			} else {
				c(v, list(id = ring[i], id.lengths = NULL, pathId = NULL, pathId.lengths = NULL))
			}
		})
	}
	m <- length(values)
	gps <- shape_gps(grob$gp, m)
	grobs <- lapply(seq_len(m), function(i) {
		# (assigned directly: the values come from the (valid) grob so needn't be validated again)
		shape <- grob
		shape[names(values[[i]])] <- values[[i]]
		shape$gp <- gps[[i]]
		shape
	})
	area <- if (inherits(grob, "points")) {
		is_filled_pch(grob$pch, m)
	} else {
		rep_len(is_filled(grob), m)
	}
	list(grobs = grobs, area = area)
}

# the shapes (see `grob_shapes()`) of the same grob merged back into one grob
# (whose region is the union of theirs)
merge_shapes <- function(shapes, call = caller_env()) {
	if (length(shapes) == 1L) {
		return(shapes[[1L]])
	}
	first <- shapes[[1L]]
	spec <- shape_spec(first)
	if (is.null(spec) || spec$by == "single") {
		abort(glue("The areas of a `{class(first)[1L]}` grob can't be grouped."), call = call)
	}
	concat <- function(slot) {
		v <- lapply(shapes, `[[`, slot)
		if (is.unit(v[[1L]])) do.call(unit.c, v) else unlist(v)
	}
	slots <- Filter(function(slot) !is.null(first[[slot]]), spec$slots)
	values <- sapply(slots, concat, simplify = FALSE)
	n <- vapply(shapes, function(s) length(s$x), integer(1L))
	if (spec$by == "id") {
		values <- c(values, list(id = rep.int(seq_along(shapes), n), id.lengths = NULL))
	} else if (spec$by == "path") {
		# each shape stays a path (with its own rings and so holes)
		offset <- 0L
		id <- vector("list", length(shapes))
		for (i in seq_along(shapes)) {
			ring <- grob_ids(shapes[[i]]$id, shapes[[i]]$id.lengths, n[i])
			id[[i]] <- match(ring, unique(ring)) + offset
			offset <- max(id[[i]])
		}
		values <- c(
			values,
			list(
				id = unlist(id),
				id.lengths = NULL,
				pathId = rep.int(seq_along(shapes), n),
				pathId.lengths = NULL
			)
		)
	}
	do.call(editGrob, c(list(first, gp = merge_gp(lapply(shapes, `[[`, "gp"))), values))
}

# the graphical parameters of each of the `n` shapes of a grob (whose `gp` elements are recycled)
shape_gps <- function(gp, n) {
	gp <- gp %||% gpar()
	# (each multi-valued element is recycled once)
	recycled <- list()
	for (nm in names(gp)) {
		v <- gp[[nm]]
		if (inherits(v, "GridPatternList")) {
			recycled[[nm]] <- rep_len(unclass(v), n)
		} else if (length(v) > 1L && !is_pattern(v)) {
			recycled[[nm]] <- as.list(rep_len(v, n))
		}
	}
	lapply(seq_len(n), function(i) {
		for (nm in names(recycled)) {
			gp[[nm]] <- recycled[[nm]][[i]]
		}
		gp
	})
}

# the graphical parameters of merged shapes: each of theirs in turn
# (but a grid pattern, which can't be combined, is the first shape's)
merge_gp <- function(gps) {
	gp <- gps[[1L]] %||% gpar()
	for (nm in names(gp)) {
		v <- lapply(gps, `[[`, nm)
		if (all(vapply(v, function(x) is.atomic(x) && length(x) == 1L, logical(1L)))) {
			gp[[nm]] <- unlist(v)
		}
	}
	gp
}

# whether `grob` has an `area_boundaries()` method of its own (e.g. from another package)
# rather than one of the builtin methods (the default one based on `grob_shapes()` or the
# `gTree` / `gList` ones walking their children)
has_area_boundaries_method <- function(grob) {
	any(vapply(
		setdiff(class(grob), c("grob", "gDesc", "gTree", "gList")),
		function(cl) !is.null(utils::getS3method("area_boundaries", cl, optional = TRUE)),
		logical(1L)
	))
}

# Applies `f(leaf, vps, gps)` to each leaf within the gTree or gList `grob` in drawing order
# (`vps` / `gps` being the viewports and graphical parameters of the gTrees it is within)
# and (if `rebuild`) returns `grob` with each leaf replaced by its value.
# A child gTree is walked unless (a subclass) with its own `area_boundaries()` method (then a leaf)
# while any other child is only a leaf if it is of the `leaves` classes.
# This one walk defines the order of the areas of `area_boundaries()` and `with_pattern()`.
walk_leaves <- function(grob, leaves, f, rebuild = TRUE, vps = list(), gps = list()) {
	walk_child <- function(child) {
		if (inherits(child, "gTree") && !has_area_boundaries_method(child)) {
			walk_leaves(child, leaves, f, rebuild, vps, gps)
		} else if (inherits(child, "gTree") || inherits(child, leaves)) {
			f(child, vps, gps)
		} else {
			child
		}
	}
	if (inherits(grob, "gList")) {
		grobs <- lapply(unclass(grob), walk_child)
		return(if (rebuild) as_gList(grobs) else grob)
	}
	vps <- c(vps, list(grob$vp))
	gps <- c(gps, list(grob$gp))
	children <- lapply(grob$children[grob$childrenOrder], walk_child)
	if (rebuild && length(children) > 0L) {
		# (`setChildren()` keeps `childrenOrder` valid: the children keep their names)
		grob <- setChildren(grob, as_gList(children))
	}
	grob
}

# A leaf grob's `shapes` (see `grob_shapes()`, `NULL` if it has its own `area_boundaries()` method)
# and the `boundaries` of its areas (`leaves` and `...` are passed to its own method).
leaf_split <- function(grob, leaves = NULL, ...) {
	if (has_area_boundaries_method(grob)) {
		boundaries <- if (is.null(leaves)) {
			area_boundaries(grob, ...)
		} else {
			area_boundaries(grob, ..., leaves = leaves)
		}
		return(list(shapes = NULL, boundaries = boundaries))
	}
	shapes <- grob_shapes(grob)
	boundaries <- if (is.null(shapes)) gList() else shapes$grobs[shapes$area]
	list(shapes = shapes, boundaries = boundaries)
}
