# Helpers for pattern grobs whose boundary is a grob (see `as.patternGrob()`).
# A polygon boundary is turned into `x` / `y` / `id` when the pattern grob is created
# while other boundaries are stored and measured when drawn (in `makeContent.pattern()`)
# since their shapes (e.g. a rect's corners or a path's holes) depend on the viewport.

# combines the `vp` of a pattern grob with the `vp` of its boundary grob
# (the boundary's viewport is pushed within `vp` as if the boundary were drawn within a gTree with `vp`)
stack_vp <- function(vp, grob_vp) {
	if (is.null(vp)) {
		grob_vp
	} else if (is.null(grob_vp)) {
		vp
	} else {
		vpStack(vp, grob_vp)
	}
}

# the id of each of `n` locations given a grob's `id` or `id.lengths` (neither means one shape)
grob_ids <- function(id, id.lengths, n) {
	if (!is.null(id)) {
		rep_len(id, n)
	} else if (!is.null(id.lengths)) {
		rep.int(seq_along(id.lengths), id.lengths)
	} else {
		rep_len(1L, n)
	}
}

# whether a grob has its own `boundary_coords()` method (so it needn't `grobCoords()` i.e. R 4.2)
has_boundary_coords <- function(grob) {
	any(vapply(
		class(grob),
		function(cl) !is.null(utils::getS3method("boundary_coords", cl, optional = TRUE)),
		logical(1L)
	))
}

# The boundary polygons (in "npc" units of the current viewport) of a stored boundary grob:
# a list of `x`, `y`, `id` (empty if the boundary has no area) and `masker`.
# `masker` is `NULL` if the (union of the) polygons is the region else a grob for `alphaMaskGrob()`
# that cuts out e.g. the holes of a path (the polygons are then the region's exterior rings).
boundary_coords <- function(grob) {
	UseMethod("boundary_coords")
}

#' @export
boundary_coords.rect <- function(grob) {
	n <- max(length(grob$x), length(grob$y), length(grob$width), length(grob$height))
	hjust <- resolveHJust(grob$just, grob$hjust)
	vjust <- resolveVJust(grob$just, grob$vjust)
	# `x` / `y` are locations but `width` / `height` are dimensions (they differ for "native" units)
	x <- rep_len(convertX(grob$x, "npc", valueOnly = TRUE), n)
	y <- rep_len(convertY(grob$y, "npc", valueOnly = TRUE), n)
	w <- rep_len(convertWidth(grob$width, "npc", valueOnly = TRUE), n)
	h <- rep_len(convertHeight(grob$height, "npc", valueOnly = TRUE), n)
	x0 <- x - hjust * w
	y0 <- y - vjust * h
	list(
		x = as.vector(rbind(x0, x0 + w, x0 + w, x0)),
		y = as.vector(rbind(y0, y0, y0 + h, y0 + h)),
		id = rep(seq_len(n), each = 4L),
		masker = NULL
	)
}

#' @export
boundary_coords.pathgrob <- function(grob) {
	n <- length(grob$x)
	x <- convertX(grob$x, "npc", valueOnly = TRUE)
	y <- convertY(grob$y, "npc", valueOnly = TRUE)
	ring <- grob_ids(grob$id, grob$id.lengths, n)
	path <- grob_ids(grob$pathId, grob$pathId.lengths, n)
	paths <- lapply(split(seq_len(n), path), function(i) {
		path_rings(x[i], y[i], ring[i], grob$rule)
	})
	rings <- unlist(lapply(paths, function(p) p$rings), recursive = FALSE)
	holes <- any(vapply(paths, function(p) p$holes, logical(1L)))
	c(rings_polygons(rings), list(masker = if (holes) masker_grob(grob)))
}

# Any other grob (R 4.2+) is measured by `grobCoords()`:
# a circle, roundrect, or (closed) xspline is approximated by its polygon
# while a gTree or any other grob (e.g. text) is the union of its shapes' coordinates masked by itself.
#' @export
boundary_coords.default <- function(grob) {
	grob <- measured_boundary(grob)
	rings <- coords_rings(grobCoords(grob, closed = TRUE))
	# `grobCoords()` are inches from the bottom left of the current viewport
	vpm <- get_vp_measurements("in")
	rings <- lapply(rings, function(r) list(x = r$x / vpm$width, y = r$y / vpm$height))
	c(rings_polygons(rings), list(masker = if (needs_mask(grob)) masker_grob(grob)))
}

# the boundary grob as measured when drawn: forced (so the children of a `makeContent()` grob are seen)
# and without any `points` (which have no area and whose `grobCoords()` are their symbols)
measured_boundary <- function(grob) {
	drop_points(forceGrob(grob))
}

# whether the polygons of `grob` (see `coords_rings()`) don't describe its region exactly
# (e.g. the glyphs of a text grob or the holes of a nested path) so the pattern must also be masked by it.
# Area-less primitives (lines, points, ...) have no polygons so they never need one.
needs_mask <- function(grob) {
	if (inherits(grob, "gTree")) {
		any(vapply(grob$children, needs_mask, logical(1L)))
	} else {
		exact <- c("circle", "roundrect", "polygon", "rect", "xspline")
		arealess <- c(
			"lines",
			"polyline",
			"segments",
			"move.to",
			"line.to",
			"points",
			"rastergrob",
			"null"
		)
		!inherits(grob, c(exact, arealess))
	}
}

# the rings (each a list of `x` / `y`) whose union fills one path's rings according to `rule`
# and whether the path has `holes` (that those rings, the region's exterior rings, fill in)
path_rings <- function(x, y, ring, rule) {
	rings <- Map(function(x, y) list(x = x, y = y), split(x, ring), split(y, ring))
	rings <- unname(Filter(function(r) length(r$x) >= 3L, rings))
	if (length(rings) == 0L || anyNA(x) || anyNA(y)) {
		return(list(rings = rings, holes = FALSE))
	}
	polygons <- sf::st_sfc(lapply(rings, ring_polygon))
	if (!isTRUE(all(sf::st_is_valid(polygons)))) {
		# How a self-intersecting ring (e.g. a pentagram) is filled isn't the overlay's below to tell:
		# its lobes may wind in different directions, "evenodd" empties its doubly covered parts,
		# and a lone ring (without a `pathId`) is drawn as a polygon with the device's own fill rule.
		# So let the mask (i.e. grid's own fill) sort out the region which the rings' union covers.
		return(list(rings = rings, holes = TRUE))
	}
	if (length(rings) == 1L) {
		return(list(rings = rings, holes = FALSE))
	}
	tryCatch(
		region_rings(polygons, rings, rule),
		error = function(e) list(rings = rings, holes = TRUE)
	)
}

# the `{sf}` POLYGON of a (simple) ring
ring_polygon <- function(r) {
	m <- cbind(r$x, r$y)
	sf::st_polygon(list(rbind(m, m[1L, ])))
}

# The exterior rings of the region filled by the (simple) `rings` (and their `polygons`)
# under grid's fill `rule` and whether that region has holes.
# `st_intersection()` of a single `sfc` returns every planar piece of the rings with the `idx`
# of the rings covering it so a piece is filled if its winding number
# (the sum of the orientations of those rings) is nonzero ("winding") or if that count is odd ("evenodd").
region_rings <- function(polygons, rings, rule) {
	pieces <- sf::st_intersection(polygons)
	idx <- attr(pieces, "idx")
	if (all(lengths(idx) == 1L)) {
		# disjoint rings are each filled whatever the rule
		return(list(rings = rings, holes = FALSE))
	}
	keep <- if (rule == "evenodd") {
		lengths(idx) %% 2L == 1L
	} else {
		sgn <- vapply(rings, function(r) sign(signed_area(r$x, r$y)), numeric(1L))
		vapply(idx, function(i) sum(sgn[i]) != 0, logical(1L))
	}
	keep <- keep & (sf::st_dimension(pieces) %in% 2L)
	# (a GEOMETRYCOLLECTION can't be cast so the caller then falls back to the mask)
	region <- sf::st_cast(sf::st_union(pieces[keep]), "POLYGON")
	exteriors <- lapply(region, function(p) {
		m <- p[[1L]]
		m <- m[-nrow(m), , drop = FALSE] # `polygon_df` rings aren't closed
		list(x = m[, 1L], y = m[, 2L])
	})
	list(rings = exteriors, holes = any(lengths(region) > 1L))
}

# twice the signed area of a ring (positive if counter-clockwise)
signed_area <- function(x, y) {
	sum(x * c(y[-1L], y[1L]) - c(x[-1L], x[1L]) * y)
}

# the `x`, `y`, `id` of a list of rings (each a list of `x` / `y`)
rings_polygons <- function(rings) {
	list(
		x = unlist(lapply(rings, function(r) r$x), use.names = FALSE),
		y = unlist(lapply(rings, function(r) r$y), use.names = FALSE),
		id = rep.int(seq_along(rings), vapply(rings, function(r) length(r$x), integer(1L)))
	)
}

# the rings (each a list of `x` / `y` in inches) of a `grobCoords()` result
coords_rings <- function(coords) {
	if (inherits(coords, "GridGTreeCoords")) {
		unlist(lapply(coords, coords_rings), recursive = FALSE)
	} else {
		Filter(function(r) length(r$x) >= 3L, unclass(coords))
	}
}

# `grob` without any `points` (which have no area and whose `grobCoords()` are their symbols)
drop_points <- function(grob) {
	if (inherits(grob, "points")) {
		nullGrob()
	} else if (inherits(grob, "gTree") && has_points(grob)) {
		children <- Filter(function(child) !inherits(child, "points"), grob$children)
		children <- lapply(children, drop_points)
		setChildren(grob, do.call(gList, unname(children)))
	} else {
		grob
	}
}

# `grob` with its (and its children's) graphical parameters replaced so it draws its region opaquely
# since only the alpha channel of a mask matters (so e.g. an `NA` fill or a stroke shouldn't change it)
masker_grob <- function(grob) {
	gp <- grob$gp %||% gpar()
	gp$alpha <- 1
	if (inherits(grob, "gTree")) {
		grob$children[] <- lapply(grob$children, masker_grob)
	} else if (
		inherits(grob, c("text", "lines", "polyline", "segments")) ||
			(inherits(grob, "xspline") && isTRUE(grob$open))
	) {
		# drawn with `col` (text may also use `fill` for some fonts)
		gp$col <- "black"
		gp$fill <- "black"
	} else {
		gp$fill <- "black"
		gp$col <- NA
		gp$lwd <- 0
	}
	grob$gp <- gp
	grob
}
