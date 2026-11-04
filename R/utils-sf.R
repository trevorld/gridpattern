sf_multipolygon_to_polygon_grob <- function(
	multipolygons_sf,
	gp = gpar(),
	default.units = "npc",
	name = NULL
) {
	df <- convert_polygon_sf_to_polygon_df(multipolygons_sf)
	if (is.null(df)) {
		nullGrob()
	} else {
		polygonGrob(
			x = df$x,
			y = df$y,
			id = df$id,
			default.units = default.units,
			gp = gp,
			name = name
		)
	}
}

# build a circle of radius `r` centered on each point in `sf_points`
sf_points_to_circle_grob <- function(
	sf_points,
	r,
	gp = gpar(),
	default.units = "npc",
	name = NULL
) {
	points_mat <- as.matrix(sf_points)
	if (is.null(points_mat) || nrow(points_mat) == 0) {
		nullGrob()
	} else {
		circleGrob(
			x = points_mat[, 1],
			y = points_mat[, 2],
			r = r,
			default.units = default.units,
			gp = gp,
			name = name
		)
	}
}

# The polygon `xy_polygon` (`x` and `y` relative to its centre) centred on each of the points (`x0`, `y0`):
# the `x`, `y`, and polygon `id` of all the vertices (in order, polygon by polygon).
# (vectorized: a loop over points growing a list / data frame is quadratic in the number of points, see issue #119)
tile_xy_polygon <- function(x0, y0, xy_polygon) {
	n_points <- length(x0)
	n_vertices <- length(xy_polygon$x)
	list(
		x = rep(xy_polygon$x, times = n_points) + rep(x0, each = n_vertices),
		y = rep(xy_polygon$y, times = n_points) + rep(y0, each = n_vertices),
		id = rep(seq_len(n_points), each = n_vertices)
	)
}

# `xy_polygon` has `x` and `y` elements which will be added to each point in `sf_points`
sf_points_to_polygon_grob <- function(
	sf_points,
	xy_polygon,
	gp = gpar(),
	default.units = "npc",
	name = NULL
) {
	points_mat <- as.matrix(sf_points)
	if (nrow(points_mat) == 0L) {
		return(nullGrob())
	}
	xy <- tile_xy_polygon(points_mat[, 1L], points_mat[, 2L], xy_polygon)
	polygonGrob(x = xy$x, y = xy$y, id = xy$id, default.units = default.units, gp = gp, name = name)
}

# `xy_polygon` has `x` and `y` elements which will be added to each point in `sf_points`
sf_points_to_sf_multipolygon <- function(sf_points, xy_polygon) {
	points_mat <- as.matrix(sf_points)
	# `{sf}` wants explicitly closed polygons
	xy_ring <- list(x = c(xy_polygon$x, xy_polygon$x[1L]), y = c(xy_polygon$y, xy_polygon$y[1L]))
	sf::st_multipolygon(xy_rings(points_mat[, 1L], points_mat[, 2L], xy_ring))
}

# the closed ring `xy_ring` centred on each point as a list of (lists of) vertex matrices for `sf::st_multipolygon()`
xy_rings <- function(x0, y0, xy_ring) {
	xy <- tile_xy_polygon(x0, y0, xy_ring)
	m <- cbind(xy$x, xy$y)
	n_vertices <- length(xy_ring$x)
	lapply(seq_along(x0), function(k) {
		list(m[(k - 1L) * n_vertices + seq_len(n_vertices), , drop = FALSE])
	})
}
