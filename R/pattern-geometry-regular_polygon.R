#' Regular polygon patterned grobs
#'
#' `grid.pattern_regular_polygon()` draws a regular polygon pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param rot Angle to rotate regular polygon (degrees, counter-clockwise).
#' @param scale For star polygons, multiplier (between 0 and 1)
#'              applied to exterior radius to get interior radius.
#' @param shape Either "convex" or "star" followed by the number of exterior vertices
#'              or alternatively "circle", "square", "null", "rhombille_rhombus",
#'              "tetrakis_left", or "tetrakis_right".
#'              For example "convex5" corresponds to a pentagon
#'              and "star6" corresponds to a six-pointed star.
#'              The "square" shape is larger than the "convex4" shape and is rotated an extra 45 degrees,
#'              it can be used to generate a multi-colored \dQuote{checkers} effect when density is 1.
#'              The "null" shape is not drawn, it can be used to create holes within multiple-element patterns.
#'              The "rhombille_rhombus" shape draws a rhombus while the
#'              "tetrakis_left" or "tetrakis_right" shapes draw an isosceles right triangle.
#'              These latter three non-regular-polygon shapes are
#'              intended to help generate rhombille and tetrakis square tilings.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @seealso [grid.pattern_circle()] for a special case of this pattern.
#'          The tiling vignette features more examples of regular polygon tiling using
#'          this function `vignette("tiling", package = "gridpattern")`.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'
#' # 'density', 'rot', and 'shape' are vectorized
#' grid.pattern_regular_polygon(x_hex, y_hex, colour = "black",
#'                              fill = c("blue", "yellow", "red"),
#'                              shape = c("convex4", "star8", "circle"),
#'                              density = c(0.45, 0.42, 0.4),
#'                              spacing = 0.08, angle = 0)
#'
#' # checker pattern using "square" shape
#' grid::grid.newpage()
#' grid.pattern_regular_polygon(x_hex, y_hex, shape = "square",
#'                              colour = "transparent",
#'                              fill = c("black", "red", "blue", "yellow"),
#'                              angle = 0, density = 1.0, spacing = 0.2)
#'
#' # checker pattern using the "convex4" shape
#' grid::grid.newpage()
#' grid.pattern_regular_polygon(x_hex, y_hex, shape = "convex4", density = 1.0,
#'                              colour = "black", fill = "blue")
#'
#' # using a "twill_zigzag" 'weave' pattern
#' grid::grid.newpage()
#' grid.pattern_regular_polygon(x_hex, y_hex, fill = c("blue", "yellow"),
#'                              shape = c("circle", "star8"),
#'                              density = c(0.5, 0.6), type = "twill_zigzag")
#'
#' # hexagon tiling
#' grid::grid.newpage()
#' grid.pattern_regular_polygon(x_hex, y_hex, color = "transparent",
#'                              fill = c("white", "grey", "black"),
#'                              density = 1.0, spacing = 0.1,
#'                              shape = "convex6", grid = "hex")
#'
#' # triangle tiling
#' grid::grid.newpage()
#' grid.pattern_regular_polygon(x_hex, y_hex, fill = "green",
#'                              density = 1.0, spacing = 0.1,
#'                              shape = "convex3", grid = "hex")
#' @export
grid.pattern_regular_polygon <- function(
	x = c(0, 0, 1, 1),
	y = c(1, 0, 0, 1),
	id = 1L,
	...,
	colour = gp$col %||% "grey20",
	fill = gp$fill %||% "grey80",
	angle = 30,
	density = 0.2,
	spacing = 0.05,
	xoffset = 0,
	yoffset = 0,
	units = "snpc",
	scale = 0.5,
	shape = "convex6",
	grid = "square",
	type = NULL,
	subtype = NULL,
	rot = 0,
	alpha = gp$alpha %||% NA_real_,
	linetype = gp$lty %||% 1,
	linewidth = size %||% gp$lwd %||% 0.5,
	size = NULL,
	default.units = "npc",
	name = NULL,
	gp = gpar(),
	draw = TRUE,
	vp = NULL
) {
	if (missing(colour) && hasName(l <- list(...), "color")) {
		colour <- l$color
	}
	grid.pattern(
		"gridpattern::regular_polygon",
		x,
		y,
		id,
		colour = colour,
		fill = fill,
		angle = angle,
		density = density,
		spacing = spacing,
		xoffset = xoffset,
		yoffset = yoffset,
		units = units,
		scale = scale,
		shape = shape,
		grid = grid,
		type = type,
		subtype = subtype,
		rot = rot,
		alpha = alpha,
		linetype = linetype,
		linewidth = linewidth,
		default.units = default.units,
		name = name,
		gp = gp,
		draw = draw,
		vp = vp
	)
}

create_pattern_regular_polygon_via_sf <- function(
	params,
	boundary_df,
	aspect_ratio,
	legend = FALSE
) {
	# work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
	default.units <- "bigpts"
	boundary_df <- convert_polygon_df_units(boundary_df, default.units)
	params <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)

	spacing <- params$pattern_spacing
	grid <- params$pattern_grid

	# create grid of points large enough to cover viewport no matter the angle
	grid_xy <- get_xy_grid(params, vpm)

	# construct grobs using subsets if certain inputs are vectorized
	fill <- update_alpha(params$pattern_fill, params$pattern_alpha)
	col <- update_alpha(params$pattern_colour, params$pattern_alpha)
	lwd <- params$pattern_linewidth * .pt
	lty <- params$pattern_linetype

	density <- params$pattern_density
	rot <- params$pattern_rot
	shape <- params$pattern_shape
	assert_rp_shape(shape)

	n_par <- max(lengths(list(fill, col, lwd, lty, density, rot, shape)))

	fill <- rep_len_fill(fill, n_par)
	col <- rep_len(col, n_par)
	lwd <- rep_len(lwd, n_par)
	lty <- rep_len(lty, n_par)
	density <- rep_len(density, n_par)
	rot <- rep_len(rot, n_par)
	shape <- rep_len(shape, n_par)

	density <- ifelse(
		shape %in% c("square", "tetrakis_left", "tetrakis_right"),
		1.414 * density,
		density
	)
	# avoid overlap errors when density == 1 due to machine precision issues
	if (grid == "square") {
		density <- ifelse(nigh(density, 1), 0.9999, density)
	}
	if (grepl("^hex", grid) && n_par < 3L) {
		density <- ifelse(nigh(density, 1), 0.994, density)
	}
	density_max <- max(density)

	# compute regular polygon relative coordinates which we will center on points
	radius_mult <- switch(grid, hex = 0.578, 0.5)
	radius_max <- radius_mult * spacing * density_max

	#### add fudge factor?
	valid_sf <- convert_polygon_df_to_valid_sf(boundary_df)
	boundary_sf <- buffer_polygon_sf(valid_sf, buffer_dist = 0)
	expanded_sf <- buffer_polygon_sf(valid_sf, buffer_dist = radius_max)
	contracted_sf <- buffer_polygon_sf(valid_sf, buffer_dist = -radius_max)
	bbox <- boundary_bbox(boundary_df, margin = radius_max)

	# compute pattern matrix of graphical elements (e.g. fill colors)
	if (is.null(params$pattern_type) || is.na(params$pattern_type)) {
		params$pattern_type <- switch(grid, square = "square", "hex")
	}
	m_pat <- get_pattern_matrix(params$pattern_type, params$pattern_subtype, grid_xy, n_par)

	gl <- gList()
	for (i_par in seq(n_par)) {
		if (shape[i_par] == "null") {
			next
		}
		radius_outer <- radius_mult * spacing * density[i_par]
		xy_polygon <- get_xy_polygon(shape[i_par], params, radius_outer, rot[i_par])
		# the (rotated) grid points whose polygons can reach the boundary
		xy_par <- get_xy_par_within(
			grid_xy,
			i_par,
			m_pat,
			grid,
			spacing,
			params$pattern_angle,
			vpm,
			bbox
		)
		if (length(xy_par$x) == 0) {
			next
		}

		# test if polygons within/near boundary
		points_sf <- sf::st_multipoint(as.matrix(as.data.frame(xy_par)))
		all_points_sf <- sf::st_intersection(expanded_sf, points_sf)
		interior_points_sf <- sf::st_intersection(all_points_sf, contracted_sf)
		exterior_points_sf <- sf::st_difference(all_points_sf, contracted_sf)

		gp <- gpar(fill = fill[[i_par]], col = col[i_par], lwd = lwd[i_par], lty = lty[i_par])

		# create grob for interior polygons
		name <- paste0("interior.", i_par)
		if (shape[i_par] == "circle") {
			grob <- sf_points_to_circle_grob(
				interior_points_sf,
				radius_outer,
				gp,
				default.units,
				name
			)
		} else {
			grob <- sf_points_to_polygon_grob(
				interior_points_sf,
				xy_polygon,
				gp,
				default.units,
				name
			)
		}
		gl <- append_gList(gl, grob)

		# create grob for exterior polygons
		polygons_sf <- sf_points_to_sf_multipolygon(exterior_points_sf, xy_polygon)
		exterior_multipolygon <- sf::st_intersection(polygons_sf, boundary_sf)
		name <- paste0("boundary.", i_par)
		grob <- sf_multipolygon_to_polygon_grob(exterior_multipolygon, gp, default.units, name)
		gl <- append_gList(gl, grob)
	}
	gTree(children = gl, name = "regular_polygon")
}

get_pattern_matrix <- function(type, subtype, grid_xy, n_par) {
	check_grid_density(grid_xy)
	nrow <- length(grid_xy$y)
	ncol <- length(grid_xy$x)
	if (is_pattern_square(type)) {
		if (is.null(subtype) || is.na(subtype)) {
			if (type %in% names_weave) {
				subtype <- NULL
			} else {
				subtype <- n_par
			}
		}
		if (type %in% names_weave && n_par > 2) {
			abort(c(
				glue("pattern_type '{type}' can't arrange more than two elements"),
				i = glue("We detected {n_par} elements requested")
			))
		}
		m_pat <- pattern_square(type, subtype, nrow = nrow, ncol = ncol)
	} else {
		if (is.null(subtype) || is.na(subtype)) {
			subtype <- n_par
		}
		m_pat <- pattern_hex(type, subtype, nrow = nrow, ncol = ncol)
	}
	m_pat
}

# The grid points (in row-major order) whose `m_pat` entry is `i_par`
# (`rows` / `cols` restrict the grid rows / columns examined, see `get_xy_par_within()`)
get_xy_par <- function(
	grid_xy,
	i_par,
	m_pat,
	grid,
	spacing,
	rows = seq_along(grid_xy$y),
	cols = seq_along(grid_xy$x)
) {
	ij <- which_row_major(m_pat[rows, cols, drop = FALSE], i_par)
	i <- rows[ij$i]
	j <- cols[ij$j]
	# every other row of a "hex" grid (every other pair of rows of an "elongated_triangle" grid)
	# is offset half a spacing to the left
	is_offset <- switch(grid, square = FALSE, elongated_triangle = i %% 4L %in% 1:2, i %% 2L == 0L)
	list(x = grid_xy$x[j] - is_offset * 0.5 * spacing, y = grid_xy$y[i])
}
# The grid points (rotated by `angle` degrees about the viewport centre) whose `m_pat` entry is `i_par`
# and that lie within `bbox` (a `boundary_bbox()`).
# The grid covers a square around the whole viewport (so it covers it at any angle)
# but in a thin viewport most of it is nowhere near the boundary (issue #119)
# so only the grid rows and columns that can reach `bbox` are examined.
get_xy_par_within <- function(grid_xy, i_par, m_pat, grid, spacing, angle, vpm, bbox) {
	# `bbox` in the unrotated grid's coordinates (plus a margin for the offset rows and rounding)
	corners <- rotate_xy(
		bbox[c("xmin", "xmax", "xmax", "xmin")],
		bbox[c("ymin", "ymin", "ymax", "ymax")],
		-angle,
		vpm$x,
		vpm$y
	)
	margin <- 0.5 * spacing
	rows <- which(grid_xy$y >= min(corners$y) - margin & grid_xy$y <= max(corners$y) + margin)
	cols <- which(grid_xy$x >= min(corners$x) - margin & grid_xy$x <= max(corners$x) + margin)
	xy <- get_xy_par(grid_xy, i_par, m_pat, grid, spacing, rows, cols)
	xy <- rotate_xy(xy$x, xy$y, angle, vpm$x, vpm$y)
	filter_xy_bbox(xy, bbox)
}
# the bounding box of the boundary expanded by `margin` (`NA` if a coordinate is missing)
boundary_bbox <- function(boundary_df, margin = 0) {
	x <- range(boundary_df$x)
	y <- range(boundary_df$y)
	c(xmin = x[1L] - margin, xmax = x[2L] + margin, ymin = y[1L] - margin, ymax = y[2L] + margin)
}
# the points within `bbox` (none if `bbox` is `NA`)
filter_xy_bbox <- function(xy, bbox) {
	keep <- which(
		xy$x >= bbox[["xmin"]] &
			xy$x <= bbox[["xmax"]] &
			xy$y >= bbox[["ymin"]] &
			xy$y <= bbox[["ymax"]]
	)
	list(x = xy$x[keep], y = xy$y[keep])
}
# row (`i`) and column (`j`) indices of the entries of matrix `m` equal to `value`
# in row-major order i.e. the order `for (i in rows) for (j in cols)` visits them
# (vectorized: a loop over rows growing `x` / `y` with `c()` is quadratic in the number of rows, see issue #119)
which_row_major <- function(m, value) {
	ij <- which(m == value, arr.ind = TRUE, useNames = FALSE)
	o <- order(ij[, 1L], ij[, 2L])
	list(i = ij[o, 1L], j = ij[o, 2L])
}

# A two-dimensional pattern grid (of points covering a square around the viewport) denser than this isn't drawn at all:
# the pattern would be invisible anyway while its matrices could exhaust memory (issue #119).
# With the default "snpc" spacing the grid has about `(40 * aspect ratio)^2` points
# so this is only reached by a viewport over 80 times wider than tall (or a spacing about 1/1600 of the longer side).
MAX_GRID_POINTS <- 1e7
# signals a classed condition that `pattern_content()` catches to draw nothing instead
check_grid_density <- function(grid_xy) {
	n_points <- as.double(length(grid_xy$x)) * as.double(length(grid_xy$y))
	if (n_points > MAX_GRID_POINTS) {
		abort(
			glue(
				"The pattern grid of {format(n_points, big.mark = ',', scientific = FALSE)} points is too dense to draw"
			),
			class = "gridpattern_grid_too_dense"
		)
	}
	invisible(grid_xy)
}

# Create a grid of points large enough to cover the viewport no matter the angle:
# a square around the viewport centre whose side is the viewport's longer side
# (so a thin viewport has far more rows than its boundary needs, see `get_xy_par_within()`).
get_xy_grid <- function(params, vpm, wavelength = FALSE) {
	xoffset <- params$pattern_xoffset
	yoffset <- params$pattern_yoffset
	if (wavelength) {
		h_spacing <- params$pattern_wavelength
	} else {
		h_spacing <- params$pattern_spacing
	}

	gm <- 1.00 # seems to need to be this big so {ggpattern} legends render correctly
	x_adjust <- switch(
		params$pattern_grid,
		hex = 0.5 * h_spacing,
		elongated_triangle = 0.5 * h_spacing,
		0
	)
	x_seq <- seq_robust(from = 0, to = gm * vpm$length + x_adjust, by = h_spacing)
	x <- xoffset + vpm$x + c(rev(tail(-x_seq, -1L)), x_seq)
	if (length(x) == 1L) {
		x_min <- vpm$x - vpm$length
		x_max <- vpm$x + vpm$length
	} else {
		x_min <- min(x)
		x_max <- max(x)
	}

	# adjust vertical spacing for "hex" pattern
	if (params$pattern_grid == "square") {
		v_spacing <- params$pattern_spacing
	} else if (params$pattern_grid == "elongated_triangle") {
		v_spacing <- (0.5 + 0.25 * sqrt(3)) * params$pattern_spacing
	} else {
		v_spacing <- 0.5 * sqrt(3) * params$pattern_spacing
	}
	y_seq <- seq_robust(from = 0, to = gm * vpm$length, by = v_spacing)
	# ensure middle y point in a hex grid is an odd number so we don't accidentally offset it
	if (params$pattern_grid != "square" && (length(y_seq) %% 2L == 0L)) {
		y_seq <- c(y_seq, y_seq[length(y_seq)] + v_spacing)
	}
	y <- yoffset + vpm$y + c(rev(tail(-y_seq, -1L)), y_seq)
	if (params$pattern_grid == "elongated_triangle") {
		y <- y + rep(c(0, -0.15 * v_spacing), length.out = length(y))
	}
	if (length(y) == 1L) {
		y_min <- vpm$y - vpm$length
		y_max <- vpm$y + vpm$length
	} else {
		y_min <- min(y)
		y_max <- max(y)
	}

	list(
		x = x,
		y = y,
		x_min = x_min,
		x_max = x_max,
		y_min = y_min,
		y_max = y_max,
		h_spacing = h_spacing,
		v_spacing = v_spacing
	)
}

get_xy_polygon <- function(shape, params, radius_outer, rot) {
	if (shape %in% c("square", "tetrakis_left", "tetrakis_right")) {
		rot <- rot + 45
	}
	if (shape == "square") {
		shape <- "convex4"
	}
	polygon_angle <- 90 + rot + params$pattern_angle
	if (shape == "circle") {
		# grid::grobPoints.circle() defaults to regular polygon with 100 vertices
		convex_xy(100, polygon_angle, radius_outer)
	} else if (grepl("convex", shape)) {
		n_vertices <- get_n_vertices(shape)
		convex_xy(n_vertices, polygon_angle, radius_outer)
	} else if (shape == "rhombille_rhombus") {
		rhombus_xy(polygon_angle, radius_outer)
	} else if (shape == "tetrakis_left") {
		tetrakis_left_xy(polygon_angle, radius_outer)
	} else if (shape == "tetrakis_right") {
		tetrakis_right_xy(polygon_angle, radius_outer)
	} else {
		n_vertices <- get_n_vertices(shape)
		radius_inner <- params$pattern_scale * radius_outer
		concave_xy(n_vertices, polygon_angle, radius_outer, radius_inner)
	}
}

assert_rp_shape <- function(shape) {
	tf <- grepl(
		"^convex[[:digit:]]+$|^star[[:digit:]]+$|^square$|^circle$|^null$|^tetrakis_left$|^tetrakis_right$|^rhombille_rhombus$",
		shape
	)
	if (all(tf)) {
		invisible(NULL)
	} else {
		shape <- shape[which(!tf)[1]]
		msg <- c(
			paste("Unknown shape", shape),
			i = 'See `help("grid.pattern_regular_polygon")` for supported shapes'
		)
		abort(msg)
	}
}
