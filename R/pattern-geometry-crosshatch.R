#' Crosshatch patterned grobs
#'
#' `grid.pattern_crosshatch()` draws a crosshatch pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @param fill2 The fill colour for the \dQuote{top} crosshatch lines.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' grid.pattern_crosshatch(x_hex, y_hex, colour = "black", fill = "blue",
#'                         fill2 = "yellow", density = 0.5)
#' grid::grid.newpage()
#' grid.pattern_crosshatch(x_hex, y_hex, density = 0.3,
#'                         gp = grid::gpar(col = "blue", fill = "yellow"))
#' @seealso [grid.pattern_weave()] which interweaves two sets of lines.
#'        For a single set of lines use [grid.pattern_stripe()].
#' @export
grid.pattern_crosshatch <- function(
	x = c(0, 0, 1, 1),
	y = c(1, 0, 0, 1),
	id = 1L,
	...,
	colour = gp$col %||% "grey20",
	fill = gp$fill %||% "grey80",
	fill2 = fill,
	angle = 30,
	density = 0.2,
	spacing = 0.05,
	xoffset = 0,
	yoffset = 0,
	units = "snpc",
	alpha = gp$alpha %||% NA_real_,
	linetype = gp$lty %||% 1,
	linewidth = size %||% gp$lwd %||% 0.5,
	size = NULL,
	grid = "square",
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
		"gridpattern::crosshatch",
		x,
		y,
		id,
		colour = colour,
		fill = fill,
		fill2 = fill2,
		angle = angle,
		density = density,
		spacing = spacing,
		xoffset = xoffset,
		yoffset = yoffset,
		units = units,
		alpha = alpha,
		linetype = linetype,
		linewidth = linewidth,
		grid = grid,
		default.units = default.units,
		name = name,
		gp = gp,
		draw = draw,
		vp = vp
	)
}

create_pattern_crosshatch_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	create_crosshatch_via_sf_helper(params, boundary_df, name = "bottom", add_top_hatch = TRUE)
}

# A gTree of the stripes (named `name`) clipped to the boundary plus (for a crosshatch) a "top" hatch
create_crosshatch_via_sf_helper <- function(params, boundary_df, name, add_top_hatch = TRUE) {
	# work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
	default.units <- "bigpts"
	boundary_df <- convert_polygon_df_units(boundary_df, default.units)
	params <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)

	# create grid of points large enough to cover viewport no matter the angle
	grid_xy <- get_xy_grid(params, vpm)

	boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)

	grob <- stripes_grob(params, grid_xy, vpm, boundary_sf, default.units, name, horizontal = TRUE)

	if (add_top_hatch) {
		grob_top <- stripes_grob(
			params,
			grid_xy,
			vpm,
			boundary_sf,
			default.units,
			"top",
			horizontal = FALSE,
			fill = params$pattern_fill2
		)
		grob <- grobTree(grob, grob_top, name = "crosshatch")
	}
	grob
}

# The stripes (horizontal before the rotation by the pattern angle if `horizontal` else vertical)
# clipped to `boundary_sf` as a gTree (named `name`) of a polygon grob (`name.1`, `name.2`, ...)
# per group of stripe parameters (see `stripe_params()`) filled with `fill` (e.g. `fill2` for a "top" hatch).
# Like the "wave" and "regular_polygon" patterns the stripes of each group are clipped separately:
# a stripe clipped into several pieces by several polygons is one colour
# (whatever the order of the `sf::st_intersection()` output) and each group can have its own `density`.
stripes_grob <- function(
	params,
	grid_xy,
	vpm,
	boundary_sf,
	default.units,
	name,
	horizontal,
	fill = params$pattern_fill
) {
	sp <- stripe_params(params, fill)
	clipped_sfs <- lapply(seq_len(sp$n_par), function(i_par) {
		params$pattern_density <- sp$density[i_par]
		grid_xy_par <- stripe_group(grid_xy, i_par, sp$n_par, horizontal)
		clip_stripes_sf(params, grid_xy_par, vpm, boundary_sf, horizontal)
	})
	stripe_groups_grob(clipped_sfs, sp, default.units, name)
}

# the gTree (named `name`) of a polygon grob (`name.1`, `name.2`, ...) per group of stripes
# given the clipped `{sf}` geometry of each group and their parameters `sp` (see `stripe_params()`)
stripe_groups_grob <- function(clipped_sfs, sp, default.units, name) {
	gl <- gList()
	for (i_par in seq_along(clipped_sfs)) {
		grob <- sf_multipolygon_to_polygon_grob(
			clipped_sfs[[i_par]],
			stripe_gpar(sp, i_par),
			default.units,
			paste0(name, ".", i_par)
		)
		gl <- append_gList(gl, grob)
	}
	gTree(children = gl, name = name)
}

# The parameters of the stripes recycled to one value per group of stripes:
# like the "wave" and "regular_polygon" patterns multiple `fill` (the pattern's `fill` by default
# e.g. its `fill2` for a crosshatch's "top" hatch), `colour`, `linewidth`, `linetype`,
# and `density` values cycle by stripe i.e. `n_par` groups of every `n_par`-th stripe (see `stripe_group()`).
stripe_params <- function(params, fill = params$pattern_fill) {
	fill <- update_alpha(fill, params$pattern_alpha)
	col <- update_alpha(params$pattern_colour, params$pattern_alpha)
	lwd <- params$pattern_linewidth * .pt
	lty <- params$pattern_linetype
	density <- params$pattern_density
	# avoid overlap errors when density == 1 due to machine precision issues
	density[nigh(density, 1)] <- 1 - 1e-6
	stopifnot(all(density <= 1))
	# (a `grid::pattern()` fill is one value although its length isn't one)
	n_fill <- if (is_pattern(fill)) 1L else length(fill)
	n_par <- max(n_fill, lengths(list(col, lwd, lty, density)))
	list(
		n_par = n_par,
		fill = rep_len_fill(fill, n_par),
		col = rep_len(col, n_par),
		lwd = rep_len(lwd, n_par),
		lty = rep_len(lty, n_par),
		density = rep_len(density, n_par)
	)
}

# the graphical parameters of group `i_par` of the stripes
stripe_gpar <- function(sp, i_par) {
	gpar(
		col = sp$col[i_par],
		fill = sp$fill[[i_par]],
		lwd = sp$lwd[i_par],
		lty = sp$lty[i_par],
		lineend = "square"
	)
}

# `grid_xy` with only the stripe centres (`y` if `horizontal` else `x`) of group `i_par` of `n_par`:
# every `n_par`-th stripe counted from the stripe through the viewport centre (which is in group one)
# so the colour of a stripe doesn't depend on the boundary.
stripe_group <- function(grid_xy, i_par, n_par, horizontal = TRUE) {
	nm <- if (horizontal) "y" else "x"
	centres <- grid_xy[[nm]]
	# (`get_xy_grid()` centres an odd number of stripes on the viewport)
	k <- seq_along(centres) - (length(centres) + 1L) %/% 2L
	grid_xy[[nm]] <- centres[k %% n_par + 1L == i_par]
	grid_xy
}

# the stripes of `create_h_stripes_sf()` (if `horizontal`) / `create_v_stripes_sf()` clipped to `boundary_sf`
clip_stripes_sf <- function(params, grid_xy, vpm, boundary_sf, horizontal) {
	stripes_sf <- if (horizontal) {
		create_h_stripes_sf(params, grid_xy, vpm)
	} else {
		create_v_stripes_sf(params, grid_xy, vpm)
	}
	sf::st_intersection(stripes_sf, boundary_sf)
}

# build sf multipolygon 'rect' for each grid_xy$y value
create_h_stripes_sf <- function(params, grid_xy, vpm) {
	halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
	l_rects <- lapply(grid_xy$y, function(y0) {
		x <- c(grid_xy$x_min, grid_xy$x_min, grid_xy$x_max, grid_xy$x_max)
		y <- y0 + c(-1, 1, 1, -1) * halfwidth
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_rects)
}

# build sf multipolygon 'rect' for each grid_xy$x value
create_v_stripes_sf <- function(params, grid_xy, vpm) {
	halfwidth <- 0.5 * grid_xy$h_spacing * params$pattern_density
	l_rects <- lapply(grid_xy$x, function(x0) {
		x <- x0 + c(-1, 1, 1, -1) * halfwidth
		y <- c(grid_xy$y_min, grid_xy$y_min, grid_xy$y_max, grid_xy$y_max)
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_rects)
}
