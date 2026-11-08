#' Weave patterned grobs
#'
#' `grid.pattern_weave()` draws a weave pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_crosshatch
#' @param fill The fill colour for the horizontal "weft" lines.
#' @param fill2 The fill colour for the vertical "warp" lines.
#' @param type The weave type.  See [pattern_weave()] for more details.
#' @param subtype The weave subtype.  See [pattern_weave()] for more details.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' gp <- grid::gpar(colour = "black", fill = "lightblue", lwd=0.5)
#'
#' # Plain weave (default weave)
#' grid.pattern_weave(x_hex, y_hex, fill2 = "yellow",
#'                    gp = gp, spacing = 0.1, density = 0.3)
#'
#' # Irregular matt weave
#' grid::grid.newpage()
#' grid.pattern_weave(x_hex, y_hex,  type = "matt_irregular",
#'                    fill2 = "yellow", gp = gp, spacing = 0.1, density = 0.3)
#'
#' # Twill weave
#' grid::grid.newpage()
#' grid.pattern_weave(x_hex, y_hex, type = "twill",
#'                    fill2 = "yellow", gp = gp, spacing = 0.1, density = 0.3)
#'
#' # Zig-zag twill
#' grid::grid.newpage()
#' grid.pattern_weave(x_hex, y_hex, type = "twill_zigzag",
#'                    fill2 = "yellow", gp = gp, spacing = 0.05, density = 0.7)
#'
#' # Herringbone twill with density 1
#' grid::grid.newpage()
#' gp$col <- NA
#' grid.pattern_weave(x_hex, y_hex, type = "twill_herringbone",
#'                    fill2 = "yellow", gp = gp, spacing = 0.05, density = 1.0)
#' @seealso [pattern_weave()]
#' @export
grid.pattern_weave <- function(
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
	type = "plain",
	subtype = NA,
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
		"gridpattern::weave",
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
		type = type,
		subtype = subtype,
		default.units = default.units,
		name = name,
		gp = gp,
		draw = draw,
		vp = vp
	)
}

create_pattern_weave_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	# (the squares of the warp covered by the weft are sized by one `density`, see `create_warp_covered_sf()`)
	if (length(params$pattern_density) != 1L) {
		abort("The \"weave\" pattern needs a single `density` value.")
	}
	# 'weft' 'yarns' will just be normal (horizontal) stripes
	grob_weft <- create_crosshatch_via_sf_helper(
		params,
		boundary_df,
		add_top_hatch = FALSE,
		name = "weft"
	)
	# we'll compute 'covered' and 'uncovered' 'warp' rectangles to represent warp 'yarns'
	l <- create_warp_via_sf(params, boundary_df)
	grobTree(l$warp_covered, grob_weft, l$warp_uncovered, name = "weave")
}

create_warp_via_sf <- function(params, boundary_df) {
	# work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
	default.units <- "bigpts"
	boundary_df <- convert_polygon_df_units(boundary_df, default.units)
	params <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)

	# create grid of points large enough to cover viewport no matter the angle
	grid_xy <- get_xy_grid(params, vpm)
	check_grid_density(grid_xy)

	# the warp yarns are vertical stripes with one group of stripes per `fill2` (etc.) value (see `stripes_grob()`)
	params$pattern_fill <- params$pattern_fill2
	sp <- stripe_params(params)
	params$pattern_density <- sp$density[1L] # (the one `density` of a weave, see `create_pattern_weave_via_sf()`)

	m_weave <- pattern_weave(
		params$pattern_type,
		params$pattern_subtype,
		nrow = length(grid_xy$y),
		ncol = length(grid_xy$x)
	)

	boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)

	# compute warp squares covered by weft lines
	warp_covered_sf <- create_warp_covered_sf(params, grid_xy, vpm, m_weave, boundary_df)
	warp_covered_sf <- sf::st_buffer(warp_covered_sf, dist = 0)

	gl_covered <- gList()
	gl_uncovered <- gList()
	for (i_par in seq_len(sp$n_par)) {
		# compute vertical stripes clipped to boundary
		grid_xy_par <- stripe_group(grid_xy, i_par, sp$n_par, horizontal = FALSE)
		clipped_stripes_sf <- clip_stripes_sf(
			params,
			grid_xy_par,
			vpm,
			boundary_sf,
			horizontal = FALSE
		)
		clipped_covered_sf <- sf::st_intersection(clipped_stripes_sf, warp_covered_sf)

		# warp rectangles not covered by weft lines is just stripes minus under squares
		buffered_covered_sf <- sf::st_buffer(clipped_covered_sf, vpm$length / 1e9)
		clipped_uncovered_sf <- sf::st_difference(clipped_stripes_sf, buffered_covered_sf)

		# (the covered and uncovered pieces of a warp yarn are the same colour)
		gp <- stripe_gpar(sp, i_par)
		grob_covered <- sf_multipolygon_to_polygon_grob(
			clipped_covered_sf,
			gp,
			default.units,
			paste0("warp_covered.", i_par)
		)
		grob_uncovered <- sf_multipolygon_to_polygon_grob(
			clipped_uncovered_sf,
			gp,
			default.units,
			paste0("warp_uncovered.", i_par)
		)
		gl_covered <- append_gList(gl_covered, grob_covered)
		gl_uncovered <- append_gList(gl_uncovered, grob_uncovered)
	}

	list(
		warp_covered = gTree(children = gl_covered, name = "warp_covered"),
		warp_uncovered = gTree(children = gl_uncovered, name = "warp_uncovered")
	)
}

# the (rotated) squares around the grid points covered by weft yarns
create_warp_covered_sf <- function(params, grid_xy, vpm, m_weave, boundary_df) {
	halfwidth <- 0.5 * grid_xy$h_spacing * params$pattern_density
	angle <- params$pattern_angle
	# only the squares that can reach the boundary matter (the warp is clipped to it)
	bbox <- boundary_bbox(boundary_df, margin = sqrt(2) * halfwidth)
	xy <- get_xy_par_within(grid_xy, FALSE, m_weave, "square", grid_xy$h_spacing, angle, vpm, bbox)
	# the closed ring of a square (rotated about its centre) around each point
	square <- rotate_xy(
		c(-1, -1, 1, 1, -1) * halfwidth,
		c(-1, 1, 1, -1, -1) * halfwidth,
		angle,
		0,
		0
	)
	sf::st_multipolygon(xy_rings(xy$x, xy$y, square))
}
