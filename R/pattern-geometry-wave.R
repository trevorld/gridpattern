#' Wave patterned grobs
#'
#' `grid.pattern_wave()` draws a wave pattern onto the graphic device.
#' `names_wave` is a character vector of supported `type` values.
#'
#' @inheritParams grid.pattern_circle
#' @param units [grid::unit()] units for `amplitude`, `frequency`, `spacing`, `xoffset`, and `yoffset` parameters.
#' @param amplitude Wave amplitude (in `units` units)
#' @param frequency Linear frequency (in inverse `units` units)
#' @param type One of the following (see `names_wave` for the canonical list):
#'   \describe{
#'     \item{`"dovetailed"`}{A wave with diagonal strokes connecting the crests and troughs.  Alias: `"dovetail"`.}
#'     \item{`"embattled"`}{Square wave. Alias: `"square"`.}
#'     \item{`"embattled_grady"`}{Graduated stepped wave: two ascending steps followed by two descending steps per period.}
#'     \item{`"engrailed"`}{Repeating arches curving downward (non-positive half of a sine wave per period).}
#'     \item{`"indented"` (default)}{Triangular wave with equal rise and fall. Alias: `"triangle"`.}
#'     \item{`"invected"`}{Repeating arches curving upward (non-negative half of a sine wave per period).}
#'     \item{`"nebuly"`}{Smooth cloud-like wave.}
#'     \item{`"potenty"`}{A stepped wave with T-shaped crenellations.}
#'     \item{`"raguly"`}{Oblique stepped wave.  Use `reverse = TRUE` for the horizontally mirror image.}
#'     \item{`"sawtoothed"`}{Sawtooth wave with a gradual rise and sharp fall.  Use `reverse = TRUE` for a sharp rise and gradual fall.  Aliases: `"sawlike"`, `"sawtooth"`.}
#'     \item{`"urdy"`}{A wave with pointed crests and troughs.}
#'     \item{`"wavy"`}{Smooth sinusoidal wave. Aliases: `"sine"`, `"undy"`.}
#'   }
#' @param reverse If `TRUE`, horizontally mirror the wave.  Currently affects `"sawtoothed"` and `"raguly"` only.  Default `FALSE`.
#' @param stagger If `TRUE`, alternate wave rows are shifted by half a wavelength so that
#'   crests of one row align with troughs of adjacent rows, creating an interlocking effect.
#'   Default `FALSE`.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' print(names_wave)
#'
#' # visual table of all wave types
#' grid::grid.newpage()
#' n <- length(names_wave)
#' nc <- 2L
#' nr <- ceiling(n / nc)
#' grid::pushViewport(grid::viewport(layout = grid::grid.layout(nr, nc)))
#' for (i in seq_len(n)) {
#'     grid::pushViewport(grid::viewport(
#'         layout.pos.row = (i - 1L) %/% nc + 1L,
#'         layout.pos.col = (i - 1L) %% nc + 1L
#'     ))
#'     grid.pattern_wave(colour = "black", fill = c("gold", "steelblue"),
#'                       type = names_wave[i], density = 0.18, spacing = 0.45,
#'                       angle = 0, amplitude = 0.100, frequency = 1 / 0.45)
#'     grid::grid.rect(x = 0.5, y = 0.86, width = 0.5, height = 0.28,
#'                     just = "centre", gp = grid::gpar(fill = "grey80", col = "black"))
#'     grid::grid.text(names_wave[i], x = 0.5, y = 0.88,
#'                     gp = grid::gpar(fontsize = 11))
#'     grid::grid.rect(gp = grid::gpar(fill = "transparent", col = "black", lwd = 6))
#'     grid::popViewport()
#' }
#' grid::popViewport()
#'
#' # stagger shifts alternate rows by half a wavelength
#' grid::grid.newpage()
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' grid.pattern_wave(x_hex, y_hex, colour = "black", type = "urdy",
#'                   fill = c("red", "blue"), density = 0.3,
#'                   spacing = 0.15, angle = 0,
#'                   amplitude = 0.045, frequency = 1 / 0.15, stagger = TRUE)
#' @seealso Use [grid.pattern_stripe()] for straight filled bands or [grid.pattern_line()] for stroked lines instead of waves.
#'   See <https://en.wikipedia.org/wiki/Line_(heraldry)> and <https://en.wikipedia.org/wiki/Waveform> for more information about the supported wave types.
#' @export
grid.pattern_wave <- function(
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
	amplitude = 0.5 * spacing,
	frequency = 1 / spacing,
	alpha = gp$alpha %||% NA_real_,
	linetype = gp$lty %||% 1,
	linewidth = size %||% gp$lwd %||% 1,
	size = NULL,
	grid = "square",
	type = "indented",
	reverse = FALSE,
	stagger = FALSE,
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
		"wave",
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
		amplitude = amplitude,
		frequency = frequency,
		alpha = alpha,
		linetype = linetype,
		linewidth = linewidth,
		grid = grid,
		type = type,
		reverse = reverse,
		stagger = stagger,
		default.units = default.units,
		name = name,
		gp = gp,
		draw = draw,
		vp = vp
	)
}

#' @rdname grid.pattern_wave
#' @export
names_wave <- c(
	"dovetailed",
	"embattled",
	"embattled_grady",
	"engrailed",
	"indented",
	"invected",
	"nebuly",
	"potenty",
	"raguly",
	"sawtoothed",
	"urdy",
	"wavy"
)

create_pattern_wave_via_sf <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	if (abs(params$pattern_density - 1) < .Machine$double.eps^0.5) {
		params$pattern_density <- 1 - 1e-6
	}
	stopifnot(params$pattern_density <= 1)

	# work in 'bigpts' instead 'npc' / 'snpc' units so we don't worry about the aspect ratio
	default.units <- "bigpts"
	boundary_df <- convert_polygon_df_units(boundary_df, default.units)
	params <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)

	# create grid of points large enough to cover viewport no matter the angle
	grid_xy <- get_xy_grid(params, vpm, wavelength = TRUE)

	fill <- update_alpha(params$pattern_fill, params$pattern_alpha)
	col <- update_alpha(params$pattern_colour, params$pattern_alpha)
	lwd <- params$pattern_linewidth * .pt
	lty <- params$pattern_linetype
	density <- params$pattern_density

	n_par <- max(lengths(list(fill, col, lwd, lty, density)))

	if (isTRUE(params$pattern_stagger) && n_par %% 2L == 1L) {
		halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
		if (2 * (params$pattern_amplitude + halfwidth) > n_par * grid_xy$v_spacing) {
			abort(c(
				"Wave stagger bands overlap between adjacent rows.",
				i = "Reduce `amplitude` or `density`, increase `spacing`, or use an even number of fill/colour values."
			))
		}
	}

	fill <- rep_len_fill(fill, n_par)
	col <- rep_len(col, n_par)
	lwd <- rep_len(lwd, n_par)
	lty <- rep_len(lty, n_par)
	density <- rep_len(density, n_par)

	gl <- gList()
	for (i_par in seq_len(n_par)) {
		gp <- gpar(
			col = col[i_par],
			fill = fill[[i_par]],
			lwd = lwd[i_par],
			lty = lty[i_par],
			lineend = 'square'
		)

		boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df, buffer_dist = 0)

		waves_sf <- create_waves_sf(params, grid_xy, vpm, i_par, n_par)
		clipped_waves_sf_bot <- tryCatch(
			sf::st_intersection(waves_sf, boundary_sf),
			error = function(e) {
				if (
					grepl(
						"TopologyException|Self-intersection|invalid",
						conditionMessage(e),
						ignore.case = TRUE
					)
				) {
					abort(
						c(
							paste0(
								"Wave pattern ",
								dQuote(params$pattern_type),
								" produced a self-intersecting polygon."
							),
							i = "Try reducing `density`, increasing `amplitude`, decreasing `frequency`, and/or increasing `spacing`."
						),
						call = NULL
					)
				}
				stop(e)
			}
		)
		name <- paste0("wave.", i_par)
		grob <- sf_multipolygon_to_polygon_grob(clipped_waves_sf_bot, gp, default.units, name)
		gl <- append_gList(gl, grob)
	}
	gTree(children = gl, name = "regular_polygon")
}

create_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	switch(
		params$pattern_type,
		sawlike = ,
		sawtooth = ,
		sawtoothed = create_sawtooth_waves_sf(params, grid_xy, vpm, i_par, n_par),
		dovetail = ,
		dovetailed = create_dovetail_waves_sf(params, grid_xy, vpm, i_par, n_par),
		embattled = ,
		square = create_stepped_waves_sf(params, grid_xy, vpm, i_par, n_par, n_steps = 1L),
		embattled_grady = create_stepped_waves_sf(params, grid_xy, vpm, i_par, n_par, n_steps = 2L),
		engrailed = create_sine_waves_sf(params, grid_xy, vpm, i_par, n_par),
		invected = create_sine_waves_sf(params, grid_xy, vpm, i_par, n_par),
		nebuly = create_nebuly_waves_sf(params, grid_xy, vpm, i_par, n_par),
		potenty = create_potenty_waves_sf(params, grid_xy, vpm, i_par, n_par),
		raguly = create_raguly_waves_sf(params, grid_xy, vpm, i_par, n_par),
		indented = ,
		triangle = create_triangle_waves_sf(params, grid_xy, vpm, i_par, n_par),
		urdy = create_urdy_waves_sf(params, grid_xy, vpm, i_par, n_par),
		wavy = ,
		sine = ,
		undy = create_sine_waves_sf(params, grid_xy, vpm, i_par, n_par),
		abort(paste("Don't know how to create wave pattern", dQuote(params$pattern_type)))
	)
}

create_sine_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	n_s <- 180L
	type <- params$pattern_type
	is_half_arch <- type %in% c("engrailed", "invected")
	if (is_half_arch) {
		# One arch per wavelength: theta spans only half a sine period (0 to π)
		theta <- to_radians(seq(0, by = 180L / n_s, length.out = n_s))
		# Use 2a amplitude and center at zero so the arch spans [-a, +a], matching
		# other wave types. Without the -/+a offset the arch baseline sits at y0±a.
		y_s_base <- if (type == "invected") 2 * a * sin(theta) - a else -2 * a * sin(theta) + a
	} else {
		theta <- to_radians(seq(0, by = 360L / n_s, length.out = n_s))
		y_s_base <- a * sin(theta)
	}
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)
	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_this <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L
		if (stagger_this) {
			y_s <- if (is_half_arch) {
				n_half <- n_s %/% 2L
				c(y_s_base[(n_half + 1L):n_s], y_s_base[1L:n_half])
			} else {
				-y_s_base
			}
		} else {
			y_s <- y_s_base
		}
		# Phase alignment: invected peak (y=+a) → x_min+λ/4 (matches sine peak, shift n_s/4).
		# Engrailed dent (y=−a) → x_min+3λ/4 (matches sine trough, shift 3*n_s/4).
		# Full sine already peaks at x_min+λ/4; no shift needed for it.
		if (is_half_arch) {
			phase_shift <- if (type == "engrailed") 3L * (n_s %/% 4L) else n_s %/% 4L
			y_s <- cycle_elements(y_s, phase_shift)
		}
		n_x <- length(grid_xy$x)
		xc <- seq(grid_xy$x_min, grid_xy$x_max, length.out = n_s * (n_x - 1L) + 1L)
		yc <- y0 + rep(y_s, length.out = n_s * (n_x - 1L) + 1L)
		yt <- yc + halfwidth
		yb <- yc - halfwidth
		x <- c(xc, rev(xc))
		y <- c(yt, rev(yb))
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

# Generalised stepped-wave constructor used by "square"/"embattled" (n_steps = 1) and
# "embattled_grady" (n_steps = 2). Phase is chosen so the top step is centred at λ/4,
# aligning with the sine/triangle wave peak. n_steps=1 starts HIGH; n_steps=2 starts MID.
create_stepped_waves_sf <- function(params, grid_xy, vpm, i_par, n_par, n_steps = 1L) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)

	# n_steps transitions per half-period; step height = 2a/n_steps; step width = wavelength/(2*n_steps).
	# Self-intersection: hw >= half the step width (band corners from adjacent steps overlap).
	step_height <- 2 * a / n_steps
	step_width <- wavelength / (2L * n_steps)
	half_step <- 0.5 * step_width
	if (hw >= half_step) {
		abort(c(
			paste0(
				toupper(substring(params$pattern_type, 1L, 1L)),
				substring(params$pattern_type, 2L),
				" wave: band corners overlap between adjacent steps."
			),
			i = "Reduce `density` or increase `spacing`/`frequency`."
		))
	}
	# Phase alignment: shift x_trans back by (n_steps %/% 2) half-steps so the top
	# step is centred at λ/4 (the sine peak).  For n_steps=1 the shift is 0 (unchanged);
	# for n_steps=2 the shift is half_step, moving the first transition to x_min+λ/8.
	phase_shift <- (n_steps %/% 2L) * half_step
	x_trans <- seq(x_min + step_width - phase_shift, x_max + wavelength, by = step_width)
	n_iter <- sum(x_trans <= x_max + hw)
	# sign convention: +1 = descent (high→low), -1 = ascent (low→high)
	base_signs <- cycle_elements(c(rep(1L, n_steps), rep(-1L, n_steps)), -(n_steps %/% 2L))

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L
		# init_level: level at x_min, offset from HIGH by the (n_steps %/% 2) leading ascents
		base_init <- a - step_height * (n_steps %/% 2L)
		init_level <- if (stagger_row) -base_init else base_init
		signs <- rep(
			if (stagger_row) -base_signs else base_signs,
			length.out = n_iter
		)
		n_alloc <- 2L * n_iter + 2L
		top_xs <- numeric(n_alloc)
		top_ys <- numeric(n_alloc)
		bot_xs <- numeric(n_alloc)
		bot_ys <- numeric(n_alloc)
		top_xs[1L] <- x_min
		top_ys[1L] <- y0 + init_level + hw
		bot_xs[1L] <- x_min
		bot_ys[1L] <- y0 + init_level - hw
		top_i <- 2L
		bot_i <- 2L

		for (k in seq_len(n_iter)) {
			xt <- x_trans[k]
			s <- signs[k]
			top_step <- xt + s * hw
			bot_step <- xt - s * hw
			delta_y <- -s * step_height
			if (top_step <= x_max) {
				top_xs[top_i] <- top_step
				top_ys[top_i] <- top_ys[top_i - 1L]
				top_i <- top_i + 1L
				top_xs[top_i] <- top_step
				top_ys[top_i] <- top_ys[top_i - 1L] + delta_y
				top_i <- top_i + 1L
			}
			if (bot_step >= x_min && bot_step <= x_max) {
				bot_xs[bot_i] <- bot_step
				bot_ys[bot_i] <- bot_ys[bot_i - 1L]
				bot_i <- bot_i + 1L
				bot_xs[bot_i] <- bot_step
				bot_ys[bot_i] <- bot_ys[bot_i - 1L] + delta_y
				bot_i <- bot_i + 1L
			}
		}
		top_xs[top_i] <- x_max
		top_ys[top_i] <- top_ys[top_i - 1L]
		bot_xs[bot_i] <- x_max
		bot_ys[bot_i] <- bot_ys[bot_i - 1L]

		x <- c(top_xs[seq_len(top_i)], rev(bot_xs[seq_len(bot_i)]))
		y <- c(top_ys[seq_len(top_i)], rev(bot_ys[seq_len(bot_i)]))
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

create_triangle_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	# Scale halfwidth so triangle's perpendicular stroke width matches sawtooth's.
	# The triangle diagonal has slope 4a/L vs sawtooth's 2a/L; dividing by the
	# steeper slope correction makes both waves appear equally thick on screen.
	halfwidth <- halfwidth * sqrt((1 + (4 * a / wavelength)^2) / (1 + (2 * a / wavelength)^2))
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)
	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		half_period_shape <- if (isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L) {
			c(0, -a, 0, a)
		} else {
			c(0, a, 0, -a)
		}
		n_x <- length(grid_xy$x)
		xc <- seq(grid_xy$x_min, grid_xy$x_max, length.out = 4L * (n_x - 1L) + 1L)
		yc <- y0 + rep(half_period_shape, length.out = 4L * (n_x - 1L) + 1L)
		yt <- yc + halfwidth
		yb <- yc - halfwidth
		x <- c(xc, rev(xc))
		y <- c(yt, rev(yb))
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

# Build a piecewise-linear edge (for sawtooth bands) with a vertical step of `step_y` at each
# x in `x_steps`. The edge starts at `y_init` at `x_min` and follows `slope` between steps.
sawtooth_edge_xy <- function(x_min, x_max, x_steps, slope, y_init, step_y) {
	valid <- x_steps[x_steps > x_min & x_steps <= x_max]
	n_valid <- length(valid)
	n_pts <- 2L * n_valid + 2L
	xs <- numeric(n_pts)
	ys <- numeric(n_pts)
	xs[1L] <- x_min
	ys[1L] <- y_init
	current_x <- x_min
	current_y <- y_init
	for (i in seq_len(n_valid)) {
		x_s <- valid[i]
		y_at_step <- current_y + slope * (x_s - current_x)
		j <- 2L * i
		xs[j] <- x_s
		ys[j] <- y_at_step
		xs[j + 1L] <- x_s
		ys[j + 1L] <- y_at_step + step_y
		current_x <- x_s
		current_y <- y_at_step + step_y
	}
	xs[n_pts] <- x_max
	ys[n_pts] <- current_y + slope * (x_max - current_x)
	list(x = xs, y = ys)
}

create_sawtooth_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)
	# reverse = FALSE: gradual rise, sharp fall (ramp up)
	# reverse = TRUE:  sharp rise, gradual fall (ramp down)
	up <- !isTRUE(params$pattern_reverse)

	# One transition per period; stagger shifts by half a wavelength
	x_trans <- seq(x_min + wavelength, x_max + wavelength, by = wavelength)

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L
		x_t <- if (stagger_row) x_trans - 0.5 * wavelength else x_trans

		if (up) {
			slope <- 2 * a / wavelength
			# gradual rise then sharp fall; stagger starts mid-ramp (center = y0)
			init_top <- if (stagger_row) y0 + hw else y0 - a + hw
			init_bot <- if (stagger_row) y0 - hw else y0 - a - hw
			# top drops at x_t + hw; bottom drops at x_t - hw
			top <- sawtooth_edge_xy(x_min, x_max, x_t + hw, slope, init_top, -2 * a)
			bot <- sawtooth_edge_xy(x_min, x_max, x_t - hw, slope, init_bot, -2 * a)
		} else {
			slope <- -2 * a / wavelength
			# sharp rise then gradual fall; stagger starts mid-ramp (center = y0)
			init_top <- if (stagger_row) y0 + hw else y0 + a + hw
			init_bot <- if (stagger_row) y0 - hw else y0 + a - hw
			# top rises at x_t - hw; bottom rises at x_t + hw
			top <- sawtooth_edge_xy(x_min, x_max, x_t - hw, slope, init_top, 2 * a)
			bot <- sawtooth_edge_xy(x_min, x_max, x_t + hw, slope, init_bot, 2 * a)
		}

		x <- c(top$x, rev(bot$x))
		y <- c(top$y, rev(bot$y))
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

create_urdy_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)

	# Each half-period: two diagonals separated by a vertical section.
	# Vertical section height = 2a/3 (one-third of total amplitude 2a).
	# Each diagonal spans wavelength/4 horizontally and 2a/3 vertically.
	v_ht <- 2 * a / 3
	d_wid <- wavelength / 4
	m_slope <- v_ht / d_wid # slope = 8a/(3L)

	# hw_y: y-offset for diagonals so perpendicular band width = hw = horizontal band width.
	# delta: miter-join offset at diagonal-vertical junctions.
	#   The diagonal band edge is extended to x ± hw (the vertical x), landing at y ± delta
	#   from the junction centre. This avoids the "backward step" that causes self-intersection.
	if (hw >= d_wid) {
		abort(c(
			"Urdy wave: band corners overlap between adjacent steps.",
			i = "Reduce `density` or increase `spacing`/`frequency`."
		))
	}
	hw_y <- hw * sqrt(1 + m_slope^2)
	hw_x <- hw
	delta <- hw_y - m_slope * hw_x # = hw * (sqrt(1+m²) - m)

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L

		# Phase: shift x_ps back by d_wid (= λ/4) so the peak lands at x_min + λ/4,
		# aligning with the sine/triangle peak.
		x_ps <- x_min - wavelength - d_wid
		if (stagger_row) {
			x_ps <- x_ps - 0.5 * wavelength
		}
		n_periods <- ceiling((x_max - x_ps) / wavelength) + 2L

		# Top edge = higher-y boundary; bot edge = lower-y boundary.
		# At each diagonal-vertical junction we use a miter join:
		#   the diagonal edge is extended to the vertical x (± hw_x) at y ± delta.
		# Ascending vertical: top edge is on the LEFT  (x - hw_x); bot on the RIGHT (x + hw_x).
		# Descending vertical: top edge is on the RIGHT (x + hw_x); bot on the LEFT  (x - hw_x).
		top_x <- x_ps
		top_y <- y0 - a + hw_y
		bot_x <- x_ps
		bot_y <- y0 - a - hw_y

		for (k in seq_len(n_periods)) {
			x0 <- x_ps + (k - 1L) * wavelength

			# One period: trough at x0, peak at x0+L/2, trough at x0+L.
			# Per-period top-edge points (6 new, starting from the in-array trough):
			#  miter entry/exit ascending-vertical left:  x0+dw-hw_x, y = y0±a/3 + delta
			#  peak:                                      x0+2dw,     y = y0+a+hw_y
			#  miter entry/exit descending-vertical right:x0+3dw+hw_x,y = y0±a/3 + delta
			#  next trough:                               x0+L,       y = y0-a+hw_y
			top_x <- c(
				top_x,
				x0 + d_wid - hw_x,
				x0 + d_wid - hw_x,
				x0 + 2 * d_wid,
				x0 + 3 * d_wid + hw_x,
				x0 + 3 * d_wid + hw_x,
				x0 + wavelength
			)
			top_y <- c(
				top_y,
				y0 - a / 3 + delta,
				y0 + a / 3 + delta,
				y0 + a + hw_y,
				y0 + a / 3 + delta,
				y0 - a / 3 + delta,
				y0 - a + hw_y
			)

			# Bot-edge points (vertical sections on the opposite sides):
			#  miter entry/exit ascending-vertical right: x0+dw+hw_x, y = y0±a/3 - delta
			#  peak:                                      x0+2dw,     y = y0+a-hw_y
			#  miter entry/exit descending-vertical left: x0+3dw-hw_x,y = y0±a/3 - delta
			#  next trough:                               x0+L,       y = y0-a-hw_y
			bot_x <- c(
				bot_x,
				x0 + d_wid + hw_x,
				x0 + d_wid + hw_x,
				x0 + 2 * d_wid,
				x0 + 3 * d_wid - hw_x,
				x0 + 3 * d_wid - hw_x,
				x0 + wavelength
			)
			bot_y <- c(
				bot_y,
				y0 - a / 3 - delta,
				y0 + a / 3 - delta,
				y0 + a - hw_y,
				y0 + a / 3 - delta,
				y0 - a / 3 - delta,
				y0 - a - hw_y
			)
		}

		x <- c(top_x, rev(bot_x))
		y <- c(top_y, rev(bot_y))
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

create_dovetail_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)

	# Dovetail per period: trough (2dw), ascending diagonal (1dw horiz, 2a vert),
	# crest (4dw), descending diagonal, trough (2dw).  dw = wavelength/6.
	#
	# Acute triangular corners: the diagonal outer/inner edges are NOT mitered
	# but extended to their natural intersection with the horizontal band boundary,
	# producing sharp pointed tips.  T_tip is the x-extension of each tip from the
	# nominal junction centre:
	#   T_tip = hw * (D + dw) / (2a),   D = sqrt(dw² + (2a)²)
	#
	# When T_tip >= dw the ascending and descending outer tips cross each other
	# (self-intersecting polygon), so we abort early.
	dw <- wavelength / 6
	D <- sqrt(dw^2 + (2 * a)^2)
	T_tip <- if (a == 0) 0 else hw * (D + dw) / (2 * a)

	if (T_tip >= dw) {
		abort(c(
			"Dovetailed wave: acute corner tips overlap between adjacent dovetails.",
			i = "Reduce `density`, increase `amplitude`, or increase `spacing`."
		))
	}

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L
		# Phase: shift x_ps back by λ/4 so the crest centre (x0+3dw, dw=λ/6) lands at λ/4.
		x_ps <- x_min - wavelength - wavelength / 4
		if (stagger_row) {
			x_ps <- x_ps - 0.5 * wavelength
		}
		n_periods <- ceiling((x_max - x_ps) / wavelength) + 2L

		# Top edge = higher-y boundary; bot edge = lower-y boundary.
		# Per period, 5 new vertices each (the period-start point is already in the array):
		#   top: inner ascending trough tip   (x0+2dw-T, y0-a+hw)
		#        outer ascending crest tip    (x0+ dw-T, y0+a+hw)
		#        outer descending crest tip   (x0+5dw+T, y0+a+hw)
		#        inner descending trough tip  (x0+4dw+T, y0-a+hw)
		#        period end                   (x0+6dw,   y0-a+hw)
		#   bot: outer ascending trough tip   (x0+2dw+T, y0-a-hw)
		#        inner ascending crest tip    (x0+ dw+T, y0+a-hw)
		#        inner descending crest tip   (x0+5dw-T, y0+a-hw)
		#        outer descending trough tip  (x0+4dw-T, y0-a-hw)
		#        period end                   (x0+6dw,   y0-a-hw)
		top_x <- x_ps
		top_y <- y0 - a + hw
		bot_x <- x_ps
		bot_y <- y0 - a - hw

		for (k in seq_len(n_periods)) {
			x0 <- x_ps + (k - 1L) * wavelength
			top_x <- c(
				top_x,
				x0 + 2 * dw - T_tip,
				x0 + dw - T_tip,
				x0 + 5 * dw + T_tip,
				x0 + 4 * dw + T_tip,
				x0 + 6 * dw
			)
			top_y <- c(top_y, y0 - a + hw, y0 + a + hw, y0 + a + hw, y0 - a + hw, y0 - a + hw)
			bot_x <- c(
				bot_x,
				x0 + 2 * dw + T_tip,
				x0 + dw + T_tip,
				x0 + 5 * dw - T_tip,
				x0 + 4 * dw - T_tip,
				x0 + 6 * dw
			)
			bot_y <- c(bot_y, y0 - a - hw, y0 + a - hw, y0 + a - hw, y0 - a - hw, y0 - a - hw)
		}

		x <- c(top_x, rev(bot_x))
		y <- c(top_y, rev(bot_y))
		xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
		m_mat <- as.matrix(as.data.frame(xy))
		list(rbind(m_mat, m_mat[1L, ]))
	})
	sf::st_multipolygon(l_waves)
}

create_nebuly_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)

	# Nebuly = potenty band with rounded corners: separate outer and inner boundary
	# curves, each a piecewise quadratic Bézier arc that goes from the midpoint of
	# one band segment through the corner vertex (control point) to the midpoint of
	# the next segment.  Building outer/inner separately (like potenty) prevents the
	# band-edge crossing that a simple vertical-offset centerline polygon can produce.
	#
	# dw = wavelength/4; potenty centerline corners P1..P8 relative to (x0,y0):
	#   P1=(3dw,-a), P2=(3dw,0), P3=(2dw,0), P4=(2dw,+a),
	#   P5=(5dw,+a), P6=(5dw,0), P7=(4dw,0), P8=(4dw,-a)
	# Outer (left/CCW) corner offsets T1..T8:
	#   T1=(3dw-hw,-a+hw), T2=(3dw-hw,-hw),  T3=(2dw-hw,-hw),  T4=(2dw-hw,a+hw)
	#   T5=(5dw+hw,a+hw),  T6=(5dw+hw,-hw),  T7=(4dw+hw,-hw),  T8=(4dw+hw,-a+hw)
	# Inner (right/CW) corner offsets B1..B8:
	#   B1=(3dw+hw,-a-hw), B2=(3dw+hw,hw),   B3=(2dw+hw,hw),   B4=(2dw+hw,a-hw)
	#   B5=(5dw-hw,a-hw),  B6=(5dw-hw,hw),   B7=(4dw-hw,hw),   B8=(4dw-hw,-a-hw)
	dw <- wavelength / 4
	if (hw > dw / 2) {
		abort(c(
			"Nebuly wave: band corners overlap between adjacent steps.",
			i = "Reduce `density` or increase `spacing`/`frequency`."
		))
	}
	tc_dx <- c(
		3 * dw - hw,
		3 * dw - hw,
		2 * dw - hw,
		2 * dw - hw,
		5 * dw + hw,
		5 * dw + hw,
		4 * dw + hw,
		4 * dw + hw
	)
	tc_dy <- c(-a + hw, -hw, -hw, a + hw, a + hw, -hw, -hw, -a + hw)
	bc_dx <- c(
		3 * dw + hw,
		3 * dw + hw,
		2 * dw + hw,
		2 * dw + hw,
		5 * dw - hw,
		5 * dw - hw,
		4 * dw - hw,
		4 * dw - hw
	)
	bc_dy <- c(-a - hw, hw, hw, a - hw, a - hw, hw, hw, -a - hw)

	# Arc endpoints = midpoints of consecutive outer/inner band corner vertices.
	# MT0_next (index 9) has the same x-offset as MT0 shifted by one wavelength:
	# MT0..MT8 for outer, MB0..MB8 for inner.
	mt_dx <- c(
		1.5 * dw,
		3 * dw - hw,
		2.5 * dw - hw,
		2 * dw - hw,
		3.5 * dw,
		5 * dw + hw,
		4.5 * dw + hw,
		4 * dw + hw,
		5.5 * dw
	)
	mt_dy <- c(-a + hw, -a / 2, -hw, a / 2, a + hw, a / 2, -hw, -a / 2, -a + hw)
	mb_dx <- c(
		1.5 * dw,
		3 * dw + hw,
		2.5 * dw + hw,
		2 * dw + hw,
		3.5 * dw,
		5 * dw - hw,
		4.5 * dw - hw,
		4 * dw - hw,
		5.5 * dw
	)
	mb_dy <- c(-a - hw, -a / 2, hw, a / 2, a - hw, a / 2, hw, -a / 2, -a - hw)
	n_seg <- 8L

	n_s <- 12L
	tv <- seq(0, 1, length.out = n_s + 1L)[seq_len(n_s)]
	q0 <- (1 - tv)^2
	q1 <- 2 * tv * (1 - tv)
	q2 <- tv^2

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L

		# Phase: shift x_ps back by 5λ/8 so the crest centre (x0+3.5dw, dw=λ/4) lands at λ/4.
		x_ps <- x_min - wavelength - 5 * wavelength / 8
		if (stagger_row) {
			x_ps <- x_ps - 0.5 * wavelength
		}
		n_periods <- ceiling((x_max - x_ps) / wavelength) + 2L

		n_pts <- n_periods * n_seg * n_s + 2L
		top_x <- numeric(n_pts)
		top_y <- numeric(n_pts)
		bot_x <- numeric(n_pts)
		bot_y <- numeric(n_pts)

		# Lead-in at x_ps (left-end cap)
		top_x[1L] <- x_ps
		top_y[1L] <- y0 + mt_dy[1L]
		bot_x[1L] <- x_ps
		bot_y[1L] <- y0 + mb_dy[1L]
		pos <- 2L

		for (k in seq_len(n_periods)) {
			x0 <- x_ps + (k - 1L) * wavelength
			for (seg in seq_len(n_seg)) {
				# Outer arc: MT_{seg} → TC_{seg} → MT_{seg+1}
				ax <- x0 + mt_dx[seg]
				ay <- y0 + mt_dy[seg]
				bx <- x0 + tc_dx[seg]
				by <- y0 + tc_dy[seg]
				if (seg < n_seg) {
					cx <- x0 + mt_dx[seg + 1L]
					cy <- y0 + mt_dy[seg + 1L]
				} else {
					cx <- x0 + mt_dx[9L]
					cy <- y0 + mt_dy[9L]
				}
				top_x[pos:(pos + n_s - 1L)] <- q0 * ax + q1 * bx + q2 * cx
				top_y[pos:(pos + n_s - 1L)] <- q0 * ay + q1 * by + q2 * cy
				# Inner arc: MB_{seg} → BC_{seg} → MB_{seg+1}
				ax <- x0 + mb_dx[seg]
				ay <- y0 + mb_dy[seg]
				bx <- x0 + bc_dx[seg]
				by <- y0 + bc_dy[seg]
				if (seg < n_seg) {
					cx <- x0 + mb_dx[seg + 1L]
					cy <- y0 + mb_dy[seg + 1L]
				} else {
					cx <- x0 + mb_dx[9L]
					cy <- y0 + mb_dy[9L]
				}
				bot_x[pos:(pos + n_s - 1L)] <- q0 * ax + q1 * bx + q2 * cx
				bot_y[pos:(pos + n_s - 1L)] <- q0 * ay + q1 * by + q2 * cy
				pos <- pos + n_s
			}
		}
		# Right-end cap
		top_x[n_pts] <- x_ps + n_periods * wavelength + mt_dx[1L]
		top_y[n_pts] <- y0 + mt_dy[1L]
		bot_x[n_pts] <- x_ps + n_periods * wavelength + mb_dx[1L]
		bot_y[n_pts] <- y0 + mb_dy[1L]

		x <- c(top_x, rev(bot_x))
		y <- c(top_y, rev(bot_y))
		xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
		m_mat <- as.matrix(as.data.frame(xy))
		list(rbind(m_mat, m_mat[1L, ]))
	})
	sf::st_multipolygon(l_waves)
}

create_potenty_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)

	# One period: right 3dw, up a, left dw, up a, right 3dw, down a, left dw, down a.
	# Total advance = 3dw - dw + 3dw - dw = 4dw = wavelength, so dw = wavelength/4.
	# The top plateau overshoots by dw (landing at x0+5dw = x0+wl+dw), which is fine
	# because the per-row polygon is clipped to the boundary by sf::st_intersection.
	dw <- wavelength / 4
	if (hw > dw / 2) {
		abort(c(
			"Potenty wave: band corners overlap between adjacent steps.",
			i = "Reduce `density` or increase `spacing`/`frequency`."
		))
	}

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L

		# Phase: shift x_ps back by 5λ/8 so the crest centre (x0+3.5dw, dw=λ/4) lands at λ/4.
		x_ps <- x_min - wavelength - 5 * wavelength / 8
		if (stagger_row) {
			x_ps <- x_ps - 0.5 * wavelength
		}
		n_periods <- ceiling((x_max - x_ps) / wavelength) + 2L

		# 9 vertices per period (P1-P8 + next-period anchor) plus initial and trailing.
		n_pts <- 9L * n_periods + 2L
		top_x <- numeric(n_pts)
		top_y <- numeric(n_pts)
		bot_x <- numeric(n_pts)
		bot_y <- numeric(n_pts)
		top_x[1L] <- x_ps
		top_y[1L] <- y0 - a + hw
		bot_x[1L] <- x_ps
		bot_y[1L] <- y0 - a - hw

		for (k in seq_len(n_periods)) {
			x0 <- x_ps + (k - 1L) * wavelength
			i <- (k - 1L) * 9L + 2L

			# Left boundary: outer corners at P1, P2, P7, P8; inner at P3, P4, P5, P6.
			# Each corner = intersection of the two adjacent offset lines (left side of travel).
			top_x[i:(i + 8L)] <- c(
				x0 + 3 * dw - hw, # outer P1 (→ to ↑): left of → at y0-a, left of ↑ at x0+3dw
				x0 + 3 * dw - hw, # outer P2 (↑ to ←): left of ↑, left of ← at y0 — same x, vertical
				x0 + 2 * dw - hw, # inner P3 (← to ↑): left of ← at y0, left of ↑ at x0+2dw
				x0 + 2 * dw - hw, # inner P4 (↑ to →): left of ↑, left of → at y0+a — same x, vertical
				x0 + 5 * dw + hw, # inner P5 (→ to ↓): left of → at y0+a, left of ↓ at x0+5dw
				x0 + 5 * dw + hw, # inner P6 (↓ to ←): left of ↓, left of ← at y0 — same x, vertical
				x0 + 4 * dw + hw, # outer P7 (← to ↓): left of ← at y0, left of ↓ at x0+4dw
				x0 + 4 * dw + hw, # outer P8 (↓ to →): left of ↓, left of → at y0-a — same x, vertical
				x0 + 4 * dw # L1 of next period
			)
			top_y[i:(i + 8L)] <- c(
				y0 - a + hw, # P1
				y0 - hw, # P2
				y0 - hw, # P3
				y0 + a + hw, # P4
				y0 + a + hw, # P5
				y0 - hw, # P6
				y0 - hw, # P7
				y0 - a + hw, # P8
				y0 - a + hw # L1 next
			)

			# Right boundary: inner corners at P1, P2, P7, P8; outer at P3, P4, P5, P6.
			bot_x[i:(i + 8L)] <- c(
				x0 + 3 * dw + hw, # inner P1
				x0 + 3 * dw + hw, # inner P2 — same x, vertical
				x0 + 2 * dw + hw, # outer P3
				x0 + 2 * dw + hw, # outer P4 — same x, vertical
				x0 + 5 * dw - hw, # outer P5
				x0 + 5 * dw - hw, # outer P6 — same x, vertical
				x0 + 4 * dw - hw, # inner P7
				x0 + 4 * dw - hw, # inner P8 — same x, vertical
				x0 + 4 * dw # R1 of next period
			)
			bot_y[i:(i + 8L)] <- c(
				y0 - a - hw, # P1
				y0 + hw, # P2
				y0 + hw, # P3
				y0 + a - hw, # P4
				y0 + a - hw, # P5
				y0 + hw, # P6
				y0 + hw, # P7
				y0 - a - hw, # P8
				y0 - a - hw # R1 next
			)
		}

		top_x[n_pts] <- x_max
		top_y[n_pts] <- top_y[n_pts - 1L]
		bot_x[n_pts] <- x_max
		bot_y[n_pts] <- bot_y[n_pts - 1L]

		x <- c(top_x, rev(bot_x))
		y <- c(top_y, rev(bot_y))
		polygon_ring(x, y, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

create_raguly_waves_sf <- function(params, grid_xy, vpm, i_par, n_par) {
	hw <- 0.5 * grid_xy$v_spacing * params$pattern_density
	a <- params$pattern_amplitude
	wavelength <- grid_xy$h_spacing
	x_min <- grid_xy$x_min
	x_max <- grid_xy$x_max
	n_y <- length(grid_xy$y)
	indices_y <- seq(from = i_par, to = n_y, by = n_par)
	# reverse = FALSE: diagonal rises rightward (default)
	# reverse = TRUE:  diagonal rises leftward (mirror image)
	up <- !isTRUE(params$pattern_reverse)

	# Raguly per period (wavelength = 4*dw):
	#   up (reverse=FALSE): S1 low flat (+2dw), S2 diagonal-up-right (+2dw), S3 high flat (+2dw),
	#                       S4 diagonal-down-left (-2dw). Net +4dw per period.
	#   down (reverse=TRUE): S1 low flat (+2dw), S2 diagonal-up-left (-2dw), S3 high flat (+2dw),
	#                        S4 diagonal-down-right (+2dw). Net +4dw per period.
	#
	# Built as a single polygon per wave row.  The backward diagonal (S4 for raguly_up,
	# S2 for raguly_down) produces an acute-angle corner where it meets the adjacent
	# horizontal flat.  Two tip offsets are used:
	#   mg     = hw*(R1-dw)/a  — miter for forward-diagonal junctions (S1/S2, S2/S3)
	#   T_tip  = hw*(R1+dw)/a  — tip extension for backward-diagonal junctions (S3/S4, S4/S1)
	# Self-intersection check: hw >= max_hw = a*dw/R1.
	dw <- wavelength / 4
	if (a > 0) {
		R1 <- sqrt(dw^2 + a^2)
		max_hw <- a * dw / R1
		if (hw >= max_hw) {
			abort(c(
				"Raguly wave: outer diagonal overlaps the inner horizontal boundary.",
				i = "Reduce `density`, increase `amplitude`, or increase `spacing`/`frequency`."
			))
		}
		mg <- hw * (R1 - dw) / a
		T_tip <- hw * (R1 + dw) / a
	} else {
		mg <- 0
		T_tip <- 0
	}

	l_waves <- lapply(seq_along(indices_y), function(j) {
		y0 <- grid_xy$y[indices_y[j]]
		stagger_row <- isTRUE(params$pattern_stagger) && indices_y[j] %% 2L == 0L
		x_ps <- x_min - wavelength
		if (stagger_row) {
			x_ps <- x_ps - 0.5 * wavelength
		}
		n_periods <- ceiling((x_max - x_ps) / wavelength) + 2L

		yL <- y0 - a + hw # outer low level
		yH <- y0 + a + hw # outer high level
		yL2 <- y0 - a - hw # inner low level
		yH2 <- y0 + a - hw # inner high level

		# 4 vertices per period × 2 passes (forward outer + backward inner) + 2 endpoints.
		n_verts <- 8L * n_periods + 2L
		vx <- numeric(n_verts)
		vy <- numeric(n_verts)

		if (up) {
			# raguly_up: S4 is the backward diagonal (down-left).
			# Forward pass traverses the outer boundary: S1_outer→S2_outer→S3_outer→S4_outer,
			#   where S4_outer produces a sharp acute corner at J3 (x0+6dw+T_tip, yH).
			# Backward pass traverses the inner boundary: S4_inner→S3_inner→S2_inner→S1_inner.
			# Vertices per period forward: J1=(x0+2dw-mg,yL), J2=(x0+4dw-mg,yH),
			#   J3=(x0+6dw+T_tip,yH) [acute corner], J4=(x0+4dw+T_tip,yL).
			# Vertices per period backward: J3i=(x0+6dw-T_tip,yH2), J2i=(x0+4dw+mg,yH2),
			#   J1i=(x0+2dw+mg,yL2), J0i=(x0-T_tip,yL2).
			vx[1L] <- x_ps + T_tip
			vy[1L] <- yL

			for (k in seq_len(n_periods)) {
				x0 <- x_ps + (k - 1L) * wavelength
				i <- (k - 1L) * 4L + 2L
				vx[i:(i + 3L)] <- c(
					x0 + 2 * dw - mg,
					x0 + 4 * dw - mg,
					x0 + 6 * dw + T_tip,
					x0 + 4 * dw + T_tip
				)
				vy[i:(i + 3L)] <- c(yL, yH, yH, yL)
			}

			vx[4L * n_periods + 2L] <- x_ps + (n_periods - 1L) * wavelength + 4 * dw - T_tip
			vy[4L * n_periods + 2L] <- yL2

			for (j in seq_len(n_periods)) {
				x0 <- x_ps + (n_periods - j) * wavelength
				i <- 4L * n_periods + 4L * j - 1L
				vx[i:(i + 3L)] <- c(
					x0 + 6 * dw - T_tip,
					x0 + 4 * dw + mg,
					x0 + 2 * dw + mg,
					x0 - T_tip
				)
				vy[i:(i + 3L)] <- c(yH2, yH2, yL2, yL2)
			}
		} else {
			# raguly_down: S2 is the backward diagonal (up-left).
			# Forward pass traverses the outer boundary: S1_outer→S2_outer→S3_outer→S4_outer,
			#   where S2_outer produces sharp acute corners at J1 (x0+2dw-T_tip, yL) and
			#   J2 (x0-T_tip, yH).
			# Backward pass traverses the inner boundary: S4_inner→S3_inner→S2_inner→S1_inner.
			# Vertices per period forward: J1=(x0+2dw-T_tip,yL) [acute], J2=(x0-T_tip,yH) [acute],
			#   J3=(x0+2dw+mg,yH), J4=(x0+4dw+mg,yL).
			# Vertices per period backward: J3i=(x0+2dw-mg,yH2), J2i=(x0+T_tip,yH2),
			#   J1i=(x0+2dw+T_tip,yL2), J0i=(x0-mg,yL2).
			vx[1L] <- x_ps + mg
			vy[1L] <- yL

			for (k in seq_len(n_periods)) {
				x0 <- x_ps + (k - 1L) * wavelength
				i <- (k - 1L) * 4L + 2L
				vx[i:(i + 3L)] <- c(
					x0 + 2 * dw - T_tip,
					x0 - T_tip,
					x0 + 2 * dw + mg,
					x0 + 4 * dw + mg
				)
				vy[i:(i + 3L)] <- c(yL, yH, yH, yL)
			}

			vx[4L * n_periods + 2L] <- x_ps + (n_periods - 1L) * wavelength + 4 * dw - mg
			vy[4L * n_periods + 2L] <- yL2

			for (j in seq_len(n_periods)) {
				x0 <- x_ps + (n_periods - j) * wavelength
				i <- 4L * n_periods + 4L * j - 1L
				vx[i:(i + 3L)] <- c(x0 + 2 * dw - mg, x0 + T_tip, x0 + 2 * dw + T_tip, x0 - mg)
				vy[i:(i + 3L)] <- c(yH2, yH2, yL2, yL2)
			}
		}

		polygon_ring(vx, vy, params$pattern_angle, vpm)
	})
	sf::st_multipolygon(l_waves)
}

# # build sf multipolygon 'rect' for each grid_xy$y value
# create_h_stripes_sf <- function(params, grid_xy, vpm) {
#     halfwidth <- 0.5 * grid_xy$v_spacing * params$pattern_density
#     l_rects <- lapply(grid_xy$y,
#                       function(y0) {
#                           x <- c(grid_xy$x_min, grid_xy$x_min, grid_xy$x_max, grid_xy$x_max)
#                           y <- y0 + c(-1, 1, 1, -1) * halfwidth
#                           xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
#                           m <- as.matrix(as.data.frame(xy))
#                           list(rbind(m, m[1,]))
#                       })
#     sf::st_multipolygon(l_rects)
# }
#
# # build sf multipolygon 'rect' for each grid_xy$x value
# create_v_stripes_sf <- function(params, grid_xy, vpm) {
#     halfwidth <- 0.5 * grid_xy$h_spacing * params$pattern_density
#     l_rects <- lapply(grid_xy$x,
#                       function(x0) {
#                           x <- x0 + c(-1, 1, 1, -1) * halfwidth
#                           y <- c(grid_xy$y_min, grid_xy$y_min, grid_xy$y_max, grid_xy$y_max)
#                           xy <- rotate_xy(x, y, params$pattern_angle, vpm$x, vpm$y)
#                           m <- as.matrix(as.data.frame(xy))
#                           list(rbind(m, m[1,]))
#                       })
#     sf::st_multipolygon(l_rects)
# }
