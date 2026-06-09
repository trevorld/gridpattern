#' Line patterned grobs
#'
#' `grid.pattern_line()` draws a line pattern onto the graphic device.
#' Unlike [grid.pattern_stripe()] which fills bands with solid colour,
#' this pattern draws stroked lines using the device's native line rendering,
#' enabling all of R's built-in `linetype` values (including `"dotdash"`,
#' `"twodash"`, and custom line types specified as hex strings per `?par`).
#'
#' @inheritParams grid.pattern_circle
#' @inheritParams alphaMaskGrob
#' @param lineend Line end style, one of `"round"` (default), `"butt"`, or `"square"`.
#' @param stagger If `TRUE`, alternate lines are shifted by half the dash period so that
#'   dashes of adjacent lines interleave.
#'   Computed from `linetype` and `linewidth` per `?par`.  Default `FALSE`.
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' if (capabilities("png") || guess_has_R4.1_features("masks")) {
#'   grid::grid.newpage()
#'   grid.pattern_line(x_hex, y_hex, colour = "black", linetype = "dotdash",
#'                     angle = 45, spacing = 0.1, stagger = TRUE)
#' }
#'
#' # more intricate dashed lines are possible with hex strings
#' if (capabilities("png") || guess_has_R4.1_features("masks")) {
#'   grid::grid.newpage()
#'   grid.pattern_line(x_hex, y_hex, gp = grid::gpar(col = "darkred", lty = "23632E"))
#' }
#' @seealso [grid.pattern_stripe()] for filled bands, [grid.pattern_crosshatch()] for two sets of lines.
#' @export
grid.pattern_line <- function(
	x = c(0, 0, 1, 1),
	y = c(1, 0, 0, 1),
	id = 1L,
	...,
	colour = gp$col %||% "grey20",
	angle = 30,
	spacing = 0.05,
	xoffset = 0,
	yoffset = 0,
	units = "snpc",
	alpha = gp$alpha %||% NA_real_,
	lineend = gp$lineend %||% "round",
	linetype = gp$lty %||% 1,
	linewidth = size %||% gp$lwd %||% 1,
	size = NULL,
	stagger = FALSE,
	use_R4.1_masks = getOption(
		"ggpattern_use_R4.1_masks",
		getOption("ggpattern_use_R4.1_features")
	),
	png_device = NULL,
	res = getOption("ggpattern_res", 72),
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
		"line",
		x,
		y,
		id,
		colour = colour,
		angle = angle,
		spacing = spacing,
		xoffset = xoffset,
		yoffset = yoffset,
		units = units,
		alpha = alpha,
		linetype = linetype,
		linewidth = linewidth,
		lineend = lineend,
		stagger = stagger,
		use_R4.1_masks = use_R4.1_masks,
		png_device = png_device,
		res = res,
		default.units = default.units,
		name = name,
		gp = gp,
		draw = draw,
		vp = vp
	)
}

# blank or solid lines return NULL
lty_pattern_str <- function(lty) {
	if (is.numeric(lty)) {
		lty <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")[lty + 1L]
	}
	switch(
		lty,
		blank = ,
		solid = NULL,
		dashed = "44",
		dotted = "13",
		dotdash = "1343",
		longdash = "73",
		twodash = "2262",
		lty
	)
}

# blank or solid lines return 0L
lty_period_sum <- function(lty) {
	pat <- lty_pattern_str(lty)
	if (is.null(pat)) {
		return(0L)
	}
	sum(strtoi(strsplit(pat, "")[[1L]], base = 16L))
}

# Expand one row of a staggered pattern into explicit solid on-segments.
# phase_offset_lwd: phase in lwd units (may be fractional, e.g. 5.5 for dotdash half-period).
# Returns list(x0, x1, y) — equal-length vectors.
expand_lty_row <- function(lty, lwd, x_min, x_max, y_row, phase_offset_lwd) {
	pat <- lty_pattern_str(lty)
	if (is.null(pat)) {
		if (lty_period_sum(lty) == 0L) {
			return(list(x0 = x_min, x1 = x_max, y = y_row))
		}
		return(list(x0 = numeric(0L), x1 = numeric(0L), y = numeric(0L)))
	}
	units_lwd <- strtoi(strsplit(pat, "")[[1L]], base = 16L)
	n <- length(units_lwd)
	period_lwd <- sum(units_lwd)
	phase <- phase_offset_lwd %% period_lwd
	cumul_lwd <- c(0, cumsum(units_lwd))
	seg_idx <- findInterval(phase, cumul_lwd[-length(cumul_lwd)])
	offset_in_seg <- phase - cumul_lwd[seg_idx]
	x0s <- numeric(0L)
	x1s <- numeric(0L)
	x_cur <- x_min
	first <- TRUE
	i <- seg_idx
	while (x_cur < x_max) {
		rem_lwd <- if (first) {
			first <- FALSE
			units_lwd[i] - offset_in_seg
		} else {
			units_lwd[i]
		}
		if (rem_lwd > 0) {
			x_end <- min(x_cur + rem_lwd * lwd, x_max)
			if (i %% 2L == 1L) {
				x0s <- c(x0s, x_cur)
				x1s <- c(x1s, x_end)
			}
			x_cur <- x_end
		}
		i <- i %% n + 1L
	}
	list(x0 = x0s, x1 = x1s, y = rep(y_row, length(x0s)))
}

# Build the segments grob for one set of lines (angle/lty/stagger already set in params).
# params must already be unit-converted (via convert_params_units).
# vpm must be from get_vp_measurements(default.units).
create_line_maskee <- function(params, vpm, default.units = "bigpts") {
	grid_xy <- get_xy_grid(params, vpm)

	col <- update_alpha(params$pattern_colour, params$pattern_alpha)
	lwd <- params$pattern_linewidth * .pt
	lty <- params$pattern_linetype
	lineend <- params$pattern_lineend
	gp <- gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)

	x0 <- rep(grid_xy$x_min, length(grid_xy$y))
	x1 <- rep(grid_xy$x_max, length(grid_xy$y))
	seg_y <- grid_xy$y

	# Stagger alternating lines by half the dash period so that marks of
	# adjacent lines interleave (heraldic convention).  Explicit solid
	# sub-segments are used so the phase is device-independent (relying on a
	# segment start outside the viewport breaks dash phase on some devices).
	if (isTRUE(params$pattern_stagger)) {
		half_period_lwd <- lty_period_sum(lty) / 2
		if (half_period_lwd > 0) {
			segs <- lapply(seq_along(grid_xy$y), function(i) {
				phase <- if (i %% 2L == 0L) half_period_lwd else 0
				expand_lty_row(lty, lwd, grid_xy$x_min, grid_xy$x_max, grid_xy$y[i], phase)
			})
			x0_list <- lapply(segs, `[[`, "x0")
			x0 <- unlist(x0_list, use.names = FALSE)
			x1 <- unlist(lapply(segs, `[[`, "x1"), use.names = FALSE)
			seg_y <- unlist(lapply(segs, `[[`, "y"), use.names = FALSE)
			# Expand per-line gp values to per-sub-segment so recycling
			# matches the non-stagger path (one gp value per line, not per dash).
			n_sub <- lengths(x0_list)
			line_idx <- rep(seq_along(grid_xy$y), n_sub)
			recycle_to_lines <- function(v) v[(line_idx - 1L) %% length(v) + 1L]
			gp <- gpar(
				col = recycle_to_lines(col),
				lwd = recycle_to_lines(lwd),
				lineend = recycle_to_lines(lineend),
				lty = "solid"
			)
		}
	}

	xy0 <- rotate_xy(x0, seg_y, params$pattern_angle, vpm$x, vpm$y)
	xy1 <- rotate_xy(x1, seg_y, params$pattern_angle, vpm$x, vpm$y)

	segmentsGrob(xy0$x, xy0$y, xy1$x, xy1$y, default.units = default.units, gp = gp)
}

create_pattern_line <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	default.units <- "bigpts"
	boundary_df <- convert_polygon_df_units(boundary_df, default.units)
	params <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)

	maskee <- create_line_maskee(params, vpm, default.units)
	masker <- convert_polygon_df_to_polygon_grob(
		boundary_df,
		default.units = default.units,
		gp = gpar(fill = "white", col = NA, lwd = 0)
	)
	alphaMaskGrob(
		maskee,
		masker,
		use_R4.1_masks = params$pattern_use_R4.1_masks,
		png_device = params$pattern_png_device,
		res = params$pattern_res,
		name = "line"
	)
}
