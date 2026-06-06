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
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' if (capabilities("png") || guess_has_R4.1_features("masks")) {
#'   grid::grid.newpage()
#'   grid.pattern_line(x_hex, y_hex, colour = "black", angle = 0, spacing = 0.1)
#' }
#'
#' if (capabilities("png") || guess_has_R4.1_features("masks")) {
#'   grid::grid.newpage()
#'   grid.pattern_line(x_hex, y_hex, colour = "black", linetype = "dotdash",
#'                     angle = 45, spacing = 0.1)
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

create_pattern_line <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	default.units <- "bigpts"
	boundary_df <- convert_polygon_df_units(boundary_df, default.units)
	params <- convert_params_units(params, default.units)
	vpm <- get_vp_measurements(default.units)
	grid_xy <- get_xy_grid(params, vpm)

	col <- update_alpha(params$pattern_colour, params$pattern_alpha)
	lwd <- params$pattern_linewidth * .pt
	lty <- params$pattern_linetype
	lineend <- params$pattern_lineend
	gp <- gpar(col = col, lwd = lwd, lty = lty, lineend = lineend)

	x0 <- rep(grid_xy$x_min, length(grid_xy$y))
	x1 <- rep(grid_xy$x_max, length(grid_xy$y))
	xy0 <- rotate_xy(x0, grid_xy$y, params$pattern_angle, vpm$x, vpm$y)
	xy1 <- rotate_xy(x1, grid_xy$y, params$pattern_angle, vpm$x, vpm$y)

	maskee <- segmentsGrob(
		xy0$x,
		xy0$y,
		xy1$x,
		xy1$y,
		default.units = default.units,
		gp = gp
	)
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
