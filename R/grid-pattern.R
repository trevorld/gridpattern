#' Create patterned grobs by pattern name
#'
#' `grid.pattern()` draws patterned shapes onto the graphic device.
#' `patternGrob()` returns the grid grob objects.
#' `names_pattern` is a character vector of builtin patterns.
#'
#' Here is a list of the various patterns supported:
#'
#' \describe{
#' \item{ambient}{Noise array patterns onto the graphic device powered by the `ambient` package.
#'                See [grid.pattern_ambient()] for more information.}
#' \item{aRtsy}{Patterns powered by the `aRtsy` package.
#'              See [grid.pattern_aRtsy()] for more information.}
#' \item{circle}{Circle geometry patterns.
#'               See [grid.pattern_circle()] for more information.}
#' \item{crosshatch}{Crosshatch geometry patterns.
#'                   See [grid.pattern_crosshatch()] for more information.}
#' \item{gradient}{Gradient array/geometry patterns.
#'                 See [grid.pattern_gradient()] for more information.}
#' \item{hatch}{Heraldic hatching patterns.
#'              See [grid.pattern_hatch()] for more information.}
#' \item{image}{Image array patterns.
#'              See [grid.pattern_image()] for more information.}
#' \item{line}{Line geometry patterns.
#'             See [grid.pattern_line()] for more information.}
#' \item{magick}{`imagemagick` array patterns.
#'               See [grid.pattern_magick()] for more information.}
#' \item{none}{Does nothing.
#'             See [grid::grid.null()] for more information.}
#' \item{pch}{Plotting character geometry patterns.
#'            See [grid.pattern_pch()] for more information.}
#' \item{placeholder}{Placeholder image array patterns.
#'                    See [grid.pattern_placeholder()] for more information.}
#' \item{plasma}{Plasma array patterns.
#'               See [grid.pattern_plasma()] for more information.}
#' \item{polygon_tiling}{Polygon tiling patterns.
#'                        See [grid.pattern_polygon_tiling()] for more information.}
#' \item{regular_polygon}{Regular polygon patterns.
#'                        See [grid.pattern_regular_polygon()] for more information.}
#' \item{rose}{Rose array/geometry patterns.
#'             See [grid.pattern_rose()] for more information.}
#' \item{stripe}{Stripe geometry patterns.
#'               See [grid.pattern_stripe()] for more information.}
#' \item{text}{Text array/geometry patterns.
#'             See [grid.pattern_text()] for more information.}
#' \item{wave}{Wave geometry patterns.
#'               See [grid.pattern_wave()] for more information.}
#' \item{weave}{Weave geometry patterns.
#'               See [grid.pattern_weave()] for more information.}
#' \item{Custom geometry-based patterns}{See [register_pattern()] and the \dQuote{Developing Patterns} vignette `vignette("developing-patterns", package = "gridpattern")` for more information.}
#' \item{Custom array-based patterns}{See [register_pattern()] and the \dQuote{Developing Patterns} vignette `vignette("developing-patterns", package = "gridpattern")` for more information.}
#' }
#'
#' @inheritParams grid::polygonGrob
#' @param pattern Name of pattern.  See Details section for a list of supported patterns.
#'                Builtin patterns may also be referred to by their qualified name e.g. `"gridpattern::stripe"`
#'                and custom patterns registered by packages by `"pkg::name"`.
#'                See [register_pattern()] for more information.
#' @param x A numeric vector or unit object specifying x-locations of the pattern boundary.
#' @param y A numeric vector or unit object specifying y-locations of the pattern boundary.
#' @param id A numeric vector used to separate locations in x, y into multiple boundaries.
#'           All locations within the same `id` belong to the same boundary.
#' @param ... Pattern parameters.
#' @param legend Whether this is intended to be drawn in a legend or not.
#' @param prefix Prefix to prepend to the name of each of the pattern parameters in `...`.
#'               For compatibility with `ggpattern` most underlying functions assume parameters beginning with `pattern_`.
#' @param default.units A string indicating the default units to use if `x` or `y`
#'                      are only given as numeric vectors.
#' @return A grid grob object (invisibly in the case of `grid.pattern()`).
#'         If `draw` is `TRUE` then `grid.pattern()` also draws to the graphic device as a side effect.
#' @examples
#'  print(names_pattern)
#'  \donttest{# May take more than 5 seconds on CRAN servers
#'  x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'  y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'
#'  # geometry-based patterns
#'  # 'stripe' pattern
#'  grid::grid.newpage()
#'  grid.pattern("stripe", x_hex, y_hex,
#'               colour="black", fill=c("yellow", "blue"), density = 0.5)
#'
#'  # Can alternatively use "gpar()" to specify colour and line attributes
#'  grid::grid.newpage()
#'  grid.pattern("stripe", x_hex, y_hex,
#'               gp = grid::gpar(col="blue", fill="red", lwd=2))
#'
#'  # 'weave' pattern
#'  grid::grid.newpage()
#'  grid.pattern("weave", x_hex, y_hex, type = "satin",
#'               colour = "black", fill = "lightblue", fill2 =  "yellow",
#'               density = 0.3)
#'
#'  # 'regular_polygon' pattern
#'  grid::grid.newpage()
#'  grid.pattern_regular_polygon(x_hex, y_hex, colour = "black",
#'                               fill = c("blue", "yellow", "red"),
#'                               shape = c("convex4", "star8", "circle"),
#'                               density = c(0.45, 0.42, 0.4),
#'                               spacing = 0.08, angle = 0)
#'
#'  # can be used to achieve a variety of 'tiling' effects
#'  grid::grid.newpage()
#'  grid.pattern_regular_polygon(x_hex, y_hex, color = "transparent",
#'                               fill = c("white", "grey", "black"),
#'                               density = 1.0, spacing = 0.1,
#'                               shape = "convex6", grid = "hex")
#'  if (suppressPackageStartupMessages(requireNamespace("magick", quietly = TRUE))) {
#'    # array-based patterns
#'    # 'image' pattern
#'    logo_filename <- system.file("img", "Rlogo.png" , package="png")
#'    grid::grid.newpage()
#'    grid.pattern("image", x_hex, y_hex, filename=logo_filename, type="fit")
#'  }
#'  if (suppressPackageStartupMessages(requireNamespace("magick", quietly = TRUE))) {
#'    # 'plasma' pattern
#'    grid::grid.newpage()
#'    grid.pattern("plasma", x_hex, y_hex, fill="green")
#'  }
#'  }
#' @seealso \url{https://coolbutuseless.github.io/package/ggpattern/index.html}
#'          for more details on the `ggpattern` package.
#' @export
grid.pattern <- function(
	pattern = "stripe",
	x = c(0, 0, 1, 1),
	y = c(1, 0, 0, 1),
	id = 1L,
	...,
	legend = FALSE,
	prefix = "pattern_",
	default.units = "npc",
	name = NULL,
	gp = gpar(),
	draw = TRUE,
	vp = NULL
) {
	grob <- patternGrob(
		pattern,
		x,
		y,
		id,
		...,
		legend = legend,
		prefix = prefix,
		default.units = default.units,
		name = name,
		gp = gp,
		vp = vp
	)
	if (draw) {
		# throw any pattern resolution error before drawing
		# (errors while drawing leave the graphics device locked)
		if (is.null(grob$entry)) {
			resolve_pattern(pattern)
		}
		grid.draw(grob)
	}
	invisible(grob)
}

#' @rdname grid.pattern
#' @export
names_pattern <- c(
	"ambient",
	"aRtsy",
	"circle",
	"crosshatch",
	"fill",
	"gradient",
	"hatch",
	"image",
	"line",
	"magick",
	"none",
	"pch",
	"placeholder",
	"plasma",
	"polygon_tiling",
	"regular_polygon",
	"rose",
	"stripe",
	"text",
	"wave",
	"weave"
)

#' @rdname grid.pattern
#' @export
patternGrob <- function(
	pattern = "stripe",
	x = c(0, 0, 1, 1),
	y = c(1, 0, 0, 1),
	id = 1L,
	...,
	legend = FALSE,
	prefix = "pattern_",
	default.units = "npc",
	name = NULL,
	gp = gpar(),
	draw = TRUE,
	vp = NULL
) {
	args <- prefix_args(list(...), prefix)
	# cache pattern resolved at creation (if possible) so redraws (e.g. resizing the device)
	# still work if the pattern is later unregistered or masked
	entry <- tryCatch(resolve_pattern(pattern), error = function(e) NULL)
	if (!inherits(x, "unit")) {
		x <- unit(x, default.units)
	}
	if (!inherits(y, "unit")) {
		y <- unit(y, default.units)
	}

	gTree(
		pattern = pattern,
		x = x,
		y = y,
		id = id,
		args = args,
		entry = entry,
		entry_pattern = if (!is.null(entry)) pattern,
		legend = legend,
		name = name,
		gp = gp,
		vp = vp,
		cl = "pattern"
	)
}

#' @export
makeContent.pattern <- function(x) {
	# use pattern cached at creation unless `pattern` has since been edited
	if (!is.null(x$entry) && identical(x$entry_pattern, x$pattern)) {
		entry <- x$entry
	} else {
		entry <- resolve_pattern(x$pattern)
	}
	params <- complete_params(
		x$args,
		pattern = entry$name,
		gp = x$gp,
		defaults = entry$defaults
	)

	# avoid weird errors with array patterns if there is an active device open
	current_dev <- grDevices::dev.cur()
	on.exit(grDevices::dev.set(current_dev))

	xp <- convertX(x$x, "npc", valueOnly = TRUE)
	yp <- convertY(x$y, "npc", valueOnly = TRUE)
	id <- x$id
	boundary_df <- create_polygon_df(xp, yp, id)

	if (!is.na(params$pattern_aspect_ratio)) {
		aspect_ratio <- params$pattern_aspect_ratio
	} else {
		width <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
		height <- convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
		aspect_ratio <- width / height
	}

	# needs to be called within active graphics device to guess R4.1 capabilities
	params <- get_R4.1_params(params)

	fn <- get_pattern_fn(entry)
	grob <- fn(params, boundary_df, aspect_ratio, x$legend)
	gl <- gList(grob)
	setChildren(x, gl)
}
