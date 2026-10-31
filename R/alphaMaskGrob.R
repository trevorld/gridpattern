#' Mask grob using another grob to specify the (alpha) mask
#'
#' `alphaMaskGrob()` masks a grob using another grob to specify the (alpha) mask.
#'
#' @param maskee Grob to be masked
#' @param masker Grob that defines masking region
#' @param use_R4.1_masks If `TRUE` use the grid mask feature introduced in R v4.1.0.
#'                       If `FALSE` do a `rasterGrob` approximation.
#'                       If `NULL` try to guess an appropriate choice.
#'                       Note not all graphic devices support the grid mask feature.
#' @param png_device \dQuote{png} graphics device to save intermediate raster data with if `use_R4.1_masks` is `FALSE`.
#'                   If `NULL` and suggested package `ragg` is available
#'                   and versions are high enough we directly capture masked raster via [ragg::agg_capture()].
#'                   Otherwise we will use `png_device`
#'                   (default [ragg::agg_png()] if available else [grDevices::png()]) and [png::readPNG()]
#'                   to manually compute a masked raster.
#' @param res Resolution of desired `rasterGrob` in pixels per inch if `use_R4.1_masks` is `FALSE`.
#'            If `NULL` (the default) or `NA` then `getOption("ggpattern_res")` or else
#'            (a little more than) the resolution of the graphics device it is drawn on.
#' @return A `grid` grob
#' @inheritParams grid::polygonGrob
#' @examples
#'   \donttest{# May take more than 5 seconds on CRAN servers
#'   if (capabilities("png") && require("grid")) {
#'     maskee <- patternGrob("circle", gp = gpar(col = "black", fill = "yellow"),
#'                            spacing = 0.1, density = 0.5)
#'     angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 7)
#'     x_hex_outer <- 0.5 + 0.5 * cos(angle)
#'     y_hex_outer <- 0.5 + 0.5 * sin(angle)
#'     x_hex_inner <- 0.5 + 0.25 * cos(rev(angle))
#'     y_hex_inner <- 0.5 + 0.25 * sin(rev(angle))
#'     gp <- gpar(lwd = 0, col = NA, fill = "white")
#'     masker <- grid::pathGrob(x = c(x_hex_outer, x_hex_inner),
#'                              y = c(y_hex_outer, y_hex_inner),
#'                              id = rep(1:2, each = 7),
#'                              rule = "evenodd", gp = gp)
#'     masked <- alphaMaskGrob(maskee, masker, use_R4.1_masks = FALSE)
#'     grid.draw(masked)
#'   }
#'   if (capabilities("png") && require("grid")) {
#'     maskee_transparent <- rectGrob(gp = gpar(col = NA, fill = "blue"))
#'     gp <- gpar(lwd = 20, col = "black", fill = grDevices::rgb(0, 0, 0, 0.5))
#'     masker_transparent <- editGrob(masker, gp = gp)
#'     masked_transparent <- alphaMaskGrob(maskee_transparent,
#'                                         masker_transparent,
#'                                         use_R4.1_masks = FALSE)
#'     grid.newpage()
#'     grid.draw(masked_transparent)
#'   }
#'   }
#' @export
alphaMaskGrob <- function(
	maskee,
	masker,
	use_R4.1_masks = getOption(
		"ggpattern_use_R4.1_masks",
		getOption("ggpattern_use_R4.1_features")
	),
	png_device = NULL,
	res = NULL,
	name = NULL,
	gp = gpar(),
	vp = NULL
) {
	gTree(
		maskee = maskee,
		masker = masker,
		use_R4.1_masks = use_R4.1_masks,
		res = res,
		png_device = png_device,
		name = name,
		gp = gp,
		vp = vp,
		cl = "alpha_mask"
	)
}

# Avoid R CMD check WARNING on R 4.0 which lacks `mask` argument
vport <- function(...) viewport(...)

#' @export
makeContent.alpha_mask <- function(x) {
	current_dev <- grDevices::dev.cur()
	on.exit(grDevices::dev.set(current_dev))

	use_R4.1_masks <- x$use_R4.1_masks
	if (is.null(use_R4.1_masks)) {
		use_R4.1_masks <- guess_has_R4.1_features("masks")
	} else {
		use_R4.1_masks <- as.logical(use_R4.1_masks)
	}

	res <- if (is_unset_res(x$res)) default_res() else x$res

	if (use_R4.1_masks) {
		grob <- grobTree(x$maskee, vp = vport(mask = x$masker), name = "alpha_mask")
	} else if (is.null(region <- raster_region(x$masker, res, stroke = TRUE))) {
		# the masker doesn't overlap the viewport
		grob <- nullGrob(name = "alpha_mask")
	} else if (
		is.null(x$png_device) &&
			requireNamespace("ragg", quietly = TRUE) &&
			packageVersion("ragg") >= '1.2.0'
	) {
		grob <- gridpattern_mask_agg_capture(x$maskee, x$masker, res, region)
	} else {
		png_device <- x$png_device %||% default_png_device()
		if (device_supports_masks(png_device)) {
			grob <- gridpattern_mask_raster_straight(x$maskee, x$masker, res, png_device, region)
		} else {
			grob <- gridpattern_mask_raster_manual(x$maskee, x$masker, res, png_device, region)
		}
	}

	gl <- gList(grob)
	setChildren(x, gl)
}

device_supports_masks <- function(png_device) {
	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev))
	}
	png_file <- tempfile(fileext = ".png")
	on.exit(unlink(png_file), add = TRUE)
	png_device(png_file)
	value <- guess_has_R4.1_features("masks")
	dev.off()
	value
}

gridpattern_mask_agg_capture <- function(maskee, masker, res, region = full_region()) {
	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev))
	}
	px <- region_pixels(region, res)
	maskee <- in_region(maskee, region)
	masker <- in_region(masker, region)

	ragg::agg_capture(height = px$height, width = px$width, res = res, bg = "transparent")
	grob <- alphaMaskGrob(maskee, masker, use_R4.1_masks = TRUE)
	grid.draw(grob)
	raster_masked <- dev.capture(native = FALSE)
	dev.off()
	region_raster(raster_masked, region)
}

# The raster fallbacks only rasterise the `region` of the current viewport that can be seen:
# the bounding box of the masker (or clipper) clamped to the viewport.
# A `region` is a list of its (left, bottom) `x` / `y` and `width` / `height` in "npc" units.
full_region <- function() {
	list(x = 0, y = 0, width = 1, height = 1)
}

# Returns `NULL` if `grob` doesn't overlap the viewport.
# A masker's strokes (unlike a clipper's) also count so pad by (half) their widths.
raster_region <- function(grob, res, stroke = FALSE) {
	# `forceGrob()` runs any `makeContent()` (once) so its children's line widths are also seen
	bbox <- tryCatch(
		{
			grob <- forceGrob(grob)
			grob_bbox(grob)
		},
		error = function(e) NULL
	)
	if (is.null(bbox)) {
		return(full_region())
	}
	# a couple pixels for anti-aliasing
	pad <- 2 / res
	if (stroke) {
		lwd <- grob_lwds(grob, get.gpar("lwd")$lwd)
		pad <- pad + 0.5 * max(lwd, 0, na.rm = TRUE) / 96 # 1 lwd = 1/96 inch
	}
	# snapped to whole pixels (from the bottom left of the viewport) clamped to the viewport
	vpm <- get_vp_measurements("in")
	vp_width <- res * vpm$width
	vp_height <- res * vpm$height
	x0 <- max(floor(res * (bbox[["xmin"]] - pad)), 0)
	x1 <- min(ceiling(res * (bbox[["xmax"]] + pad)), vp_width)
	y0 <- max(floor(res * (bbox[["ymin"]] - pad)), 0)
	y1 <- min(ceiling(res * (bbox[["ymax"]] + pad)), vp_height)
	if (x1 <= x0 || y1 <= y0) {
		return(NULL)
	}
	list(
		x = x0 / vp_width,
		y = y0 / vp_height,
		width = (x1 - x0) / vp_width,
		height = (y1 - y0) / vp_height
	)
}

# bounding box (in inches from the bottom left of the current viewport) of what `grob` draws
# or `NULL` if we can't tell (in which case the whole viewport is rasterised)
# NB. `grobWidth()` etc. of a `gTree()` is zero so use `grobCoords()` (whose result changed in R 4.2)
grob_bbox <- function(grob) {
	if (getRversion() < "4.2.0" || has_points(grob)) {
		return(NULL)
	}
	xy <- coords_xy(grobCoords(grob, closed = TRUE), grobCoords(grob, closed = FALSE))
	if (length(xy$x) == 0L || !all(is.finite(c(xy$x, xy$y)))) {
		return(NULL)
	}
	c(xmin = min(xy$x), xmax = max(xy$x), ymin = min(xy$y), ymax = max(xy$y))
}

# `grobCoords()` of points is their symbol (not where they are drawn)
has_points <- function(grob) {
	inherits(grob, "points") ||
		(inherits(grob, "gTree") && any(vapply(grob$children, has_points, logical(1L))))
}

# the `x` / `y` of the (parallel) `closed` and `open` coordinates of a grob
# or `NULL` if any (primitive) grob has neither e.g. a raster or a grob without a `grobCoords()` method
coords_xy <- function(closed, open) {
	if (inherits(closed, "GridGTreeCoords")) {
		l <- Map(coords_xy, closed, open)
		if (any(vapply(l, is.null, logical(1L)))) {
			return(NULL)
		}
	} else {
		# closed shapes only have `closed = TRUE` coordinates and open ones only `closed = FALSE` ones
		l <- Filter(Negate(isEmptyCoords), c(unclass(closed), unclass(open)))
		if (length(l) == 0L) {
			return(NULL)
		}
	}
	list(x = unlist(lapply(l, `[[`, "x")), y = unlist(lapply(l, `[[`, "y")))
}

# (approximate) line widths of the strokes of `grob` (and its children) given the `inherited` one
grob_lwds <- function(grob, inherited) {
	gp <- grob$gp
	if (!is.null(gp$col) && all(is.na(gp$col))) {
		lwd <- 0
	} else {
		lwd <- gp$lwd %||% inherited
	}
	if (inherits(grob, "gTree")) {
		lwd <- c(lwd, unlist(lapply(grob$children, grob_lwds, inherited = max(lwd))))
	}
	lwd
}

# (whole) pixel width / height of the raster of `region` of the current viewport at `res`
region_pixels <- function(region, res) {
	vpm <- get_vp_measurements("in")
	list(
		width = round(res * region$width * vpm$width),
		height = round(res * region$height * vpm$height)
	)
}

# draws `grob` so that `region` of the current viewport fills the (raster) device
# (keeping the viewport's physical size so absolute sizes are unchanged)
in_region <- function(grob, region) {
	vp <- viewport(
		x = (0.5 - region$x) / region$width,
		y = (0.5 - region$y) / region$height,
		width = 1 / region$width,
		height = 1 / region$height
	)
	grobTree(grob, vp = vp)
}

# places `raster` (of `region`) back in `region` of the current viewport
region_raster <- function(raster, region, name = NULL) {
	rasterGrob(
		raster,
		x = region$x,
		y = region$y,
		width = region$width,
		height = region$height,
		just = c("left", "bottom"),
		name = name
	)
}

default_png_device <- function() {
	if (requireNamespace("ragg", quietly = TRUE)) {
		ragg::agg_png
	} else {
		stopifnot(capabilities("png"))
		grDevices::png
	}
}

gridpattern_mask_raster_straight <- function(
	maskee,
	masker,
	res,
	png_device,
	region = full_region()
) {
	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev))
	}
	px <- region_pixels(region, res)
	maskee <- in_region(maskee, region)
	masker <- in_region(masker, region)

	png_masked <- tempfile(fileext = ".png")
	on.exit(unlink(png_masked), add = TRUE)
	png_device(png_masked, height = px$height, width = px$width, res = res, bg = "transparent")
	grob <- alphaMaskGrob(maskee, masker, use_R4.1_masks = TRUE)
	grid.draw(grob)
	dev.off()

	raster_masked <- png::readPNG(png_masked, native = FALSE)
	region_raster(raster_masked, region)
}

gridpattern_mask_raster_manual <- function(
	maskee,
	masker,
	res,
	png_device,
	region = full_region()
) {
	current_dev <- grDevices::dev.cur()
	if (current_dev > 1) {
		on.exit(grDevices::dev.set(current_dev))
	}
	px <- region_pixels(region, res)
	maskee <- in_region(maskee, region)
	masker <- in_region(masker, region)

	png_maskee <- tempfile(fileext = ".png")
	on.exit(unlink(png_maskee), add = TRUE)
	png_device(png_maskee, height = px$height, width = px$width, res = res, bg = "transparent")
	grid.draw(maskee)
	dev.off()

	png_masker <- tempfile(fileext = ".png")
	on.exit(unlink(png_masker), add = TRUE)
	png_device(png_masker, height = px$height, width = px$width, res = res, bg = "transparent")
	grid.draw(masker)
	dev.off()

	raster_maskee <- png::readPNG(png_maskee, native = FALSE)
	raster_masker <- png::readPNG(png_masker, native = FALSE)

	stopifnot(
		length(dim(raster_maskee)) == 3L,
		length(dim(raster_masker)) == 3L,
		dim(raster_maskee)[3L] >= 3L,
		dim(raster_masker)[3L] >= 3L
	)
	if (dim(raster_maskee)[3L] < 4L) {
		raster_maskee <- add_alpha_channel(raster_maskee)
	}
	if (dim(raster_masker)[3L] < 4L) {
		raster_masker <- add_alpha_channel(raster_masker)
	}

	raster_masked <- raster_maskee
	raster_masked[,, 4L] <- raster_maskee[,, 4L] * raster_masker[,, 4L]

	region_raster(raster_masked, region, name = "alpha_mask")
}

add_alpha_channel <- function(a) {
	a_ <- array(NA, dim = c(dim(a)[1], dim(a)[2], 4L))
	a_[,, -4L] <- a
	a_[,, 4L] <- 1.
	a_
}
