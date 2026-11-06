#' Clip grob using another grob to specify the clipping path
#'
#' `clippingPathGrob()` clips a grob using another grob to specify the clipping path
#'
#' @param clippee Grob to be clipped
#' @param clipper Grob that defines clipping region.
#'                Like native clipping paths the raster fallbacks ignore its graphical parameters
#'                (e.g. a stroke or a semi-transparent fill)
#'                although if `use_R4.1_clipping` is `FALSE` and the `png_device` doesn't support clipping paths
#'                only its filled shapes (not e.g. lines or text) clip.
#' @param use_R4.1_clipping If `TRUE` use the grid clipping path feature introduced in R v4.1.0.
#'                          If `FALSE` do a `rasterGrob` approximation.
#'                          If `NULL` try to guess an appropriate choice.
#'                          Note not all graphic devices support the grid clipping path feature
#'                          and the grid clipping path feature does not nest.
#' @param png_device \dQuote{png} graphics device to save intermediate raster data with if `use_R4.1_clipping` is `FALSE`.
#'                   If `NULL` and suggested package `ragg` is available
#'                   and versions are high enough we directly capture clipped raster via [ragg::agg_capture()].
#'                   Otherwise we will use `png_device`
#'                   (default `grDevices::png(type = "cairo")` if R has Cairo and PNG support
#'                   else [ragg::agg_png()] if available else [grDevices::png()])
#'                   and [png::readPNG()]
#'                   to manually compute a clipped raster.
#' @param res Resolution of desired `rasterGrob` in pixels per inch if `use_R4.1_clipping` is `FALSE`.
#'            If `NULL` (the default) or `NA` then `getOption("ggpattern_res")` or else
#'            (a little more than) the resolution of the graphics device it is drawn on.
#' @return A `grid` grob
#' @inheritParams grid::polygonGrob
#' @examples
#'   if (capabilities("png") && require("grid")) {
#'     clippee <- patternGrob("circle", gp = gpar(col = "black", fill = "yellow"),
#'                            spacing = 0.1, density = 0.5)
#'     angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 7)
#'     x_hex_outer <- 0.5 + 0.5 * cos(angle)
#'     y_hex_outer <- 0.5 + 0.5 * sin(angle)
#'     x_hex_inner <- 0.5 + 0.25 * cos(rev(angle))
#'     y_hex_inner <- 0.5 + 0.25 * sin(rev(angle))
#'     clipper <- grid::pathGrob(x = c(x_hex_outer, x_hex_inner),
#'                               y = c(y_hex_outer, y_hex_inner),
#'                               id = rep(1:2, each = 7),
#'                               rule = "evenodd")
#'     clipped <- clippingPathGrob(clippee, clipper, use_R4.1_clipping = FALSE)
#'     grid.newpage()
#'     grid.draw(clipped)
#'   }
#' @export
clippingPathGrob <- function(
	clippee,
	clipper,
	use_R4.1_clipping = getOption(
		"ggpattern_use_R4.1_clipping",
		getOption("ggpattern_use_R4.1_features")
	),
	png_device = NULL,
	res = NULL,
	name = NULL,
	gp = gpar(),
	vp = NULL
) {
	gTree(
		clippee = clippee,
		clipper = clipper,
		use_R4.1_clipping = use_R4.1_clipping,
		res = res,
		png_device = png_device,
		name = name,
		gp = gp,
		vp = vp,
		cl = "clipping_path"
	)
}

#' @export
makeContent.clipping_path <- function(x) {
	current_dev <- grDevices::dev.cur()
	on.exit(restore_dev(current_dev), add = TRUE)

	use_R4.1_clipping <- x$use_R4.1_clipping
	if (is.null(use_R4.1_clipping)) {
		use_R4.1_clipping <- guess_has_R4.1_features("clippingPaths")
	} else {
		use_R4.1_clipping <- as.logical(use_R4.1_clipping)
	}

	res <- if (is_unset_res(x$res)) default_res() else x$res

	if (use_R4.1_clipping) {
		grob <- grobTree(x$clippee, vp = viewport(clip = x$clipper), name = "clip")
	} else if (is.null(region <- raster_region(x$clipper, res, stroke = FALSE))) {
		# the clipper doesn't overlap the viewport
		grob <- nullGrob(name = "clip")
	} else if (
		is.null(x$png_device) &&
			requireNamespace("ragg", quietly = TRUE) &&
			packageVersion("ragg") >= '1.2.0'
	) {
		grob <- gridpattern_clip_agg_capture(x$clippee, x$clipper, res, region)
	} else {
		png_device <- x$png_device %||% default_png_device()
		if (device_supports_clipping(png_device)) {
			grob <- gridpattern_clip_raster_straight(
				x$clippee,
				x$clipper,
				res,
				png_device,
				region
			)
		} else {
			masker <- clipper_masker(x$clipper)
			grob <- gridpattern_mask_raster_manual(x$clippee, masker, res, png_device, region)
			grob <- editGrob(grob, name = "clip")
		}
	}

	gl <- gList(grob)
	setChildren(x, gl)
}

device_supports_clipping <- function(png_device) {
	png_file <- tempfile(fileext = ".png")
	on.exit(unlink(png_file), add = TRUE)
	with_temp_dev(function() png_device(png_file), guess_has_R4.1_features("clippingPaths"))
}

gridpattern_clip_agg_capture <- function(clippee, clipper, res, region = full_region()) {
	px <- region_pixels(region, res)
	clippee <- in_region(clippee, region)
	clipper <- in_region(clipper, region)

	raster_clipped <- draw_agg_capture(
		px,
		res,
		grid.draw(clippingPathGrob(clippee, clipper, use_R4.1_clipping = TRUE))
	)
	region_raster(raster_clipped, region, name = "clip")
}

gridpattern_clip_raster_straight <- function(
	clippee,
	clipper,
	res,
	png_device,
	region = full_region()
) {
	px <- region_pixels(region, res)
	clippee <- in_region(clippee, region)
	clipper <- in_region(clipper, region)

	raster_clipped <- draw_png_array(
		png_device,
		px,
		res,
		grid.draw(clippingPathGrob(clippee, clipper, use_R4.1_clipping = TRUE))
	)
	region_raster(raster_clipped, region, name = "clip")
}

# Without native clipping paths a clipper is an alpha mask of its filled shapes:
# like native clipping paths ignore its (and its children's) graphical parameters
# (the fill colour doesn't matter: only its alpha channel is used)
clipper_masker <- function(clipper) {
	clipper_gp(forceGrob(clipper))
}

clipper_gp <- function(grob) {
	grob$gp <- gpar(fill = "black", col = NA, lwd = 0, alpha = 1)
	if (inherits(grob, "gTree")) {
		grob$children[] <- lapply(grob$children, clipper_gp)
	}
	grob
}
