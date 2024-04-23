#' Clip grob using another grob to specify the clipping path
#'
#' `clippingPathGrob()` clips a grob using another grob to specify the clipping path
#'
#' @param clippee Grob to be clipped
#' @param clipper Grob that defines clipping region
#' @param use_R4.1_clipping If `TRUE` use the grid clipping path feature introduced in R v4.1.0.
#'                          If `FALSE` do a `rasterGrob` approximation.
#'                          If `NULL` try to guess an appropriate choice.
#'                          Note not all graphic devices support the grid clipping path feature
#'                          and the grid clipping path feature does not nest.
#' @param png_device \dQuote{png} graphics device to save intermediate raster data with if `use_R4.1_clipping` is `FALSE`.
#'                   If `NULL` and suggested package `ragg` is available
#'                   and versions are high enough we directly capture clipped raster via [ragg::agg_capture()].
#'                   Otherwise we will use `png_device`
#'                   (default [ragg::agg_png()] if available else [grDevices::png()]) and [png::readPNG()]
#'                   to manually compute a clipped raster.
#' @param res Resolution of desired `rasterGrob` in pixels per inch if `use_R4.1_clipping` is `FALSE`.
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
clippingPathGrob <- function(clippee, clipper,
         use_R4.1_clipping = getOption("ggpattern_use_R4.1_clipping",
                                       getOption("ggpattern_use_R4.1_features")),
         png_device = NULL, res = getOption("ggpattern_res", 72),
         name = NULL, gp = gpar(), vp = NULL) {
    gTree(clippee = clippee, clipper = clipper,
          use_R4.1_clipping = use_R4.1_clipping,
          res = res, png_device = png_device,
          name = name, gp = gp, vp = vp, cl = "clipping_path")
}

#' @export
makeContent.clipping_path <- function(x) {
    current_dev <- grDevices::dev.cur()
    on.exit(grDevices::dev.set(current_dev))

    use_R4.1_clipping <- x$use_R4.1_clipping
    if (is.null(use_R4.1_clipping))
        use_R4.1_clipping <- guess_has_R4.1_features("clippingPaths")
    else
        use_R4.1_clipping <- as.logical(use_R4.1_clipping)

    stopifnot(getRversion() >= '4.1.0' || !use_R4.1_clipping)

    if (use_R4.1_clipping) {
        grob <- grobTree(x$clippee,
                         vp = viewport(clip = x$clipper),
                         name = "clip")
    } else if (is.null(x$png_device) &&
               getRversion() >= '4.1.0' &&
               requireNamespace("ragg", quietly = TRUE) &&
               packageVersion("ragg") >= '1.2.0') {
        grob <- gridpattern_clip_agg_capture(x$clippee, x$clipper, x$res)
    } else {
        png_device <- x$png_device %||% default_png_device()
        if (device_supports_clipping(png_device)) {
            grob <- gridpattern_clip_raster_straight(x$clippee, x$clipper, x$res, png_device)
        } else {
            grob <- gridpattern_clip_raster_manual(x$clippee, x$clipper, x$res, png_device)
        }
    }

    gl <- gList(grob)
    setChildren(x, gl)
}

device_supports_clipping <- function(png_device) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    png_file <- tempfile(fileext = ".png")
    on.exit(unlink(png_file), add = TRUE)
    png_device(png_file)
    value <- guess_has_R4.1_features("clippingPaths")
    dev.off()
    value
}

gridpattern_clip_agg_capture <- function(clippee, clipper, res) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)

    ragg::agg_capture(height = height, width = width, res = res, bg = "transparent")
    grob <- clippingPathGrob(clippee, clipper, use_R4.1_clipping = TRUE)
    grid.draw(grob)
    raster_clipped <- dev.capture(native = FALSE)
    dev.off()
    grid::rasterGrob(raster_clipped)
}

gridpattern_clip_raster_straight <- function(clippee, clipper, res, png_device) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)

    png_clipped <- tempfile(fileext = ".png")
    on.exit(unlink(png_clipped), add = TRUE)
    png_device(png_clipped, height = height, width = width,
               res = res, bg = "transparent")
    grob <- clippingPathGrob(clippee, clipper, use_R4.1_clipping = TRUE)
    grid.draw(grob)
    dev.off()

    raster_clipped <- png::readPNG(png_clipped, native = FALSE)
    grid::rasterGrob(raster_clipped)
}

gridpattern_clip_raster_manual <- function(clippee, clipper, res, png_device) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)

    png_clippee <- tempfile(fileext = ".png")
    on.exit(unlink(png_clippee), add = TRUE)
    png_device(png_clippee, height = height, width = width,
               res = res, bg = "transparent")
    grid.draw(clippee)
    dev.off()

    png_clipper <- tempfile(fileext = ".png")
    on.exit(unlink(png_clipper), add = TRUE)
    png_device(png_clipper, height = height, width = width,
               res = res, bg = "transparent")
    pushViewport(viewport(gp = gpar(lwd = 0, col = NA, fill = "black")))
    grid.draw(clipper)
    popViewport()
    dev.off()

    raster_clippee <- png::readPNG(png_clippee, native = FALSE)
    raster_clipper <- png::readPNG(png_clipper, native = FALSE)
    clip_region <- apply(raster_clipper, c(1,2), function(x) any(x > 0))
    if (length(dim(raster_clippee) == 2)) {
        raster_clippee[!clip_region] <- 0
    } else {
        for (j in seq_len(dim(raster_clippee)[3])) {
            raster_clippee[!clip_region, j] <- 0
        }
    }
    rasterGrob(raster_clippee, name = "clip")
}
