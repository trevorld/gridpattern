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
#' @param png_device \dQuote{png} graphics device to use if `use_R4.1_clipping` is `FALSE`.
#'                   If `NULL` (default) will use `ragg::agg_png()` if the
#'                   suggested package `ragg` is available else `grDevices::png()`.
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
          name=name, gp=gp, vp=vp, cl="clipping_path")
}

#' @export
makeContent.clipping_path <- function(x) {
    current_dev <- grDevices::dev.cur()
    on.exit(grDevices::dev.set(current_dev))

    use_R4.1_clipping <- x$use_R4.1_clipping
    if (is.null(use_R4.1_clipping))
        use_R4.1_clipping <- guess_has_R4.1_features()
    else
        use_R4.1_clipping <- as.logical(use_R4.1_clipping)

    stopifnot(getRversion() >= '4.1.0' || !use_R4.1_clipping)

    if (use_R4.1_clipping) {
        grob <- grobTree(x$clippee,
                         vp = viewport(clip = x$clipper),
                         name = "clip")
    } else {
        grob <- gridpattern_clip_raster(x)
    }

    gl <- gList(grob)
    setChildren(x, gl)
}

gridpattern_clip_raster <- function(x) {
    height <- x$res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- x$res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)
    png_device <- x$png_device
    if (is.null(png_device)) {
        if (requireNamespace("ragg", quietly = TRUE)) {
            png_device <- ragg::agg_png
        } else {
            stopifnot(capabilities("png"))
            png_device <- grDevices::png
        }
    }

    png_clippee <- tempfile(fileext = ".png")
    on.exit(unlink(png_clippee))
    png_device(png_clippee, height = height, width = width,
               res = x$res, bg = "transparent")
    grid.draw(x$clippee)
    dev.off()

    png_clipper <- tempfile(fileext = ".png")
    on.exit(unlink(png_clipper))
    png_device(png_clipper, height = height, width = width,
               res = x$res, bg = "transparent")
    pushViewport(viewport(gp = gpar(lwd = 0, col = NA, fill = "black")))
    grid.draw(x$clipper)
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
