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
#' @param png_device \dQuote{png} graphics device to use if `use_R4.1_masks` is `FALSE`.
#'                   If `NULL` (default) will use `ragg::agg_png()` if the
#'                   suggested package `ragg` is available else `grDevices::png()`.
#' @param res Resolution of desired `rasterGrob` in pixels per inch if `use_R4.1_masks` is `FALSE`.
#' @return A `grid` grob
#' @inheritParams grid::polygonGrob
#' @examples
#'   if (capabilities("png") && require("grid")) {
#'     maskee <- patternGrob("circle", gp = gpar(col = "black", fill = "yellow"),
#'                            spacing = 0.1, density = 0.5)
#'     angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 7)
#'     x_hex_outer <- 0.5 + 0.5 * cos(angle)
#'     y_hex_outer <- 0.5 + 0.5 * sin(angle)
#'     x_hex_inner <- 0.5 + 0.25 * cos(rev(angle))
#'     y_hex_inner <- 0.5 + 0.25 * sin(rev(angle))
#'     gp <- gpar(lwd = 0, col = NA, fill = "black")
#'     masker <- grid::pathGrob(x = c(x_hex_outer, x_hex_inner),
#'                              y = c(y_hex_outer, y_hex_inner),
#'                              id = rep(1:2, each = 7),
#'                              rule = "evenodd", gp = gp)
#'     masked <- alphaMaskGrob(maskee, masker, use_R4.1_masks = FALSE)
#'     grid.newpage()
#'     grid.draw(masked)
#'
#'     maskee_transparent <- rectGrob(gp = gpar(col = NA, fill = "blue"))
#'     gp <- gpar(lwd = 20, col = "black", fill = grDevices::rgb(0, 0, 0, 0.5))
#'     masker_transparent <- editGrob(masker, gp = gp)
#'     masked_transparent <- alphaMaskGrob(maskee_transparent,
#'                                         masker_transparent,
#'                                         use_R4.1_masks = FALSE)
#'     grid.newpage()
#'     grid.draw(masked_transparent)
#'   }
#' @export
alphaMaskGrob <- function(maskee, masker,
         use_R4.1_masks = getOption("ggpattern_use_R4.1_masks",
                                    getOption("ggpattern_use_R4.1_features")),
         png_device = NULL, res = getOption("ggpattern_res", 72),
         name = NULL, gp = gpar(), vp = NULL) {
    gTree(maskee = maskee, masker = masker,
          use_R4.1_masks = use_R4.1_masks,
          res = res, png_device = png_device,
          name=name, gp=gp, vp=vp, cl="alpha_mask")
}

#' @export
makeContent.alpha_mask <- function(x) {
    current_dev <- grDevices::dev.cur()
    on.exit(grDevices::dev.set(current_dev))

    use_R4.1_masks <- x$use_R4.1_masks
    if (is.null(use_R4.1_masks))
        use_R4.1_masks <- guess_has_R4.1_features()
    else
        use_R4.1_masks <- as.logical(use_R4.1_masks)

    if (use_R4.1_masks) {
        grob <- grobTree(x$maskee,
                         vp = viewport(mask = x$masker),
                         name = "alpha_mask")
    } else {
        grob <- gridpattern_mask_raster(x)
    }

    gl <- gList(grob)
    setChildren(x, gl)
}

gridpattern_mask_raster <- function(x) {
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

    png_maskee <- tempfile(fileext = ".png")
    on.exit(unlink(png_maskee))
    png_device(png_maskee, height = height, width = width,
               res = x$res, bg = "transparent")
    grid.draw(x$maskee)
    dev.off()

    png_masker <- tempfile(fileext = ".png")
    on.exit(unlink(png_masker))
    png_device(png_masker, height = height, width = width,
               res = x$res, bg = "transparent")
    grid.draw(x$masker)
    dev.off()

    raster_maskee <- png::readPNG(png_maskee, native = FALSE)
    raster_masker <- png::readPNG(png_masker, native = FALSE)

    stopifnot(length(dim(raster_maskee)) == 3L,
              length(dim(raster_masker)) == 3L,
              dim(raster_maskee)[3L] >= 3L,
              dim(raster_masker)[3L] >= 3L)
    if (dim(raster_maskee)[3L] < 4L) {
        raster_maskee <- add_alpha_channel(raster_maskee)
    }
    if (dim(raster_masker)[3L] < 4L) {
        raster_masker <- add_alpha_channel(raster_masker)
    }

    raster_masked <- raster_maskee
    raster_masked[, , 4L] <- raster_maskee[, , 4L] * raster_masker[, , 4L]

    rasterGrob(raster_masked, name = "alpha_mask")
}

add_alpha_channel <- function(a) {
    a_ <- array(NA, dim = c(dim(a)[1], dim(a)[2], 4L))
    a_[, , -4L] <- a
    a_[, , 4L] <- 1.
    a_
}
