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
#' @return A `grid` grob
#' @inheritParams grid::polygonGrob
#' @examples
#'   \donttest{ # Once took more >10s on a CRAN autocheck
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
alphaMaskGrob <- function(maskee, masker,
                          use_R4.1_masks = getOption("ggpattern_use_R4.1_masks",
                                                     getOption("ggpattern_use_R4.1_features")),
                          png_device = NULL, res = getOption("ggpattern_res", 72),
                          name = NULL, gp = gpar(), vp = NULL) {
    gTree(maskee = maskee, masker = masker,
          use_R4.1_masks = use_R4.1_masks,
          res = res, png_device = png_device,
          name = name, gp = gp, vp = vp, cl = "alpha_mask")
}

# Avoid R CMD check WARNING on R 4.0 which lacks `mask` argument
vport <- function(...) viewport(...)

#' @export
makeContent.alpha_mask <- function(x) {
    current_dev <- grDevices::dev.cur()
    on.exit(grDevices::dev.set(current_dev))

    use_R4.1_masks <- x$use_R4.1_masks
    if (is.null(use_R4.1_masks))
        use_R4.1_masks <- guess_has_R4.1_features("masks")
    else
        use_R4.1_masks <- as.logical(use_R4.1_masks)

    stopifnot(getRversion() >= '4.1.0' || !use_R4.1_masks)

    if (use_R4.1_masks) {
        grob <- grobTree(x$maskee,
                         vp = vport(mask = x$masker),
                         name = "alpha_mask")
    } else if (is.null(x$png_device) &&
               getRversion() >= '4.1.0' &&
               requireNamespace("ragg", quietly = TRUE) &&
               packageVersion("ragg") >= '1.2.0') {
        grob <- gridpattern_mask_agg_capture(x$maskee, x$masker, x$res)
    } else {
        png_device <- x$png_device %||% default_png_device()
        if (device_supports_masks(png_device)) {
            grob <- gridpattern_mask_raster_straight(x$maskee, x$masker, x$res, png_device)
        } else {
            grob <- gridpattern_mask_raster_manual(x$maskee, x$masker, x$res, png_device)
        }
    }

    gl <- gList(grob)
    setChildren(x, gl)
}

device_supports_masks <- function(png_device) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    png_file <- tempfile(fileext = ".png")
    on.exit(unlink(png_file), add = TRUE)
    png_device(png_file)
    value <- guess_has_R4.1_features("masks")
    dev.off()
    value
}

gridpattern_mask_agg_capture <- function(maskee, masker, res) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)

    ragg::agg_capture(height = height, width = width, res = res, bg = "transparent")
    grob <- alphaMaskGrob(maskee, masker, use_R4.1_masks = TRUE)
    grid.draw(grob)
    raster_masked <- dev.capture(native = FALSE)
    dev.off()
    grid::rasterGrob(raster_masked)
}

default_png_device <- function() {
    if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_png
    } else {
        stopifnot(capabilities("png"))
        grDevices::png
    }
}

gridpattern_mask_raster_straight <- function(maskee, masker, res, png_device) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)

    png_masked <- tempfile(fileext = ".png")
    on.exit(unlink(png_masked), add = TRUE)
    png_device(png_masked, height = height, width = width,
               res = res, bg = "transparent")
    grob <- alphaMaskGrob(maskee, masker, use_R4.1_masks = TRUE)
    grid.draw(grob)
    dev.off()

    raster_masked <- png::readPNG(png_masked, native = FALSE)
    grid::rasterGrob(raster_masked)
}

gridpattern_mask_raster_manual <- function(maskee, masker, res, png_device) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1) on.exit(grDevices::dev.set(current_dev))
    height <- res * convertHeight(unit(1, "npc"), "in",  valueOnly = TRUE)
    width <- res * convertWidth(unit(1, "npc"),  "in", valueOnly = TRUE)

    png_maskee <- tempfile(fileext = ".png")
    on.exit(unlink(png_maskee), add = TRUE)
    png_device(png_maskee, height = height, width = width,
               res = res, bg = "transparent")
    grid.draw(maskee)
    dev.off()

    png_masker <- tempfile(fileext = ".png")
    on.exit(unlink(png_masker), add = TRUE)
    png_device(png_masker, height = height, width = width,
               res = res, bg = "transparent")
    grid.draw(masker)
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
