#' Guess whether "active" graphics device supports
#' the grid graphics features introduced in R v4.1.
#'
#' `guess_has_R_4.1_features()` guesses whether "active" graphics device supports
#' the grid graphics features introduced in R v4.1.  If it guesses it does
#' it returns `TRUE` else `FALSE`.
#'
#' @param features Character vector of features to guess support for.
#'                 Will return `TRUE` only if guesses support for all requested features.\describe{
#'                 \item{"clippingPaths"}{Supports clipping path feature}
#'                 \item{"gradients"}{Supports (both linear and radial) gradient feature}
#'                 \item{"masks"}{Supports (alpha) mask feature}
#'                 \item{"patterns"}{Supports (tiling) pattern feature}
#'                 }
#' @seealso \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html} for more info about the new grid graphics
#'         features introduced in R v4.1.
#' @return `TRUE` if we guess all `features` are supported else `FALSE`
#' @examples
#'   # If R version (weakly) greater than 4.1 should be TRUE
#'   pdf(tempfile(fileext = ".pdf"))
#'   print(guess_has_R4.1_features())
#'   invisible(dev.off())
#'
#'   # Should be FALSE
#'   postscript(tempfile(fileext = ".ps"))
#'   print(guess_has_R4.1_features())
#'   invisible(dev.off())
#'
#' @export
guess_has_R4.1_features <- function(features = c("clippingPaths", "gradients", "masks", "patterns")) {
    if (getRversion() < '4.1.0')
        return (FALSE)

    # In R 4.2 `dev.capabilities()` can confirm/deny R 4.1 graphic feature support
    # if active graphics device has implemented this feature
    if (getRversion() >= '4.2.0') {
        if (confirm_via_dev_capabilities(features))
            return (TRUE)
        if (deny_via_dev_capabilities(features))
            return (FALSE)
    }

    device <- names(grDevices::dev.cur())
    if (device %in% c("cairo_pdf", "cairo_ps", "pdf", "svg", "X11cairo")) {
        TRUE
    } else if (device %in% c("bmp", "jpeg", "png", "tiff")) {
        if (.Platform$OS.type == "windows") # could be `type = "windows"` or `type = "cairo"`
            confirm_via_dev_capabilities(features)
        else # on unix non-"cairo" type have different device names from "cairo" type
            TRUE
    } else if (device %in% c("agg_jpeg", "agg_ppm", "agg_png", "agg_tiff")) {
        packageVersion("ragg") >= '1.2.0'
    } else if (device == "devSVG") {
        # {vdiffr}'s embedded {svglite} graphics device is also called "devSGV"
        # but doesn't support R 4.1 features (yet)
        if (likely_called_by_vdiffr()) {
            FALSE
        } else {
            packageVersion("svglite") >= '2.1.0'
        }
    } else {
        FALSE
    }
}

# Will always return FALSE if called within R 4.1
# or if graphics device hasn't been updated to provide this information
# even if the device had been updated to provide R 4.1 graphic feature support
confirm_via_dev_capabilities <- function(features = c("clippingPaths", "gradients", "masks", "patterns")) {
    support <- dev.capabilities()

    if (("clippingPaths" %in% features) && !isTRUE(support$clippingPaths))
        return (FALSE)
    if (("gradients" %in% features) && !all(c("LinearGradient", "RadialGradient") %in% support$patterns))
        return (FALSE)
    if (("masks" %in% features) && !("alpha" %in% support$masks))
        return (FALSE)
    if (("patterns" %in% features) && !("TilingPattern" %in% support$patterns))
        return (FALSE)

    TRUE
}

# Will return `TRUE` if `dev.capabilities()` explicitly indicates
# the given features are not supported (versus merely missing a positive indication)
deny_via_dev_capabilities <- function(features = c("clippingPaths", "gradients", "masks", "patterns")) {
    support <- dev.capabilities()

    if (("clippingPaths" %in% features) && !is.na(support$clippingPaths) && !isTRUE(support$clippingPaths))
        return (TRUE)
    if (("gradients" %in% features) && !is.na(support$patterns) && !all(c("LinearGradient", "RadialGradient") %in% support$patterns))
        return (TRUE)
    if (("masks" %in% features) && !is.na(support$masks) && !("alpha" %in% support$masks))
        return (TRUE)
    if (("patterns" %in% features) && !is.na(support$patterns) && !("TilingPattern" %in% support$patterns))
        return (TRUE)

    FALSE
}

# `vdiffr::write_svg()` is the function that calls the embedded {svglite}
# but when `vdiffr::expect_doppelganger()` calls it its call is "writer"
likely_called_by_vdiffr <- function() {
    if (!requireNamespace("svglite", quietly = TRUE))
        return (TRUE)

    n <- sys.nframe()
    while(n > 0) {
        call <- as.character(sys.call(n))
        if (grepl("write_svg$|expect_doppelganger$", call[1]))
            return(TRUE)
        n <- n - 1L
    }
    FALSE
}
