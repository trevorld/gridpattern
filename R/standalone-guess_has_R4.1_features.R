# ---
# repo: trevorld/gridpattern
# file: standalone-guess_has_R4.1_features.R
# last-updated: 2023-06-01
# license: https://unlicense.org
# imports: [grDevices, utils]
# ---
#
# nocov start
#
# You may copy this source into your own R package
# by either using `usethis::use_standalone("trevorld/gridpattern", "standalone-guess_has_R4.1_features.R")`
# or simply copying this file into your `R` directory and addding `grDevices` and `utils` to the `Imports` of
# your `DESCRIPTION` file.

#' Guess whether "active" graphics device supports
#' the grid graphics features introduced in R v4.1.
#'
#' `guess_has_R4.1_features()` guesses whether "active" graphics device supports
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
#' @seealso \url{https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html}
#'   for more info about the new grid graphics features introduced in R v4.1.
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
#' @keywords internal
#' @noRd
guess_has_R4.1_features <- function(features = c("clippingPaths", "gradients", "masks", "patterns")) {
    if (getRversion() < "4.1.0")
        return(FALSE)

    # In R 4.2 `dev.capabilities()` can confirm/deny R 4.1 graphic feature support
    # if active graphics device has implemented this feature
    if (getRversion() >= "4.2.0") {
        capabilities <- grDevices::dev.capabilities()
        if (confirm_via_dev_capabilities(features, capabilities))
            return(TRUE)
        if (deny_via_dev_capabilities(features, capabilities))
            return(FALSE)
    }

    device <- names(grDevices::dev.cur())
    if (device %in% c("cairo_pdf", "cairo_ps", "pdf", "svg", "X11cairo")) {
        TRUE
    } else if (device %in% c("bmp", "jpeg", "png", "tiff")) {
        # on unix non-"cairo" type have different device names from "cairo" type
        # but on Windows can't distinguish between `type = "windows"` or `type = "cairo"`
        .Platform$OS.type == "unix"
    } else if (device %in% c("agg_jpeg", "agg_ppm", "agg_png", "agg_tiff")) {
        utils::packageVersion("ragg") >= "1.2.0"
    } else if (device == "devSVG") {
        # {vdiffr}'s embedded {svglite} graphics device is also called "devSGV"
        # but doesn't support R 4.1 features (yet)
        if (likely_called_by_vdiffr()) {
            FALSE
        } else {
            utils::packageVersion("svglite") >= "2.1.0"
        }
    } else {
        FALSE
    }
}

# Will always return FALSE if called within R 4.1
# or if graphics device hasn't been updated to provide this information
# even if the device had been updated to provide R 4.1 graphic feature support
confirm_via_dev_capabilities <- function(features = c("clippingPaths", "gradients", "masks", "patterns"),
                                         capabilities = grDevices::dev.capabilities()) {

    for (feature in features) {
        if (!confirm_feature(feature, capabilities))
            return(FALSE)
    }

    TRUE
}

confirm_feature <- function(feature, capabilities) {
    switch(feature,
           clippingPaths = isTRUE(capabilities$clippingPaths),
           gradients = all(c("LinearGradient", "RadialGradient") %in% capabilities$patterns),
           masks = "alpha" %in% capabilities$masks,
           patterns = "TilingPattern" %in% capabilities$patterns
           )
}

# Will return `TRUE` if `dev.capabilities()` explicitly indicates
# the given features are not supported (versus merely missing a positive indication)
deny_via_dev_capabilities <- function(features = c("clippingPaths", "gradients", "masks", "patterns"),
                                      capabilities = grDevices::dev.capabilities()) {
    for (feature in features) {
        if (deny_feature(feature, capabilities))
            return(TRUE)
    }

    FALSE
}

deny_feature <- function(feature, capabilities) {
    switch(feature,
           clippingPaths = isFALSE(capabilities$clippingPaths),
           gradients = !is.na(capabilities$patterns) &&
               !all(c("LinearGradient", "RadialGradient") %in% capabilities$patterns),
           masks = !is.na(capabilities$masks) && !("alpha" %in% capabilities$masks),
           patterns = !is.na(capabilities$patterns) && !("TilingPattern" %in% capabilities$patterns)
           )
}

# `vdiffr::write_svg()` is the function that calls the embedded {svglite}
# but when `vdiffr::expect_doppelganger()` calls it its call is "writer"
likely_called_by_vdiffr <- function() {
    if (any(system.file(package = "vdiffr") == ""))
        return(FALSE)
    if (any(system.file(package = "svglite") == ""))
        return(TRUE)

    n <- sys.nframe()
    while (n > 0) {
        call <- as.character(sys.call(n))
        if (grepl("write_svg$|expect_doppelganger$", call[1]))
            return(TRUE)
        n <- n - 1L
    }
    FALSE
}

# nocov end
