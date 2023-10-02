# ---
# repo: trevorld/gridpattern
# file: standalone-guess_has_R4.1_features.R
# last-updated: 2023-08-02
# license: https://spdx.org/licenses/MIT-0.html
# imports: [grDevices, utils]
# ---
#
# nocov start
#
# You may copy this source into your own R package
# by either using `usethis::use_standalone("trevorld/gridpattern", "standalone-guess_has_R4.1_features.R")`
# or simply copying this file into your `R` directory and adding `grDevices` and `utils` to the `Imports` of
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
        dev_capabilities <- grDevices::dev.capabilities()
        if (confirm_via_dev_capabilities(features, dev_capabilities))
            return(TRUE)
        if (deny_via_dev_capabilities(features, dev_capabilities))
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
        # `vdiffr:::svglite()` has name "devSVG_vdiffr" since v1.0.6
        utils::packageVersion("svglite") >= "2.1.0"
    } else {
        FALSE
    }
}

# Will always return FALSE if called within R 4.1
# or if graphics device hasn't been updated to provide this information
# even if the device had been updated to provide R 4.1 graphic feature support
confirm_via_dev_capabilities <- function(features = c("clippingPaths", "gradients", "masks", "patterns"),
                                         dev_capabilities = grDevices::dev.capabilities()) {
    for (feature in features) {
        if (!confirm_feature(feature, dev_capabilities))
            return(FALSE)
    }
    TRUE
}

confirm_feature <- function(feature, dev_capabilities) {
    switch(feature,
           clippingPaths = isTRUE(dev_capabilities$clippingPaths),
           gradients = all(c("LinearGradient", "RadialGradient") %in% dev_capabilities$patterns),
           masks = "alpha" %in% dev_capabilities$masks,
           patterns = "TilingPattern" %in% dev_capabilities$patterns
           )
}

# Will return `TRUE` if `dev.capabilities()` explicitly indicates
# the given features are not supported (versus merely missing a positive indication)
deny_via_dev_capabilities <- function(features = c("clippingPaths", "gradients", "masks", "patterns"),
                                      dev_capabilities = grDevices::dev.capabilities()) {
    for (feature in features) {
        if (deny_feature(feature, dev_capabilities))
            return(TRUE)
    }
    FALSE
}

deny_feature <- function(feature, dev_capabilities) {
    switch(feature,
           clippingPaths = isFALSE(dev_capabilities$clippingPaths),
           gradients = !is.na(dev_capabilities$patterns) &&
               !all(c("LinearGradient", "RadialGradient") %in% dev_capabilities$patterns),
           masks = !is.na(dev_capabilities$masks) && !("alpha" %in% dev_capabilities$masks),
           patterns = !is.na(dev_capabilities$patterns) && !("TilingPattern" %in% dev_capabilities$patterns)
           )
}

# nocov end
