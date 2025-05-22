assert_suggested <- function(package, pattern) {
    if (!requireNamespace(package, quietly = TRUE)) {
        abort(c(glue("The suggested package {{{package}}} must be installed ",
                   'in order to use the "{pattern}" pattern.'),
               i = glue('Install with the command `install.packages("{package}")`')))
    }
}

# base R's Cairo/Quartz devices as well as {ragg} / {svglite} / {vdiffr} devices
# should support Unicode without complaint
# Notably `pdf()` is a device that does not...
# Any other devices to add?
device_supports_unicode <- function() {
    device <- names(grDevices::dev.cur())
    unicode_devices <- c("agg_capture", "agg_jpeg", "agg_ppm", "agg_png", "agg_record", "agg_tiff", # {ragg}
                         "devSVG", "devSVG_vdiffr", # {svglite} / {vdiffr}
                         "quartz", "quartz_off_screen", # Quartz
                         "cairo_pdf", "cairo_ps", "svg", "X11cairo") # Cairo
    if (any(vapply(unicode_devices, function(x) grepl(paste0("^", x), device),
                   FUN.VALUE = logical(1L)))) {
        TRUE
    } else if (device %in% c("bmp", "jpeg", "png", "tiff")) {
        # on unix non-"cairo" type have different device names from "cairo" type
        # but on Windows can't distinguish between `type = "windows"` or `type = "cairo"`
        # Windows device doesn't support new patterns feature
        if (getRversion() >= "4.2.0") {
            "LinearGradient" %in% grDevices::dev.capabilities()$patterns
        } else {
            .Platform$OS.type == "unix"
        }
    } else {
        FALSE
    }
}
