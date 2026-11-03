assert_suggested <- function(package, pattern = NULL, fn = NULL) {
	if (!requireNamespace(package, quietly = TRUE)) {
		context <- if (!is.null(pattern)) {
			glue('in order to use the "{pattern}" pattern.')
		} else {
			glue("in order to use `{fn}()`.")
		}
		abort(c(
			glue("The suggested package {{{package}}} must be installed {context}"),
			i = glue('Install with the command `install.packages("{package}")`')
		))
	}
}

# base R's Cairo/Quartz devices as well as {ragg} / {svglite} / {vdiffr} devices
# should support Unicode without complaint
# Notably `pdf()` is a device that does not...
# Any other devices to add?
device_supports_unicode <- function() {
	device <- names(grDevices::dev.cur())
	unicode_devices <- c(
		"agg_capture",
		"agg_jpeg",
		"agg_ppm",
		"agg_png",
		"agg_record",
		"agg_tiff", # {ragg}
		"devSVG",
		"devSVG_vdiffr", # {svglite} / {vdiffr}
		"quartz",
		"quartz_off_screen", # Quartz
		"cairo_pdf",
		"cairo_ps",
		"svg",
		"X11cairo"
	) # Cairo
	if (
		any(vapply(
			unicode_devices,
			function(x) grepl(paste0("^", x), device),
			FUN.VALUE = logical(1L)
		))
	) {
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

# Restores graphics device `dev` unless it is the null device (1), no longer open,
# or already the current device.
# NB. `dev.set()` calls the device's (de)activate callbacks even if `dev` is already current
# and `dev.set(1L)` would open a new device.
restore_dev <- function(dev) {
	if (dev > 1L && dev %in% grDevices::dev.list() && grDevices::dev.cur() != dev) {
		grDevices::dev.set(dev)
	}
	invisible(NULL)
}

# Closes the temporary device `temp` and returns to device `old`
# (the result of `dev.cur()` before `temp` was opened).
# `old` is selected *first* so that closing `temp` doesn't make some other device current in between.
# Safe to call more than once.
close_temp_dev <- function(temp, old) {
	force(temp) # e.g. `close_temp_dev(dev.cur(), old)` must not see `old`
	restore_dev(old)
	if (temp > 1L && temp != old && temp %in% grDevices::dev.list()) {
		grDevices::dev.off(temp)
	}
	invisible(NULL)
}

# Opens a device with `open()`, evaluates `code` on it,
# then closes it and returns to the previously current device (also on error).
# Returns the value of `code`.
with_temp_dev <- function(open, code) {
	old <- grDevices::dev.cur()
	# If `open()` opens a device and then errors close that device and return to `old`.
	# A calling handler doesn't catch the error so it reaches the caller unchanged.
	withCallingHandlers(
		open(),
		error = function(e) close_temp_dev(grDevices::dev.cur(), old)
	)
	temp <- grDevices::dev.cur()
	if (temp == old) {
		abort("`open()` did not open a new graphics device.")
	}
	on.exit(close_temp_dev(temp, old), add = TRUE)
	code
}
