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

# Returns to device `old` (the result of `dev.cur()` before `temp` was opened)
# and then closes the temporary device `temp`.
# `old` is selected *first* so that closing `temp` doesn't make some other device current in between.
# Safe to call more than once.
close_temp_dev <- function(temp, old) {
	force(temp) # e.g. `close_temp_dev(dev.cur(), old)` must not see `old`
	restore_dev(old)
	if (temp > 1L && temp != old && temp %in% grDevices::dev.list()) {
		# an error while drawing leaves the device locked and closing it then warns
		# "Killing locked device" which is just noise next to that error
		withCallingHandlers(
			grDevices::dev.off(temp),
			warning = function(w) {
				if (conditionMessage(w) %in% killing_locked_device_messages()) {
					invokeRestart("muffleWarning")
				}
			}
		)
	}
	invisible(NULL)
}

killing_locked_device_messages <- function() {
	msg <- "Killing locked device"
	domains <- c("R", "R-grDevices", "R-grid")
	unique(c(msg, vapply(domains, function(d) gettext(msg, domain = d), character(1L))))
}

# Opens a temporary device with `open()` and returns a list with the number of that device `temp`
# and the number of the previously current device `old` (to pass to `close_temp_dev()`).
# If `open()` errors after opening a device returns to `old` and then closes that device.
open_temp_dev <- function(open) {
	old <- grDevices::dev.cur()
	# Only ever close a device that `open()` opened:
	# `open()` may leave some other pre-existing device current
	# e.g. if it closes the device it opened (making the next device current) or calls `dev.set()`.
	before <- grDevices::dev.list()
	# A calling handler doesn't catch the error so it reaches the caller unchanged.
	withCallingHandlers(
		open(),
		error = function(e) {
			dev <- grDevices::dev.cur()
			if (dev %in% before) restore_dev(old) else close_temp_dev(dev, old)
		}
	)
	temp <- grDevices::dev.cur()
	if (temp %in% c(before, 1L)) {
		restore_dev(old)
		abort("`open()` did not open a new graphics device.")
	}
	list(temp = temp, old = old)
}

# Opens a device with `open()`, evaluates `code` on it,
# then returns to the previously current device and closes it (also on error).
# Returns the value of `code`.
with_temp_dev <- function(open, code) {
	dev <- open_temp_dev(open)
	on.exit(close_temp_dev(dev$temp, dev$old), add = TRUE)
	code
}

# Draws `code` on a temporary transparent `png_device()` device of `px` pixels at resolution `res`
# and returns the image as a numeric array (see `png::readPNG()`).
draw_png_array <- function(png_device, px, res, code) {
	png_file <- tempfile(fileext = ".png")
	on.exit(unlink(png_file), add = TRUE)
	open <- function() {
		png_device(png_file, height = px$height, width = px$width, res = res, bg = "transparent")
	}
	with_temp_dev(open, code)
	png::readPNG(png_file, native = FALSE)
}

# Draws `code` on a temporary transparent `ragg::agg_capture()` device of `px` pixels at resolution `res`
# and returns the captured raster (see `grDevices::dev.capture()`).
draw_agg_capture <- function(px, res, code) {
	open <- function() {
		ragg::agg_capture(height = px$height, width = px$width, res = res, bg = "transparent")
	}
	with_temp_dev(open, {
		code
		grDevices::dev.capture(native = FALSE)
	})
}
