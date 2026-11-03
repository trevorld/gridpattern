# Opens a temporary graphics device with `open()` until `.local_envir` exits,
# then returns to the previously current device and closes the temporary device
# (unlike `withr::local_pdf()` etc., see r-lib/withr#295).
local_temp_dev <- function(open = function() grDevices::pdf(NULL), .local_envir = parent.frame()) {
	dev <- open_temp_dev(open)
	withr::defer(close_temp_dev(dev$temp, dev$old), envir = .local_envir)
	invisible(dev$temp)
}
