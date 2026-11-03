test_that("aRtsy patterns works as expected", {
	withr::local_preserve_seed() # {aRtsy} patterns use the RNG
	skip_on_ci()
	skip_on_cran()
	skip_if_not_installed("aRtsy")
	skip_if_not(getRversion() >= "4.3.0")
	skip_if_not(isTRUE(all(capabilities(c("cairo", "png")))))

	f <- withr::local_tempfile(fileext = ".png")
	with_temp_dev(function() grDevices::png(f, type = "cairo"), grid.pattern_aRtsy(type = "maze"))
	expect_true(file.size(f) > 0)

	f <- withr::local_tempfile(fileext = ".png")
	with_temp_dev(
		function() grDevices::png(f, type = "cairo"),
		grid.pattern_aRtsy(type = "strokes")
	)
	expect_true(file.size(f) > 0)

	expect_true(length(names_aRtsy()) >= 34L)
})
