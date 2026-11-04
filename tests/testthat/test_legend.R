# pattern that records what it is drawn with (and where)
legend_captured <- new.env()
create_pattern_legend_capture <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	legend_captured$legend <- legend
	legend_captured$params <- params
	legend_captured$boundary_df <- boundary_df
	legend_captured$spacing_in <- grid::convertWidth(
		unit(params$pattern_spacing, params$pattern_units),
		"in",
		valueOnly = TRUE
	)
	legend_captured$vp_in <- grid::convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
	grid::nullGrob()
}

draw_legend_capture <- function(legend, ...) {
	local_temp_dev(function() grDevices::pdf(NULL, width = 2, height = 2))
	grid.pattern("legend_capture", ..., legend = legend, res = 100)
	as.list(legend_captured)
}

test_that("`legend` may be logical or a non-negative number", {
	skip_if_not_installed("withr")
	withr::local_options(
		ggpattern_geometry_funcs = list(legend_capture = create_pattern_legend_capture)
	)

	no <- draw_legend_capture(FALSE)
	expect_identical(no$legend, FALSE)
	expect_identical(draw_legend_capture(0)$legend, FALSE)

	yes <- draw_legend_capture(TRUE)
	expect_identical(yes$legend, TRUE)
	one <- draw_legend_capture(1)
	expect_identical(one$legend, TRUE)
	# `TRUE` (or `1`) doesn't rescale anything
	expect_equal(yes$spacing_in, no$spacing_in)
	expect_equal(one$params$pattern_res, 100)
	expect_equal(one$boundary_df, no$boundary_df)

	# a scale of `s` draws in a centred viewport `s` times larger than the key
	s <- 2.5
	scaled <- draw_legend_capture(s)
	expect_identical(scaled$legend, TRUE)
	expect_equal(scaled$vp_in, s * no$vp_in)
	expect_equal(scaled$spacing_in, s * no$spacing_in)
	expect_equal(scaled$params$pattern_res, 100)
	expect_equal(range(scaled$boundary_df$x), 0.5 + c(-0.5, 0.5) / s)
	expect_equal(range(scaled$boundary_df$y), 0.5 + c(-0.5, 0.5) / s)
	# the pattern parameters (and absolute resolution) themselves are untouched
	expect_equal(scaled$params$pattern_spacing, no$params$pattern_spacing)

	# absolute units don't scale
	cm <- draw_legend_capture(s, units = "cm", spacing = 1)
	expect_equal(cm$spacing_in, 1 / 2.54)

	for (bad in list(-1, NA, NaN, Inf, c(TRUE, FALSE), "yes", NULL)) {
		expect_error(patternGrob("stripe", legend = bad), "`legend` must be")
	}
})

test_that("a scaled legend draws the pattern inside the boundary", {
	local_temp_dev(function() grDevices::pdf(NULL, width = 2, height = 2))
	grob <- grid.pattern(
		"stripe",
		x = c(0.2, 0.2, 0.8, 0.8),
		y = c(0.8, 0.2, 0.2, 0.8),
		legend = 3
	)
	grob <- grid::grid.force(grob)
	expect_equal(grob$children[[1]]$vp$width, unit(3, "npc"))
	expect_equal(grob$children[[1]]$vp$height, unit(3, "npc"))
	# the larger viewport is the child's `vp` (and not pushed by `makeContent()` itself)
	expect_identical(grid::current.vpPath(), NULL)
	grid::grid.newpage()
	grid::grid.draw(patternGrob("stripe", legend = 3))
	expect_identical(grid::current.vpPath(), NULL)
	expect_false("pattern_legend" %in% grid::grid.ls(viewports = TRUE, print = FALSE)$name)
})

test_that("`pattern_key_scale_factor` zooms a legend key", {
	local_pattern(
		"legend_capture",
		create_pattern_legend_capture,
		defaults = list(pattern_amplitude = function(params, gp) params$pattern_spacing / 2),
		key_scaling = c(
			pattern_spacing = 1,
			pattern_xoffset = 1,
			pattern_yoffset = 1,
			pattern_amplitude = 1
		),
		env = globalenv()
	)
	k <- 2
	s <- 2.5
	no <- draw_legend_capture(FALSE, key_scale_factor = k)
	# not zoomed if not a legend key
	expect_equal(no$params$pattern_spacing, 0.05)
	expect_equal(no$params$pattern_amplitude, 0.025)

	# relative lengths are zoomed by a viewport `legend * k` times larger than the key
	for (legend in list(1, s)) {
		zoomed <- draw_legend_capture(legend, key_scale_factor = k, xoffset = 0.1)
		expect_equal(zoomed$vp_in, as.numeric(legend) * k * no$vp_in)
		expect_equal(zoomed$params$pattern_spacing, 0.05)
		expect_equal(zoomed$params$pattern_xoffset, 0.1)
		expect_equal(zoomed$params$pattern_amplitude, 0.025)
		expect_equal(range(zoomed$boundary_df$x), 0.5 + c(-0.5, 0.5) / (as.numeric(legend) * k))
	}

	# absolute `key_scaling` lengths (whether supplied or defaults) are zoomed by `k`
	cm <- draw_legend_capture(s, units = "cm", spacing = 1, yoffset = 0.5, key_scale_factor = k)
	expect_equal(cm$vp_in, s * k * no$vp_in)
	expect_equal(cm$params$pattern_spacing, k)
	expect_equal(cm$spacing_in, k / 2.54)
	expect_equal(cm$params$pattern_yoffset, k * 0.5)
	expect_equal(cm$params$pattern_xoffset, 0)
	expect_equal(cm$params$pattern_amplitude, k / 2)
	cm <- draw_legend_capture(1, units = "cm", key_scale_factor = k)
	expect_equal(cm$params$pattern_spacing, k * 0.05)
	expect_equal(cm$params$pattern_amplitude, k * 0.025)
	# but a logical `legend` (as passed by `{ggpattern}` <= 1.3.1 which zooms the key itself) isn't zoomed
	for (units in c("snpc", "cm")) {
		yes <- draw_legend_capture(TRUE, units = units, spacing = 0.3, key_scale_factor = k)
		expect_identical(yes$legend, TRUE)
		expect_equal(yes$vp_in, no$vp_in)
		expect_equal(yes$params$pattern_spacing, 0.3)
		expect_equal(yes$params$pattern_amplitude, 0.15)
		# although it is still passed on (e.g. "magick" scales its image by it)
		expect_equal(yes$params$pattern_key_scale_factor, k)
	}
	# an unset (`NaN`) key scale factor is `1`
	expect_equal(draw_legend_capture(s, key_scale_factor = NaN)$vp_in, s * no$vp_in)

	for (bad in list(0, -1, NA, c(1, 2), "2")) {
		expect_error(
			complete_params(list(pattern_key_scale_factor = bad), key_zoom = TRUE),
			"`pattern_key_scale_factor` must be"
		)
	}
	# (which is only checked for a legend key)
	expect_equal(complete_params(list(pattern_key_scale_factor = 0))$pattern_key_scale_factor, 0)
})

test_that("defaults see the unzoomed parameters and `key_scaling` zooms them afterwards", {
	seen <- list()
	defaults <- list(
		pattern_spacing = 0.025,
		pattern_key_scale_factor = function(params, gp) {
			seen$ksf_spacing <<- params$pattern_spacing
			min(1, 0.025 / params$pattern_spacing)
		},
		pattern_amplitude = function(params, gp) params$pattern_spacing / 2,
		pattern_frequency = function(params, gp) 1 / params$pattern_spacing
	)
	key_scaling <- c(
		pattern_spacing = 1,
		pattern_xoffset = 1,
		pattern_yoffset = 1,
		pattern_amplitude = 1,
		pattern_frequency = -1
	)
	complete <- function(..., key_zoom = TRUE) {
		complete_params(
			list(...),
			"swatch",
			defaults = defaults,
			key_zoom = key_zoom,
			key_scaling = key_scaling
		)
	}

	# the key scale factor default sees the (registered or user) spacing
	params <- complete()
	expect_equal(seen$ksf_spacing, 0.025)
	expect_equal(params$pattern_key_scale_factor, 1)
	params <- complete(pattern_spacing = 0.1)
	expect_equal(seen$ksf_spacing, 0.1)
	expect_equal(params$pattern_key_scale_factor, 0.25)
	expect_equal(params$pattern_spacing, 0.1) # "snpc" lengths aren't zoomed
	expect_equal(params$pattern_frequency, 10)
	expect_equal(complete(pattern_spacing = 0.1, key_zoom = FALSE)$pattern_key_scale_factor, 0.25)

	# absolute `key_scaling` parameters are zoomed after all defaults are filled in
	# whether supplied or defaults (an inverse length by `1 / k`)
	params <- complete(pattern_spacing = 0.1, pattern_yoffset = 1, pattern_units = "cm")
	expect_equal(seen$ksf_spacing, 0.1)
	expect_equal(params$pattern_spacing, 0.025)
	expect_equal(params$pattern_yoffset, 0.25)
	expect_equal(params$pattern_xoffset, 0)
	expect_equal(params$pattern_amplitude, 0.0125)
	expect_equal(params$pattern_frequency, 40)
	params <- complete(
		pattern_spacing = 0.1,
		pattern_amplitude = 0.2,
		pattern_frequency = 2,
		pattern_units = "cm"
	)
	expect_equal(params$pattern_amplitude, 0.05)
	expect_equal(params$pattern_frequency, 8)
	params <- complete(pattern_spacing = 0.1, pattern_units = "cm", key_zoom = FALSE)
	expect_equal(params$pattern_spacing, 0.1)
	expect_equal(params$pattern_amplitude, 0.05)
	expect_equal(params$pattern_frequency, 10)

	# only the parameters in `key_scaling` are zoomed
	params <- complete_params(
		list(pattern_spacing = 0.1, pattern_xoffset = 1, pattern_units = "cm"),
		"swatch",
		defaults = defaults,
		key_zoom = TRUE,
		key_scaling = c(pattern_xoffset = 1)
	)
	expect_equal(params$pattern_spacing, 0.1)
	expect_equal(params$pattern_xoffset, 0.25)
	expect_equal(params$pattern_amplitude, 0.05)

	# a `pattern_units` default may also use earlier parameters
	defaults <- list(
		pattern_spacing = 0.3,
		pattern_units = function(params, gp) if (params$pattern_spacing > 0.2) "cm" else "snpc"
	)
	params <- complete(pattern_key_scale_factor = 2)
	expect_equal(params$pattern_units, "cm")
	expect_equal(params$pattern_spacing, 0.6)
	params <- complete(pattern_key_scale_factor = 2, pattern_spacing = 0.1)
	expect_equal(params$pattern_units, "snpc")
	expect_equal(params$pattern_spacing, 0.1)
})

test_that("`key_scaling` of registered patterns", {
	skip_if_not_installed("withr")
	draw <- function(...) {
		draw_legend_capture(1, units = "cm", key_scale_factor = 2, ...)$params
	}
	local_pattern("legend_capture", create_pattern_legend_capture, env = globalenv())
	params <- draw(spacing = 1, amplitude = 1, frequency = 1)
	expect_equal(params$pattern_spacing, 2)
	expect_equal(params$pattern_amplitude, 1)
	expect_equal(params$pattern_frequency, 1)

	local_pattern(
		"legend_capture",
		create_pattern_legend_capture,
		key_scaling = c(pattern_amplitude = 1, pattern_frequency = -1),
		env = globalenv()
	)
	params <- draw(spacing = 1, amplitude = 1, frequency = 1)
	expect_equal(params$pattern_spacing, 1)
	expect_equal(params$pattern_amplitude, 2)
	expect_equal(params$pattern_frequency, 0.5)

	local_pattern(
		"legend_capture",
		create_pattern_legend_capture,
		key_scaling = NULL,
		env = globalenv()
	)
	expect_equal(draw(spacing = 1)$pattern_spacing, 1)

	# the builtin "wave" also zooms its amplitude and frequency, "rose" doesn't zoom its frequency
	expect_equal(
		resolve_pattern("gridpattern::wave")$key_scaling,
		c(
			pattern_spacing = 1,
			pattern_xoffset = 1,
			pattern_yoffset = 1,
			pattern_amplitude = 1,
			pattern_frequency = -1
		)
	)
	expect_equal(resolve_pattern("gridpattern::rose")$key_scaling, DEFAULT_KEY_SCALING)

	f <- create_pattern_legend_capture
	for (bad in list("pattern_spacing", c(1, 2), c(pattern_spacing = NA), c(a = 1, a = 2))) {
		expect_error(
			register_pattern("key_capture", f, key_scaling = bad, env = globalenv()),
			"`key_scaling` must be"
		)
	}
	expect_error(
		register_pattern("key_capture", f, key_scaling = c(spacing = 1), env = globalenv()),
		"`key_scaling` has names without the `pattern_` prefix"
	)
})

test_that("the default `pattern_res` is computed from the graphics device when drawn", {
	skip_if_not_installed("withr")
	local_pattern("legend_capture", create_pattern_legend_capture, env = globalenv())
	draw_res <- function(...) {
		grid.newpage()
		grid.pattern("legend_capture", ...)
		legend_captured$params$pattern_res
	}
	with_temp_dev(function() grDevices::pdf(NULL), {
		expect_equal(draw_res(), 1.14 * 72)
		expect_equal(draw_res(legend = TRUE), 1.14 * 72)
		# a numeric legend key's default resolution scales with the key's zoom
		expect_equal(draw_res(legend = 1), 1.3 * 72)
		expect_equal(draw_res(legend = 2.5, key_scale_factor = 2), 1.3 * 72 * 5)
		# a user-set resolution isn't scaled
		expect_equal(draw_res(legend = 2.5, res = 50), 50)
		expect_equal(draw_res(legend = 2.5, res = NaN), 1.3 * 72 * 2.5)
		withr::with_options(list(ggpattern_res = 100), {
			expect_equal(draw_res(), 100)
			expect_equal(draw_res(legend = 2), 200)
		})
	})
	skip_if_not(capabilities("png"))
	png_file <- withr::local_tempfile(fileext = ".png")
	with_temp_dev(function() grDevices::png(png_file, res = 150), {
		expect_equal(draw_res(), 1.14 * 150)
	})
	# a grob created on one device uses the resolution of the device it is drawn on
	grob <- patternGrob("legend_capture")
	with_temp_dev(function() grDevices::png(png_file, res = 300), {
		grid.draw(grob)
		expect_equal(legend_captured$params$pattern_res, 1.14 * 300)
	})
})
