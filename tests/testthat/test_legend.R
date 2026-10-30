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
	grDevices::pdf(NULL, width = 2, height = 2)
	on.exit(grDevices::dev.off())
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
	grDevices::pdf(NULL, width = 2, height = 2)
	on.exit(grDevices::dev.off())
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
