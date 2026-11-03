context("pch")
test_that("pch patterns work as expected", {
	skip_if_not_installed("vdiffr")
	skip_on_ci()

	x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
	y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
	gp <- gpar(fill = "yellow", col = "black")
	g.pp <- function(shape, ...) {
		grid.pattern_pch(x, y, shape = shape, angle = 0, spacing = 0.15, gp = gp)
	}

	vdiffr::expect_doppelganger("simple", function() g.pp(0:6))
	vdiffr::expect_doppelganger("compound", function() g.pp(7:14))
	vdiffr::expect_doppelganger("col_fill", function() g.pp(15:20))
	vdiffr::expect_doppelganger("fill_fill", function() g.pp(21:25))
})
