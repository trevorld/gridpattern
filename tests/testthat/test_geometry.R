# errors thrown by pattern functions while drawing leave the graphics device locked
# (and open a default device if none is open) so check them with `makeContent()` instead
expect_pattern_error <- function(grob, ...) {
	local_temp_dev(function() grDevices::pdf(NULL))
	expect_error(makeContent(grob), ...)
}

# the grob a `pattern` draws (within the unit square) in the current viewport
# (a numeric `legend` key computes its pattern within its own zoomed viewport)
pattern_content_grob <- function(pattern, ...) {
	grob <- grid.pattern(c(0, 1, 1, 0), c(0, 0, 1, 1), pattern = pattern, draw = FALSE, ...)
	content <- makeContent(grob)$children[[1L]]
	if (inherits(content, "pattern_legend")) {
		pushViewport(content$vp)
		content <- makeContent(content)$children[[1L]]
		popViewport()
	}
	expect_no_error(grid.draw(grob))
	content
}
# whether a grob draws nothing i.e. it is a null grob or a `gTree()` of such grobs
draws_nothing <- function(grob) {
	inherits(grob, "null") ||
		(inherits(grob, "gTree") && all(vapply(grob$children, draws_nothing, logical(1L))))
}

context("geometry")
test_that("geometry helpers work as expected", {
	xy <- rotate_xy(c(0, 1), c(0, 1), 90)
	expect_equal(xy$x, c(1, 0))
	expect_equal(xy$y, c(0, 1))
})
test_that("`pattern_units` can't be \"npc\", \"native\", nor \"null\"", {
	for (units in c("npc", "native", "null")) {
		msg <- sprintf('`pattern_units` can\'t be "%s"', units)
		expect_error(patternGrob("stripe", units = units), msg)
		expect_error(patternGrob("stripe", pattern_units = units, prefix = ""), msg)
		# also checked when drawn (e.g. if edited after creation)
		grob <- editGrob(patternGrob("stripe"), args = list(pattern_units = units))
		expect_pattern_error(grob, msg)
		grob <- editGrob(patternGrob("wave", legend = 2), args = list(pattern_units = units))
		expect_pattern_error(grob, msg)
		expect_error(patternGrob("image", units = units), msg)
	}
})
test_that("geometry patterns work as expected", {
	png_file <- withr::local_tempfile(fileext = ".png")
	with_temp_dev(function() grDevices::png(png_file), {
		expect_error(grid.pattern_crosshatch(x, y, density = 1.1))
		expect_error(grid.pattern_stripe(x, y, density = 1.1))
	})

	skip_if_not_installed("vdiffr")
	skip_on_ci()

	vdiffr::expect_doppelganger("default", grid.pattern)

	x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
	y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
	vdiffr::expect_doppelganger("circle", function() {
		grid.pattern_circle(x, y, color = "blue", fill = "yellow", size = 2, density = 0.5)
	})

	vdiffr::expect_doppelganger("crosshatch", function() {
		grid.pattern_crosshatch(
			x,
			y,
			color = "black",
			fill = "blue",
			fill2 = "yellow",
			density = 0.5
		)
	})

	vdiffr::expect_doppelganger("fill", function() {
		grid.pattern_fill(x, y, fill = "blue", alpha = 0.5)
	})

	vdiffr::expect_doppelganger("none", function() {
		grid.pattern_none(x, y)
	})

	expect_error(assert_rp_shape(1), "Unknown shape 1")
	expect_null(assert_rp_shape(c("square", "convex4")))
	expect_null(assert_rp_shape(c("star5", "circle", "null")))
	vdiffr::expect_doppelganger("regular_polygon", function() {
		grid.pattern_regular_polygon(
			x,
			y,
			shape = "convex4",
			color = "black",
			fill = "blue",
			density = 0.5
		)
	})

	vdiffr::expect_doppelganger("hexagon", function() {
		grid.pattern_regular_polygon(
			x,
			y,
			color = "transparent",
			fill = c("white", "grey", "black"),
			density = 1.0,
			shape = "convex6",
			grid = "hex"
		)
	})

	vdiffr::expect_doppelganger("square", function() {
		grid.pattern_regular_polygon(
			x,
			y,
			color = "black",
			fill = c("white", "grey"),
			density = 1.0,
			shape = "square"
		)
	})

	vdiffr::expect_doppelganger("eight_sided_star", function() {
		grid.pattern_regular_polygon(
			x,
			y,
			colour = "black",
			fill = c("blue", "yellow"),
			density = 1.0,
			spacing = 0.1,
			shape = "star8"
		)
	})
	vdiffr::expect_doppelganger("stripe", function() {
		grid.pattern_stripe(x, y, color = "black", fill = c("yellow", "blue"), density = 0.5)
	})

	vdiffr::expect_doppelganger("stripe_gpar", function() {
		x <- c(0.1, 0.6, 0.8, 0.3)
		y <- c(0.2, 0.3, 0.8, 0.5)
		grid.pattern("stripe", x, y, gp = gpar(col = "blue", fill = "red", lwd = 2))
	})

	vdiffr::expect_doppelganger("wave_wavy", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "wavy",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_indented", function() {
		grid.pattern_wave(
			x,
			y,
			color = "black",
			fill = "yellow",
			type = "indented",
			density = 0.5,
			spacing = 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_sawtooth", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "sawtoothed",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_sawtooth_reverse", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "sawtoothed",
			reverse = TRUE,
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "embattled",
			fill = c("red", "blue"),
			density = 0.6,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_embattled", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "embattled",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_wavy_stagger", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "wavy",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			stagger = TRUE
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			fill = "yellow",
			type = "indented",
			density = 0.5,
			spacing = 0.15,
			stagger = TRUE,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_indented_stagger", function() {
		grid.pattern_wave(
			x,
			y,
			color = "black",
			fill = "yellow",
			type = "indented",
			density = 0.3,
			spacing = 0.15,
			amplitude = 0.03,
			stagger = TRUE
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "urdy",
			fill = c("red", "blue"),
			density = 0.6,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_urdy", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "urdy",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "dovetailed",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_dovetailed", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "dovetailed",
			fill = c("red", "blue"),
			density = 0.2,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_engrailed", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "engrailed",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_invected", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "invected",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "nebuly",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_nebuly", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "nebuly",
			fill = c("red", "blue"),
			density = 0.2,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "potenty",
			fill = c("red", "blue"),
			density = 0.4,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_potenty", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "potenty",
			fill = c("red", "blue"),
			density = 0.2,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "embattled_grady",
			fill = c("red", "blue"),
			density = 0.5,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_embattled_grady", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "embattled_grady",
			fill = c("red", "blue"),
			density = 0.2,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	expect_pattern_error(
		grid.pattern_wave(
			x,
			y,
			type = "raguly",
			fill = c("red", "blue"),
			density = 0.5,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15,
			draw = FALSE
		),
		"overlap"
	)

	vdiffr::expect_doppelganger("wave_raguly", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "raguly",
			fill = c("red", "blue"),
			density = 0.3,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("wave_raguly_reverse", function() {
		grid.pattern_wave(
			x,
			y,
			colour = "black",
			type = "raguly",
			reverse = TRUE,
			fill = c("red", "blue"),
			density = 0.3,
			spacing = 0.15,
			angle = 0,
			amplitude = 0.05,
			frequency = 1 / 0.15
		)
	})

	vdiffr::expect_doppelganger("weave", function() {
		grid.pattern_weave(
			x,
			y,
			color = "black",
			fill = "yellow",
			fill2 = "blue",
			type = "twill",
			density = 0.5
		)
	})

	centroid_dot_pattern <- function(params, boundary_df, aspect_ratio, legend) {
		boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)
		centroid <- sf::st_centroid(boundary_sf)
		grid::pointsGrob(
			x = centroid[1],
			y = centroid[2],
			pch = params$pattern_shape,
			size = unit(params$pattern_size, 'char'),
			default.units = "npc",
			gp = grid::gpar(col = update_alpha(params$pattern_fill, params$pattern_alpha))
		)
	}
	withr::local_options(ggpattern_geometry_funcs = list(centroid = centroid_dot_pattern))
	x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
	y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
	vdiffr::expect_doppelganger("centroid", function() {
		grid.pattern("centroid", x, y, fill = "blue", size = 5)
	})

	x <- c(0, 0, 0.5, 0.5, 0.5, 0.5, 1, 1)
	y <- c(0, 0.5, 0.5, 0, 0.5, 1, 1, 0.5)
	id <- rep(1:2, each = 4L)
	vdiffr::expect_doppelganger("two_id", function() {
		grid.pattern(x = x, y = y, id = id)
	})
})
test_that("get_xy_grid() bounding box is non-degenerate when spacing exceeds viewport (issue #95)", {
	# In ggpattern legend keys, `pattern_spacing` (after unit conversion) can exceed
	# `vpm$length`. When that happens `x_seq` collapses to `c(0)`, so `x = c(vpm$x)` and
	# `x_min == x_max`, producing a zero-width rectangle that `sf::st_intersection()` silently
	# discards — the pattern disappears entirely.
	params <- complete_params(list(pattern_spacing = 20, pattern_grid = "square")) # spacing already in same units as vpm
	vpm <- list(width = 10, height = 10, length = 10, x = 5, y = 5)

	grid_xy <- get_xy_grid(params, vpm)

	expect_gt(grid_xy$x_max - grid_xy$x_min, 0)
	expect_gt(grid_xy$y_max - grid_xy$y_min, 0)
})
test_that("get_xy_par() selects the grid points of a pattern matrix entry in row-major order (issue #119)", {
	grid_xy <- list(x = c(10, 20, 30), y = c(1, 2, 3, 4))
	m_pat <- matrix(c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2), nrow = 4L, byrow = TRUE) # checkerboard

	expect_equal(
		get_xy_par(grid_xy, 1L, m_pat, "square", 10),
		list(x = c(10, 30, 20, 10, 30, 20), y = c(1, 1, 2, 3, 3, 4))
	)
	expect_equal(
		get_xy_par(grid_xy, 2L, m_pat, "square", 10),
		list(x = c(20, 10, 30, 20, 10, 30), y = c(1, 2, 2, 3, 4, 4))
	)
	# even rows are offset half a spacing to the left
	expect_equal(
		get_xy_par(grid_xy, 1L, m_pat, "hex", 10),
		list(x = c(10, 30, 15, 10, 30, 15), y = c(1, 1, 2, 3, 3, 4))
	)
	# rows 1, 2 (mod 4) are offset half a spacing to the left
	expect_equal(
		get_xy_par(grid_xy, 1L, m_pat, "elongated_triangle", 10),
		list(x = c(5, 25, 15, 10, 30, 20), y = c(1, 1, 2, 3, 3, 4))
	)
	# restricted to some rows and columns (the offsets still depend on the original row)
	expect_equal(
		get_xy_par(grid_xy, 1L, m_pat, "hex", 10, rows = 2:3, cols = 2:3),
		list(x = c(15, 30), y = c(2, 3))
	)
	# an entry that never occurs
	expect_equal(get_xy_par(grid_xy, 3L, m_pat, "square", 10), list(x = numeric(0), y = numeric(0)))
	expect_equal(get_xy_par(grid_xy, 3L, m_pat, "hex", 10), list(x = numeric(0), y = numeric(0)))
})
test_that("get_xy_par_within() keeps the rotated grid points within a bounding box (issue #119)", {
	vpm <- list(x = 50, y = 50, length = 100)
	grid_xy <- list(x = seq(-50, 150, by = 10), y = seq(-50, 150, by = 10))
	m_pat <- matrix(1L, nrow = length(grid_xy$y), ncol = length(grid_xy$x))
	# the brute force equivalent: select, rotate, and filter all the grid points
	within_all <- function(grid, angle, bbox) {
		xy <- get_xy_par(grid_xy, 1L, m_pat, grid, 10)
		filter_xy_bbox(rotate_xy(xy$x, xy$y, angle, vpm$x, vpm$y), bbox)
	}
	bbox <- boundary_bbox(create_polygon_df(c(40, 60, 60, 40), c(0, 0, 100, 100)), margin = 5)
	for (grid in c("square", "hex", "elongated_triangle")) {
		for (angle in c(0, 30, 90, 135)) {
			expect_equal(
				get_xy_par_within(grid_xy, 1L, m_pat, grid, 10, angle, vpm, bbox),
				within_all(grid, angle, bbox)
			)
		}
	}
	# (`rotate_xy()` goes through polar coordinates so even a zero rotation isn't exact)
	xy <- get_xy_par_within(grid_xy, 1L, m_pat, "square", 10, 0, vpm, bbox)
	expect_equal(sort(unique(round(xy$x, 6))), c(40, 50, 60))
	expect_length(xy$y, 3L * 11L)
	# nothing within an `NA` bounding box
	bbox_na <- boundary_bbox(create_polygon_df(c(0, NA, 1), c(0, 0, 1)))
	expect_equal(
		get_xy_par_within(grid_xy, 1L, m_pat, "square", 10, 0, vpm, bbox_na),
		list(x = numeric(0), y = numeric(0))
	)
})
test_that("filter_xy_bbox() keeps the points within the (expanded) boundary bounding box", {
	boundary_df <- create_polygon_df(c(0, 1, 1, 0), c(0, 0, 1, 1))
	expect_equal(boundary_bbox(boundary_df), c(xmin = 0, xmax = 1, ymin = 0, ymax = 1))
	expect_equal(
		boundary_bbox(boundary_df, margin = 0.5),
		c(xmin = -0.5, xmax = 1.5, ymin = -0.5, ymax = 1.5)
	)

	xy <- list(x = c(0.5, 2, -1, 0), y = c(0.5, 0.5, 0.5, 1))
	expect_equal(filter_xy_bbox(xy, boundary_bbox(boundary_df)), list(x = c(0.5, 0), y = c(0.5, 1)))
	expect_equal(filter_xy_bbox(xy, boundary_bbox(boundary_df, margin = 1)), xy)
	# a boundary with a missing coordinate has an `NA` bounding box (on that axis): keep nothing
	bbox_na <- boundary_bbox(create_polygon_df(c(0, NA, 1), c(0, 0, 1)))
	expect_equal(bbox_na, c(xmin = NA_real_, xmax = NA_real_, ymin = 0, ymax = 1))
	expect_equal(filter_xy_bbox(xy, bbox_na), list(x = numeric(0), y = numeric(0)))
})
test_that("tile_xy_polygon() centres a polygon on each point", {
	triangle <- list(x = c(0, 1, 0), y = c(0, 0, 1))
	expect_equal(
		tile_xy_polygon(c(10, 20), c(1, 2), triangle),
		list(x = c(10, 11, 10, 20, 21, 20), y = c(1, 1, 2, 2, 2, 3), id = c(1L, 1L, 1L, 2L, 2L, 2L))
	)
	expect_equal(
		tile_xy_polygon(numeric(0), numeric(0), triangle),
		list(x = numeric(0), y = numeric(0), id = integer(0))
	)
	rings <- xy_rings(c(10, 20), c(1, 2), list(x = c(0, 1, 0, 0), y = c(0, 0, 1, 0)))
	expect_length(rings, 2L)
	expect_equal(rings[[2L]][[1L]], cbind(c(20, 21, 20, 20), c(2, 2, 3, 2)))
})
test_that("glyph_radius() bounds the extent of a text glyph from its centre", {
	local_temp_dev()
	gp <- gpar(fontsize = 12)
	r <- glyph_radius("X", gp)
	w <- convertWidth(grobWidth(textGrob("X", gp = gp)), "bigpts", valueOnly = TRUE)
	expect_gt(r, w / 2)
	expect_gt(glyph_radius("XX", gp), r)
	expect_gt(glyph_radius("X", gpar(fontsize = 24)), r)
})
test_that("patterns draw nothing instead of erroring in a viewport thinner than a device pixel (issue #119)", {
	# e.g. a ggplot2 panel squeezed to nothing by a wide legend on a small device:
	# the default "snpc" spacing is then tiny (a huge grid) or non-positive (a `seq()` error)
	local_temp_dev() # `pdf(NULL)` so a device pixel is one bigpt
	patterns <- c(
		"circle",
		"crosshatch",
		"hatch",
		"line",
		"pch",
		"plasma",
		"polygon_tiling",
		"regular_polygon",
		"rose",
		"stripe",
		"text",
		"wave",
		"weave"
	)
	expect_null_patterns <- function(width, height, ...) {
		pushViewport(viewport(width = width, height = height))
		on.exit(popViewport())
		for (pattern in patterns) {
			expect_s3_class(pattern_content_grob(pattern, ...), "null")
		}
	}
	# (nor is anything drawn in a mirrored viewport i.e. a negative width / height)
	for (thin in c(0.5, 0, -0.5, -5)) {
		expect_null_patterns(unit(thin, "bigpts"), unit(2, "in"))
		expect_null_patterns(unit(2, "in"), unit(thin, "bigpts"))
	}
	expect_null_patterns(unit(-2, "in"), unit(1, "in"))
	# a numeric legend key is drawn in a viewport zoomed `legend` times (which is also degenerate)
	expect_null_patterns(unit(0, "bigpts"), unit(2, "in"), legend = 2)
	expect_null_patterns(unit(-2, "in"), unit(0.25, "bigpts"), legend = 2)
})
test_that("regular polygons draw in a boundary too narrow for any interior polygon", {
	# (no interior points: `sf::st_intersection()` returns an empty geometry collection)
	local_temp_dev()
	x <- c(0.46, 0.49, 0.49, 0.46)
	expect_false(draws_nothing(pattern_content_grob(
		"regular_polygon",
		shape = "convex4",
		spacing = 0.05
	)))
	grob <- grid.pattern_regular_polygon(
		x,
		c(0.46, 0.46, 0.49, 0.49),
		shape = "convex4",
		spacing = 0.05,
		draw = FALSE
	)
	expect_no_error(makeContent(grob))
	grob <- grid.pattern_polygon_tiling(x, c(0, 0, 1, 1), spacing = 0.05, draw = FALSE)
	expect_no_error(makeContent(grob))
})
test_that("the \"cendree\" hatch draws nothing (quietly) for a boundary outside the viewport", {
	local_temp_dev()
	grob <- grid.pattern_hatch(c(3, 4, 4, 3), c(0, 0, 1, 1), type = "cendree", draw = FALSE)
	expect_no_warning(content <- makeContent(grob)$children[[1L]])
	expect_s3_class(content, "null")
	# or with a missing coordinate
	grob <- grid.pattern_hatch(c(0, NA, 1, 0), c(0, 0, 1, 1), type = "cendree", draw = FALSE)
	expect_no_warning(content <- makeContent(grob)$children[[1L]])
	expect_s3_class(content, "null")
})
test_that("patterns with an absurdly dense grid draw nothing instead of exhausting memory (issue #119)", {
	expect_silent(check_grid_density(list(x = seq_len(3000), y = seq_len(3000))))
	expect_error(
		check_grid_density(list(x = seq_len(4000), y = seq_len(4000))),
		class = "gridpattern_grid_too_dense"
	)

	local_temp_dev() # a 7in by 7in `pdf(NULL)` device
	# ("pch" and "polygon_tiling" wrap the null grobs of their "regular_polygon" patterns in a `gTree()`)
	patterns <- c("circle", "pch", "polygon_tiling", "regular_polygon", "rose", "text", "weave")
	for (pattern in patterns) {
		# 20000 by 20000 grid points
		expect_true(draws_nothing(pattern_content_grob(pattern, spacing = 1e-4)))
	}
	expect_true(draws_nothing(pattern_content_grob("hatch", type = "cendree", spacing = 1e-4)))
	# but a merely dense grid (200 by 200 points) is drawn
	expect_false(draws_nothing(pattern_content_grob("circle", spacing = 0.01)))
})
test_that("every lobe of a self-intersecting boundary is filled", {
	# the edges of this "bow tie" cross at (0.5, 0.5)
	x <- c(0, 1, 0, 1)
	y <- c(0, 1, 1, 0)
	bowtie_sf <- convert_polygon_df_to_polygon_sf(create_polygon_df(x, y))
	expect_true(sf::st_is_valid(bowtie_sf))
	expect_equal(sf::st_area(bowtie_sf), 0.5) # (previously `0.25`: one lobe)
	# (a boundary with a degenerate i.e. zero-area polygon still buffers like a line)
	collinear_sf <- convert_polygon_df_to_polygon_sf(
		create_polygon_df(c(0, 1, 2), c(0, 0, 0)),
		buffer_dist = 0.1
	)
	expect_equal(sf::st_area(collinear_sf), 0.1 * (2 * 2 + pi * 0.1), tolerance = 0.01)

	vdiffr::expect_doppelganger("bowtie_stripe", function() {
		grid.pattern_stripe(x, y, fill = "blue", density = 0.4)
	})
	vdiffr::expect_doppelganger("bowtie_crosshatch", function() {
		grid.pattern_crosshatch(x, y, fill = "blue", fill2 = "red", density = 0.4)
	})
	vdiffr::expect_doppelganger("bowtie_circle", function() {
		grid.pattern_circle(x, y, fill = "blue", density = 0.6, spacing = 0.1)
	})
})
