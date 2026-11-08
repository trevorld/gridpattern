context("as.patternGrob")

# the boundary of a pattern grob (with a stored boundary grob) in the current viewport
pattern_boundary <- function(grob) {
	boundary_coords(grob$boundary)
}

test_that("`as.patternGrob()` of a polygon reuses its coordinates", {
	x <- c(0, 0, 0.4, 0.4, 0.6, 0.6, 1, 1)
	y <- c(0, 0.4, 0.4, 0, 0.6, 1, 1, 0.6)
	id <- rep(1:2, each = 4L)
	vp <- viewport(width = 0.5)
	polygon <- polygonGrob(x, y, id.lengths = c(4L, 4L), default.units = "native", vp = vp)
	grob <- as.patternGrob(polygon, "stripe", fill = "red")
	expect_s3_class(grob, "pattern")
	expect_null(grob$boundary)
	expect_equal(grob$x, unit(x, "native"))
	expect_equal(grob$y, unit(y, "native"))
	expect_equal(grob$id, id)
	expect_equal(grob$args, list(pattern_fill = "red"))
	expect_identical(grob$vp, vp)
	expect_equal(as.patternGrob(polygonGrob(x, y, id = id))$id, id)
	expect_equal(as.patternGrob(polygonGrob())$id, rep_len(1L, 4L))
	# `vp` and the polygon's `vp` are stacked
	grob <- as.patternGrob(polygon, vp = viewport(height = 0.5))
	expect_s3_class(grob$vp, "vpStack")
	expect_identical(as.patternGrob(polygonGrob(), vp = vp)$vp, vp)

	# draws the same as `patternGrob()`
	local_temp_dev()
	grob <- as.patternGrob(polygon, "stripe", fill = "red", vp = NULL)
	expected <- patternGrob("stripe", x, y, id, fill = "red", default.units = "native", vp = vp)
	content <- makeContent(grob)$children[[1L]]
	expected_content <- makeContent(expected)$children[[1L]]
	expect_equal(content$x, expected_content$x)
	expect_equal(content$y, expected_content$y)
	expect_equal(content$gp, expected_content$gp)
	expect_equal(grobCoords(grob, closed = TRUE)[[1L]], grobCoords(expected, closed = TRUE)[[1L]])
	expect_no_error(grid.draw(grob))
	# a polygon edited in as the `boundary` is measured when drawn (on R 4.1 too)
	expect_true(has_boundary_coords(polygonGrob()))
	grob <- editGrob(as.patternGrob(rectGrob(), "stripe"), boundary = polygonGrob(x, y, id = id))
	b <- pattern_boundary(grob)
	expect_equal(b$x, x)
	expect_equal(b$y, y)
	expect_equal(b$id, id)
	expect_null(b$masker)
	expect_no_error(grid.draw(grob))
})

test_that("`as.patternGrob()` of a path fills by its rule and masks its holes", {
	local_temp_dev()
	x_outer <- c(0, 1, 1, 0) # counter-clockwise
	y_outer <- c(0, 0, 1, 1)
	x_inner <- c(0.25, 0.75, 0.75, 0.25) # counter-clockwise
	y_inner <- c(0.25, 0.25, 0.75, 0.75)
	path <- function(rule, reverse = FALSE) {
		i <- if (reverse) 4:1 else 1:4
		pathGrob(
			c(x_outer, x_inner[i]),
			c(y_outer, y_inner[i]),
			id = rep(1:2, each = 4L),
			rule = rule,
			gp = gpar(fill = NA, col = "red", lwd = 4)
		)
	}
	# "evenodd": the inner ring is a hole whatever its orientation
	for (reverse in c(FALSE, TRUE)) {
		grob <- as.patternGrob(path("evenodd", reverse), "stripe")
		expect_s3_class(grob$boundary, "pathgrob")
		expect_null(grob$boundary$vp)
		b <- pattern_boundary(grob)
		# the exterior ring is the boundary polygon while the mask cuts out the hole
		expect_equal(b$id, rep(1L, 4L))
		expect_equal(range(b$x), c(0, 1))
		expect_equal(range(b$y), c(0, 1))
		expect_s3_class(b$masker, "pathgrob")
		expect_equal(b$masker$gp$fill, "black")
		expect_equal(b$masker$gp$col, NA)
		expect_equal(b$masker$gp$lwd, 0)
		expect_equal(b$masker$rule, "evenodd")
	}
	# "winding": an inner ring of the same orientation is filled, of the opposite one is a hole
	b <- pattern_boundary(as.patternGrob(path("winding")))
	expect_null(b$masker)
	expect_true(all(b$id == 1L))
	expect_equal(range(b$x), c(0, 1))
	b <- pattern_boundary(as.patternGrob(path("winding", reverse = TRUE)))
	expect_s3_class(b$masker, "pathgrob")
	expect_equal(b$id, rep(1L, 4L))
	# paths of one ring each are simply polygons
	two <- pathGrob(
		c(0, 0.4, 0.4, 0, 0.6, 1, 1, 0.6),
		c(0, 0, 1, 1, 0, 0, 1, 1),
		id.lengths = c(4L, 4L),
		pathId.lengths = c(4L, 4L)
	)
	b <- pattern_boundary(as.patternGrob(two))
	expect_null(b$masker)
	expect_equal(b$id, rep(1:2, each = 4L))
	expect_equal(b$x, as.numeric(two$x))
	# a path with a hole and one without
	three <- pathGrob(
		c(x_outer, x_inner, 2:3, 3, 2),
		c(y_outer, y_inner, 0, 0, 1, 1),
		id = rep(1:3, each = 4L),
		pathId = rep(1:2, c(8L, 4L)),
		rule = "evenodd"
	)
	b <- pattern_boundary(as.patternGrob(three))
	expect_equal(b$id, rep(1:2, each = 4L))
	expect_s3_class(b$masker, "pathgrob")

	# the mask honours the R 4.1 pattern parameters like the builtin patterns' own masks do
	grob <- as.patternGrob(path("evenodd"), "stripe", use_R4.1_masks = FALSE)
	mask <- makeContent(grob)$children[[1L]]
	expect_s3_class(mask, "alpha_mask")
	expect_false(mask$use_R4.1_masks)
	expect_equal(mask$res, default_res())
	# a zoomed legend key's mask is drawn in the key viewport so it keeps the unzoomed resolution
	grob <- as.patternGrob(path("evenodd"), "stripe", legend = 2, use_R4.1_masks = FALSE)
	mask <- makeContent(grob)$children[[1L]]
	expect_equal(mask$res, default_res(legend = TRUE))
	expect_equal(mask$maskee$params$pattern_res, 2 * default_res(legend = TRUE))
	# `grobCoords()` is the path's
	grob <- as.patternGrob(path("evenodd"), name = "pattern")
	coords <- grobCoords(grob, closed = TRUE)
	expect_length(coords, 2L)
	expect_equal(attr(coords, "name"), "pattern")
	expect_no_error(grid.draw(as.patternGrob(path("evenodd"), "stripe")))
	expect_no_error(grid.draw(as.patternGrob(path("evenodd"), "stripe", legend = 2)))
})

test_that("`as.patternGrob()` of a rect uses its corners", {
	local_temp_dev()
	rect <- rectGrob(
		x = c(0.25, 0.75),
		y = 0.5,
		width = unit(1, "in"),
		height = 0.5,
		just = c("left", "bottom")
	)
	grob <- as.patternGrob(rect, "stripe")
	expect_s3_class(grob$boundary, "rect")
	b <- pattern_boundary(grob)
	w <- convertWidth(unit(1, "in"), "npc", valueOnly = TRUE)
	expect_equal(b$id, rep(1:2, each = 4L))
	expect_equal(b$x[1:4], c(0.25, 0.25 + w, 0.25 + w, 0.25))
	expect_equal(b$x[5:8], c(0.75, 0.75 + w, 0.75 + w, 0.75))
	expect_equal(b$y[1:4], c(0.5, 0.5, 1, 1))
	expect_null(b$masker)
	expect_equal(pattern_boundary(as.patternGrob(rectGrob()))$x, c(0, 1, 1, 0))
	# `width` / `height` are dimensions (not locations) so "native" units differ by the scale origin
	vp <- viewport(xscale = c(10, 20), yscale = c(10, 30))
	rect <- rectGrob(
		x = 15,
		y = 10,
		width = 2,
		height = 4,
		default.units = "native",
		just = c("left", "bottom"),
		vp = vp
	)
	grob <- as.patternGrob(rect)
	expect_null(grob$vp)
	expect_identical(grob$boundary$vp, vp)
	pushViewport(vp)
	b <- pattern_boundary(grob)
	popViewport()
	expect_equal(b$x, c(0.5, 0.7, 0.7, 0.5))
	expect_equal(b$y, c(0, 0, 0.2, 0.2))
	expect_no_error(grid.draw(as.patternGrob(rect, "stripe")))
})

test_that("`as.patternGrob()` of other grobs uses `grobCoords()`", {
	skip_if(getRversion() < "4.2.0")
	local_temp_dev()
	circle <- circleGrob(r = 0.25)
	grob <- as.patternGrob(circle, "stripe")
	b <- pattern_boundary(grob)
	expect_null(b$masker)
	expect_true(all(b$id == 1L))
	expect_gt(length(b$x), 20L)
	expect_equal(range(b$x), c(0.25, 0.75), tolerance = 1e-3)
	expect_equal(range(b$y), c(0.25, 0.75), tolerance = 1e-3)
	expect_null(pattern_boundary(as.patternGrob(roundrectGrob()))$masker)
	# a gTree of exact shapes is its polygons (no mask needed)
	tree <- gTree(
		children = gList(circleGrob(r = 0.25), rectGrob(width = 0.5, height = 0.5)),
		gp = gpar(fill = NA, alpha = 0.5)
	)
	b <- pattern_boundary(as.patternGrob(tree))
	expect_length(unique(b$id), 2L)
	expect_null(b$masker)
	# but one with e.g. text (or a nested path) is masked by itself drawn opaquely
	text <- textGrob("A", gp = gpar(fontsize = 40, col = "red", alpha = 0.5))
	tree <- gTree(
		children = gList(rectGrob(width = 0.5, height = 0.5), text),
		gp = gpar(alpha = 0.5)
	)
	b <- pattern_boundary(as.patternGrob(tree))
	expect_length(unique(b$id), 2L)
	expect_s3_class(b$masker, "gTree")
	expect_equal(b$masker$gp$alpha, 1)
	expect_equal(b$masker$children[[1L]]$gp$fill, "black")
	expect_equal(b$masker$children[[1L]]$gp$col, NA)
	expect_equal(b$masker$children[[2L]]$gp$col, "black")
	expect_s3_class(
		pattern_boundary(as.patternGrob(gTree(
			children = gList(pathGrob(c(0, 1, 1, 0), c(0, 0, 1, 1)))
		)))$masker,
		"gTree"
	)
	b <- pattern_boundary(as.patternGrob(text))
	expect_equal(b$id, rep(1L, 4L))
	expect_equal(b$masker$gp$col, "black")
	expect_equal(b$masker$gp$alpha, 1)
	expect_equal(b$masker$gp$fontsize, 40)
	# stroked (area-less) children contribute no polygons so don't need a mask either
	expect_null(
		pattern_boundary(as.patternGrob(gTree(children = gList(rectGrob(), linesGrob()))))$masker
	)
	# the filled symbols (pch 21 to 25) of points are their outlines (exact, like a circle)
	points <- pointsGrob(
		c(0.25, 0.75),
		c(0.5, 0.5),
		pch = 21,
		default.units = "npc",
		gp = gpar(fontsize = c(20, 40))
	)
	b <- pattern_boundary(as.patternGrob(points))
	expect_null(b$masker)
	expect_equal(as.vector(table(b$id)), c(100L, 100L))
	widths <- tapply(b$x, b$id, function(x) diff(range(x)))
	expect_equal(widths[[2L]] / widths[[1L]], 2, tolerance = 1e-6)
	# the other symbols have no area: e.g. the open pch 1 and the solid pch 19 (painted with `col`)
	points <- pointsGrob(
		c(0.2, 0.5, 0.8),
		c(0.5, 0.5, 0.5),
		pch = c(1, 21, 19),
		size = unit(1, "in"),
		default.units = "npc"
	)
	b <- pattern_boundary(as.patternGrob(points))
	expect_equal(unique(b$id), 1L)
	expect_equal(mean(range(b$x)), 0.5, tolerance = 1e-6)
	expect_no_warning(b <- pattern_boundary(as.patternGrob(pointsGrob(0.5, 0.5, pch = "A"))))
	expect_length(b$x, 0L)
	# within a gTree the filled symbols count (exactly) while the other symbols are blanked
	points <- pointsGrob(
		c(0.25, 0.75),
		c(0.5, 0.5),
		pch = c(21, 19),
		size = unit(2, "in"),
		default.units = "npc"
	)
	tree <- gTree(children = gList(points, rectGrob(width = 0.5, height = 0.5)))
	b <- pattern_boundary(as.patternGrob(tree))
	expect_null(b$masker)
	expect_equal(as.vector(table(b$id)), c(100L, 4L))
	tree <- gTree(children = gList(points, rectGrob(), text))
	grob <- as.patternGrob(tree)
	b <- pattern_boundary(grob)
	expect_equal(as.vector(table(b$id)), c(100L, 4L, 4L))
	expect_length(b$masker$children, 3L)
	expect_equal(b$masker$children[[1L]]$pch, c(21L, NA))
	expect_equal(b$masker$children[[1L]]$gp$fill, "black")
	expect_length(grobCoords(grob, closed = TRUE), 3L)
	# a boundary without a filled area draws nothing
	arealess <- list(
		nullGrob(),
		pointsGrob(),
		pointsGrob(pch = 19),
		linesGrob(),
		rasterGrob(matrix(1))
	)
	for (grob in arealess) {
		grob <- as.patternGrob(grob, "stripe")
		expect_s3_class(makeContent(grob)$children[[1L]], "null")
	}

	# `grobCoords()` is the boundary's
	expect_equal(
		grobCoords(as.patternGrob(circle), closed = TRUE)[[1L]],
		grobCoords(circle, closed = TRUE)[[1L]]
	)
	expect_no_error(grid.draw(as.patternGrob(circle, "stripe")))
	expect_no_error(grid.draw(as.patternGrob(text, "stripe")))
})

test_that("`as.patternGrob()` of a pattern grob returns it unchanged", {
	grob <- patternGrob("stripe", c(0, 0, 1), c(0, 1, 1), fill = "red", vp = viewport(width = 0.5))
	expect_identical(as.patternGrob(grob), grob)
	grob <- as.patternGrob(rectGrob(), "weave")
	expect_identical(as.patternGrob(grob), grob)
	# but a request to change it is an error rather than silently ignored
	expect_error(as.patternGrob(grob, "stripe"), "editGrob")
	expect_error(as.patternGrob(grob, fill = "blue"), "editGrob")
	expect_error(as.patternGrob(grob, vp = viewport()), "editGrob")
	expect_error(as.patternGrob(grob, name = "pattern"), "editGrob")
	expect_identical(lapply(list(grob), as.patternGrob)[[1L]], grob)
})

test_that("`as.patternGrob()` masks the self-intersecting rings of a path", {
	local_temp_dev()
	# a pentagram: how its centre is filled depends on the rule and on the device
	# (grid draws a lone ring as a polygon) so the mask lets grid decide
	t <- seq(pi / 2, by = 4 * pi / 5, length.out = 5)
	for (rule in c("evenodd", "winding")) {
		star <- pathGrob(0.5 + 0.4 * cos(t), 0.5 + 0.4 * sin(t), rule = rule)
		b <- pattern_boundary(as.patternGrob(star))
		expect_equal(b$id, rep(1L, 5L))
		expect_s3_class(b$masker, "pathgrob")
		expect_equal(b$masker$rule, rule)
	}
	# a bow tie (whose lobes wind in opposite directions) overlapping another ring
	bow_tie <- pathGrob(
		c(0, 1, 0, 1, 0.4, 0.4, 0.6, 0.6),
		c(0, 1, 1, 0, 0.4, 0.6, 0.6, 0.4),
		id = rep(1:2, each = 4L),
		rule = "winding"
	)
	b <- pattern_boundary(as.patternGrob(bow_tie))
	expect_equal(b$id, rep(1:2, each = 4L))
	expect_s3_class(b$masker, "pathgrob")
	expect_no_error(grid.draw(as.patternGrob(bow_tie, "stripe")))
})

test_that("`as.patternGrob()` validates its boundary", {
	expect_error(as.patternGrob(c(0, 1, 1), "stripe"), "must be a grid grob")
	expect_error(patternGrob("stripe", x = rectGrob()), "as.patternGrob")
	# the boundary is `boundary` so `x`, `y`, and `id` can't also be given
	expect_error(as.patternGrob(rectGrob(), x = c(0, 1, 1)), "can't be given")
	expect_error(as.patternGrob(polygonGrob(), "stripe", id = 1L), "can't be given")
	if (getRversion() < "4.2.0") {
		expect_error(as.patternGrob(circleGrob()), "R 4.2")
	}
	# the boundary is exactly one of a stored (grob) `boundary` or `x` / `y` / `id`
	grob <- as.patternGrob(rectGrob())
	expect_error(editGrob(grob, x = unit(c(0, 1, 1), "npc")), "`boundary`")
	expect_error(editGrob(patternGrob(), boundary = rectGrob()), "`boundary`")
	expect_error(editGrob(grob, boundary = NULL), "`boundary`")
	expect_error(editGrob(grob, boundary = 1), "grid grob")
	grob2 <- editGrob(grob, boundary = circleGrob())
	expect_s3_class(grob2$boundary, "circle")
	grob3 <- editGrob(patternGrob(), boundary = rectGrob(), x = NULL, y = NULL, id = NULL)
	expect_s3_class(grob3$boundary, "rect")
})

test_that("`as.patternGrob()` honours the viewport of its boundary", {
	local_temp_dev()
	vp <- viewport(x = 0.25, width = 0.5)
	rect <- rectGrob(width = 0.5, vp = vp)
	# the boundary keeps its `vp` which is pushed within the pattern's `vp` when drawn
	grob <- as.patternGrob(rect, "fill", fill = "black", vp = viewport(y = 0.25, height = 0.5))
	expect_identical(grob$boundary$vp, vp)
	expect_identical(grob$vp$y, unit(0.25, "npc"))
	drawn <- makeContext(grob)
	expect_s3_class(drawn$vp, "vpStack")
	expect_null(drawn$boundary$vp)
	# `grobCoords()` are those of the boundary drawn within both viewports
	x_range <- function(coords) {
		x <- unlist(lapply(coords_rings(coords), function(r) r$x))
		range(x) / convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
	}
	grob <- as.patternGrob(rect, "fill", fill = "black")
	expect_equal(x_range(grobCoords(grob, closed = TRUE)), c(0.125, 0.375))
	nested <- as.patternGrob(rect, "fill", fill = "black", vp = viewport(x = 0.75, width = 0.5))
	expect_equal(x_range(grobCoords(nested, closed = TRUE)), c(0.5625, 0.6875))
	path <- pathGrob(c(0.25, 0.75, 0.75, 0.25), c(0, 0, 1, 1), vp = vp)
	edited <- editGrob(as.patternGrob(rectGrob(), "fill", fill = "black"), boundary = path)
	expect_equal(x_range(grobCoords(edited, closed = TRUE)), c(0.125, 0.375))
	circle <- editGrob(edited, boundary = circleGrob(r = 0.25, vp = vp))
	if (getRversion() >= "4.2.0") {
		expect_equal(x_range(grobCoords(circle, closed = TRUE)), c(0.125, 0.375), tolerance = 1e-3)
	}

	# and the pattern is drawn there (whether the boundary was given or edited in)
	skip_if_not(capabilities("cairo"))
	skip_if_not_installed("magick")
	painted_x <- function(grob, width = 200L, height = 100L) {
		f <- withr::local_tempfile(fileext = ".png")
		grDevices::png(f, width = width, height = height, type = "cairo")
		tryCatch(grid.draw(grob), finally = grDevices::dev.off())
		# (`as.integer()` of a `{magick}` bitmap is a rows x columns x channels array)
		gray <- as.integer(magick::image_data(magick::image_read(f), "gray"))[,, 1L]
		painted <- which(colSums(gray < 128L) > 0L)
		(range(painted) - c(1L, 0L)) / width
	}
	expect_equal(painted_x(grob), c(0.125, 0.375), tolerance = 0.01)
	expect_equal(painted_x(nested), c(0.5625, 0.6875), tolerance = 0.01)
	expect_equal(painted_x(edited), c(0.125, 0.375), tolerance = 0.01)
	if (getRversion() >= "4.2.0") {
		expect_equal(painted_x(circle), c(0.125, 0.375), tolerance = 0.01)
	}
})

test_that("`as.patternGrob()` visual tests", {
	skip_if_not_installed("vdiffr")
	skip_on_ci()
	angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6)
	path <- pathGrob(
		x = c(0.5 + 0.5 * cos(angle), 0.5 + 0.25 * cos(angle)),
		y = c(0.5 + 0.5 * sin(angle), 0.5 + 0.25 * sin(angle)),
		id = rep(1:2, each = 6),
		rule = "evenodd",
		gp = gpar(fill = NA, lwd = 4)
	)
	vdiffr::expect_doppelganger("path_hole", function() {
		grid.draw(as.patternGrob(path, "stripe", fill = c("red", "blue"), density = 0.5))
		grid.draw(path)
	})
	vdiffr::expect_doppelganger("path_hole_legend", function() {
		grid.draw(as.patternGrob(
			path,
			"stripe",
			fill = c("red", "blue"),
			density = 0.5,
			legend = 2
		))
		grid.draw(path)
	})
	rect <- rectGrob(
		x = c(0.25, 0.75),
		width = 0.4,
		height = c(0.8, 0.4),
		gp = gpar(fill = NA, lwd = 4)
	)
	vdiffr::expect_doppelganger("rect", function() {
		grid.draw(as.patternGrob(rect, "weave", fill = "yellow", fill2 = "blue"))
		grid.draw(rect)
	})
	skip_if(getRversion() < "4.2.0")
	circle <- circleGrob(r = 0.4, gp = gpar(fill = NA, lwd = 4))
	vdiffr::expect_doppelganger("circle", function() {
		grid.draw(as.patternGrob(circle, "crosshatch", fill = "yellow", fill2 = "blue"))
		grid.draw(circle)
	})
	text <- textGrob("Ab", gp = gpar(fontsize = 150, fontface = "bold"))
	vdiffr::expect_doppelganger("text", function() {
		grid.draw(as.patternGrob(text, "stripe", fill = c("red", "blue"), density = 0.5))
	})
	points <- pointsGrob(
		c(0.3, 0.7),
		c(0.5, 0.5),
		pch = c(21, 24),
		size = unit(2, "in"),
		default.units = "npc",
		gp = gpar(fill = NA, lwd = 4)
	)
	vdiffr::expect_doppelganger("points", function() {
		grid.draw(as.patternGrob(points, "stripe", fill = c("red", "blue"), density = 0.5))
		grid.draw(points)
	})
})
