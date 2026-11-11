context("with_pattern")

test_that("`area_boundaries()` splits a grob into one boundary per area", {
	# polygons by `id` or `id.lengths`
	x <- c(0.1, 0.4, 0.4, 0.1, 0.6, 0.9, 0.75)
	y <- c(0.1, 0.1, 0.4, 0.4, 0.6, 0.6, 0.9)
	polygon <- polygonGrob(x, y, id.lengths = c(4L, 3L), vp = viewport(width = 0.5))
	b <- area_boundaries(polygon)
	expect_s3_class(b, "gList")
	expect_length(b, 2L)
	expect_s3_class(b[[1L]], "polygon")
	expect_equal(b[[1L]]$x, unit(x[1:4], "npc"))
	expect_equal(b[[2L]]$y, unit(y[5:7], "npc"))
	expect_null(b[[1L]]$id)
	expect_null(b[[1L]]$id.lengths)
	expect_identical(b[[1L]]$vp, polygon$vp)
	expect_length(area_boundaries(polygonGrob(x, y, id = c(1, 1, 1, 1, 2, 2, 2))), 2L)
	expect_length(area_boundaries(polygonGrob(x, y)), 1L)

	# rects, circles, and rounded rects by their (recycled) slots
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, height = c(0.8, 0.4))
	b <- area_boundaries(rect)
	expect_length(b, 2L)
	expect_s3_class(b[[2L]], "rect")
	expect_equal(b[[2L]]$x, unit(0.75, "npc"))
	expect_equal(b[[2L]]$width, unit(0.4, "npc"))
	expect_equal(b[[2L]]$height, unit(0.4, "npc"))
	b <- area_boundaries(circleGrob(x = c(0.2, 0.5, 0.8), r = 0.1))
	expect_length(b, 3L)
	expect_s3_class(b[[3L]], "circle")
	expect_equal(b[[3L]]$x, unit(0.8, "npc"))
	expect_equal(b[[3L]]$r, unit(0.1, "npc"))
	expect_length(area_boundaries(roundrectGrob()), 1L)
	# a vector `hjust` / `vjust` justifies each rectangle
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.2, height = 0.2, hjust = c(0, 1), vjust = 0)
	b <- area_boundaries(rect)
	expect_equal(b[[1L]]$hjust, 0)
	expect_equal(b[[2L]]$hjust, 1)
	expect_equal(b[[2L]]$vjust, 0)
	expect_null(area_boundaries(rectGrob(x = c(0.25, 0.75)))[[2L]]$hjust)
	local_temp_dev()
	expect_equal(range(boundary_coords(b[[2L]])$x), c(0.55, 0.75))
	merged <- getGrob(
		with_pattern(layers = "grob", rect, group = c(1, 1)),
		gPath("pattern", "area.1")
	)$boundary
	expect_equal(merged$hjust, c(0, 1))

	# paths by `pathId` keeping their rings
	path <- pathGrob(
		x = c(0, 1, 1, 0, 0.25, 0.75, 0.75, 0.25, 2, 3, 3),
		y = c(0, 0, 1, 1, 0.25, 0.25, 0.75, 0.75, 0, 0, 1),
		id.lengths = c(4L, 4L, 3L),
		pathId.lengths = c(8L, 3L),
		rule = "evenodd"
	)
	b <- area_boundaries(path)
	expect_length(b, 2L)
	expect_s3_class(b[[1L]], "pathgrob")
	expect_equal(b[[1L]]$id, rep(1:2, each = 4L))
	expect_null(b[[1L]]$id.lengths)
	expect_null(b[[1L]]$pathId)
	expect_null(b[[1L]]$pathId.lengths)
	expect_equal(b[[1L]]$rule, "evenodd")
	expect_equal(b[[2L]]$id, rep(3L, 3L))
	expect_length(area_boundaries(pathGrob(path$x[1:8], path$y[1:8], id.lengths = c(4L, 4L))), 1L)

	# a gTree's areas are its children's (in drawing order) within its viewport
	polygon <- polygonGrob(
		x,
		y,
		id.lengths = c(4L, 3L),
		name = "polygons",
		vp = viewport(width = 0.5)
	)
	inner_vp <- viewport(height = 0.5)
	tree <- gTree(
		children = gList(
			rectGrob(x = c(0.25, 0.75), width = 0.4, name = "rects"),
			textGrob("a", name = "text"),
			gTree(children = gList(polygon), vp = inner_vp, name = "inner")
		),
		vp = viewport(width = 0.8),
		name = "tree"
	)
	b <- area_boundaries(tree)
	expect_length(b, 4L)
	expect_equal(
		vapply(b, function(g) class(g)[1L], character(1L)),
		c("rect", "rect", "polygon", "polygon")
	)
	expect_identical(b[[1L]]$vp, tree$vp)
	expect_s3_class(b[[3L]]$vp, "vpStack")
	expect_identical(b[[3L]]$vp, vpStack(tree$vp, vpStack(inner_vp, polygon$vp)))

	# grobs without an area
	expect_length(area_boundaries(textGrob("a")), 0L)
	expect_length(area_boundaries(linesGrob()), 0L)
	expect_length(area_boundaries(pointsGrob()), 0L)
	expect_s3_class(area_boundaries(gTree()), "gList")
	expect_length(area_boundaries(gTree()), 0L)
	expect_error(area_boundaries("a"), "must be a grid grob")
})

test_that("the areas of a grob are in a fixed (documented) order", {
	# polygons by increasing (integer) `id`, not by first appearance
	x <- c(0.1, 0.4, 0.4, 0.1, 0.6, 0.9, 0.75)
	y <- c(0.1, 0.1, 0.4, 0.4, 0.6, 0.6, 0.9)
	polygon <- polygonGrob(x, y, id = c(3, 3, 3, 3, 1, 1, 1), name = "polygons")
	b <- area_boundaries(polygon)
	expect_length(b, 2L)
	expect_equal(b[[1L]]$x, unit(x[5:7], "npc"))
	expect_equal(b[[2L]]$x, unit(x[1:4], "npc"))
	# (each polygon takes its points wherever they are)
	b <- area_boundaries(polygonGrob(x[1:6], y[1:6], id = c(2, 1, 2, 1, 2, 1)))
	expect_equal(b[[1L]]$x, unit(x[c(2, 4, 6)], "npc"))
	expect_equal(b[[2L]]$x, unit(x[c(1, 3, 5)], "npc"))
	# `id.lengths` number the polygons in drawing order
	b <- area_boundaries(polygonGrob(x, y, id.lengths = c(4L, 3L)))
	expect_equal(b[[1L]]$x, unit(x[1:4], "npc"))
	expect_equal(b[[2L]]$x, unit(x[5:7], "npc"))
	# the per-area pattern parameters of `with_pattern()` follow the same order
	grob <- with_pattern(polygon, c("stripe", "circle"), fill = c("red", "blue"))
	p1 <- getGrob(grob, gPath("area.1", "pattern", "area.1"))
	expect_equal(p1$pattern, "stripe")
	expect_equal(p1$args, list(pattern_fill = "red"))
	expect_equal(p1$x, unit(x[5:7], "npc"))
	p2 <- getGrob(grob, gPath("area.2", "pattern", "area.2"))
	expect_equal(p2$pattern, "circle")
	expect_equal(p2$x, unit(x[1:4], "npc"))

	# paths by increasing `pathId` (each keeping its rings)
	px <- c(0, 1, 1, 0, 0.25, 0.75, 0.75, 0.25, 2, 3, 3)
	py <- c(0, 0, 1, 1, 0.25, 0.25, 0.75, 0.75, 0, 0, 1)
	path <- pathGrob(px, py, id = rep(1:3, c(4L, 4L, 3L)), pathId = rep(c(2L, 1L), c(8L, 3L)))
	b <- area_boundaries(path)
	expect_length(b, 2L)
	expect_equal(b[[1L]]$x, unit(px[9:11], "npc"))
	expect_equal(b[[1L]]$id, rep(3L, 3L))
	expect_equal(b[[2L]]$x, unit(px[1:8], "npc"))
	expect_equal(b[[2L]]$id, rep(1:2, each = 4L))
	# `pathId.lengths` number the paths in drawing order
	b <- area_boundaries(pathGrob(px, py, id.lengths = c(4L, 4L, 3L), pathId.lengths = c(8L, 3L)))
	expect_equal(b[[1L]]$x, unit(px[1:8], "npc"))
	expect_equal(b[[2L]]$x, unit(px[9:11], "npc"))

	# rects in element order and all of a child's areas before its next child's
	b <- area_boundaries(rectGrob(x = c(0.75, 0.25), width = 0.2, height = 0.2))
	expect_equal(b[[1L]]$x, unit(0.75, "npc"))
	tree <- gTree(
		children = gList(
			polygon,
			rectGrob(x = c(0.75, 0.25), width = 0.2, height = 0.2, name = "rects")
		)
	)
	b <- area_boundaries(tree)
	expect_equal(
		vapply(b, function(g) class(g)[1L], character(1L)),
		c("polygon", "polygon", "rect", "rect")
	)
	expect_equal(b[[1L]]$x, unit(x[5:7], "npc"))
	expect_equal(b[[3L]]$x, unit(0.75, "npc"))
})

test_that("`with_pattern()` replaces a grob with areas by fill / pattern / border layers", {
	rect <- rectGrob(
		x = c(0.25, 0.75),
		width = 0.4,
		height = c(0.8, 0.4),
		name = "rects",
		gp = gpar(fill = c("red", "blue"), col = "black", lwd = 3),
		vp = viewport(width = 0.9)
	)
	attr(rect, "width") <- unit(1, "cm")
	grob <- with_pattern(layers = "grob", rect, "stripe", fill = c("yellow", "green"), angle = 45)
	expect_s3_class(grob, "gTree")
	expect_equal(grob$name, "rects")
	expect_identical(grob$vp, rect$vp)
	expect_equal(attr(grob, "width"), unit(1, "cm"))
	expect_equal(childNames(grob), c("fill", "pattern", "border"))
	fill <- getGrob(grob, "fill")
	expect_s3_class(fill, "rect")
	expect_null(fill$vp)
	expect_equal(fill$gp$fill, c("red", "blue"))
	expect_true(is.na(fill$gp$col))
	expect_equal(fill$gp$lwd, 0)
	border <- getGrob(grob, "border")
	expect_s3_class(border, "rect")
	expect_null(border$vp)
	expect_true(is.na(border$gp$fill))
	expect_equal(border$gp$col, "black")
	expect_equal(border$gp$lwd, 3)
	patterns <- getGrob(grob, "pattern")
	expect_equal(childNames(patterns), c("area.1", "area.2"))
	expect_null(patterns$gp)
	p1 <- getGrob(patterns, "area.1")
	expect_equal(p1$content_gp$col, "black")
	p2 <- getGrob(patterns, "area.2")
	expect_s3_class(p1, "pattern")
	expect_equal(p1$pattern, "stripe")
	expect_equal(p1$args, list(pattern_fill = "yellow", pattern_angle = 45))
	expect_equal(p2$args, list(pattern_fill = "green", pattern_angle = 45))
	expect_false(p1$legend)
	expect_s3_class(p1$boundary, "rect")
	expect_null(p1$boundary$vp)
	expect_equal(p1$boundary$x, unit(0.25, "npc"))
	expect_equal(p2$boundary$height, unit(0.4, "npc"))
	local_temp_dev()
	expect_no_error(grid.draw(grob))

	# a polygon's pattern grobs reuse its coordinates
	polygon <- polygonGrob(
		c(0, 1, 1, 0, 0, 0.5, 1),
		c(0, 0, 0.4, 0.4, 0.6, 1, 0.6),
		id.lengths = c(4L, 3L)
	)
	grob <- with_pattern(layers = "grob", polygon, "circle", legend = 2)
	p1 <- getGrob(grob, gPath("pattern", "area.1"))
	expect_null(p1$boundary)
	expect_equal(p1$x, unit(c(0, 1, 1, 0), "npc"))
	expect_equal(p1$id, rep_len(1L, 4L))
	expect_equal(p1$legend, 2)
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$y, unit(c(0.6, 1, 0.6), "npc"))

	# grobs without an area are returned unchanged
	expect_identical(
		with_pattern(layers = "grob", textGrob("a", name = "t"), fill = "red"),
		textGrob("a", name = "t")
	)
	tree <- gTree(children = gList(linesGrob(name = "l")), name = "g")
	expect_identical(with_pattern(layers = "grob", tree, fill = c("red", "blue")), tree)
	expect_error(with_pattern(layers = "grob", "a"), "method for an object of class `character`")
})

test_that("`with_pattern()` walks a gTree replacing its grobs with areas in place", {
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, name = "rects")
	polygon <- polygonGrob(
		c(0, 1, 1, 0, 0, 0.5, 1),
		c(0, 0, 0.4, 0.4, 0.6, 1, 0.6),
		id.lengths = c(4L, 3L),
		name = "polygons"
	)
	tree <- gTree(
		children = gList(
			rect,
			textGrob("a", name = "text"),
			gTree(children = gList(polygon, circleGrob(name = "circle")), name = "inner")
		),
		name = "tree"
	)
	out <- with_pattern(
		layers = "grob",
		tree,
		"stripe",
		fill = c("red", "blue", "green", "yellow", "purple"),
		angle = 45
	)
	expect_s3_class(out, "gTree")
	expect_equal(out$name, "tree")
	expect_equal(childNames(out), childNames(tree))
	expect_identical(getGrob(out, "text"), getGrob(tree, "text"))
	rects <- getGrob(out, "rects")
	expect_s3_class(rects, "gTree")
	expect_equal(childNames(rects), c("fill", "pattern", "border"))
	inner <- getGrob(out, "inner")
	expect_equal(childNames(inner), c("polygons", "circle"))
	# the parameters are consumed in drawing order
	fill_of <- function(...) getGrob(out, gPath(...))$args$pattern_fill
	expect_equal(fill_of("rects", "pattern", "area.1"), "red")
	expect_equal(fill_of("rects", "pattern", "area.2"), "blue")
	expect_equal(fill_of("inner", "polygons", "pattern", "area.1"), "green")
	expect_equal(fill_of("inner", "polygons", "pattern", "area.2"), "yellow")
	expect_equal(fill_of("inner", "circle", "pattern", "area.1"), "purple")
	expect_equal(getGrob(out, gPath("inner", "circle", "pattern", "area.1"))$args$pattern_angle, 45)
})

test_that("`with_pattern()` validates its pattern parameters", {
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, name = "rects")
	expect_error(with_pattern(layers = "grob", rect, "stripe", "red"), "must be named")
	expect_error(with_pattern(layers = "grob", rect, "stripe", name = "x"), "`name` can't be given")
	expect_error(with_pattern(layers = "grob", rect, "stripe", y = 1), "`y` can't be given")
	expect_error(with_pattern(layers = "grob", rect, "stripe", gp = gpar()), "`gp` can't be given")
	expect_error(
		with_pattern(layers = "grob", rect, "stripe", fill = c("red", "blue", "green")),
		"`fill` must have length one or one value per area (2) but has length 3",
		fixed = TRUE
	)
	# a multi-valued parameter of a single area is wrapped in a list
	grob <- with_pattern(
		layers = "grob",
		rect,
		"stripe",
		fill = list(c("red", "blue")),
		density = 0.3
	)
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$args$pattern_fill, c("red", "blue"))
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$args$pattern_density, 0.3)
	grob <- with_pattern(layers = "grob", rect, "stripe", fill = list(c("red", "blue"), "green"))
	expect_equal(getGrob(grob, gPath("pattern", "area.1"))$args$pattern_fill, c("red", "blue"))
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$args$pattern_fill, "green")
	# a `NULL` parameter is unset for every area
	expect_no_error(with_pattern(layers = "grob", rect, "stripe", fill = NULL))
	# a `grid::pattern()` fill is one value
	skip_if(getRversion() < "4.1.0")
	gradient <- linearGradient(c("red", "blue"))
	grob <- with_pattern(layers = "grob", rect, "stripe", fill = gradient)
	expect_identical(getGrob(grob, gPath("pattern", "area.2"))$args$pattern_fill, gradient)
	grob <- with_pattern(layers = "grob", rect, "stripe", fill = list(gradient, "green"))
	expect_identical(getGrob(grob, gPath("pattern", "area.1"))$args$pattern_fill, gradient)
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$args$pattern_fill, "green")
})

test_that("`with_pattern()` takes one pattern per area, a `prefix`, and gLists", {
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, name = "rects")
	grob <- with_pattern(layers = "grob", rect, c("stripe", "circle"), fill = "red")
	expect_equal(getGrob(grob, gPath("pattern", "area.1"))$pattern, "stripe")
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$pattern, "circle")
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$args, list(pattern_fill = "red"))
	expect_error(
		with_pattern(layers = "grob", rect, c("stripe", "circle", "wave")),
		"`pattern` must have length one or one value per area (2) but has length 3",
		fixed = TRUE
	)
	expect_error(with_pattern(layers = "grob", rect, 1), "must be a character vector")

	# parameters already named `pattern_*`
	grob <- with_pattern(
		layers = "grob",
		rect,
		"stripe",
		pattern_fill = c("red", "blue"),
		prefix = ""
	)
	expect_equal(getGrob(grob, gPath("pattern", "area.2"))$args, list(pattern_fill = "blue"))

	# a gList's areas are those of its grobs in order
	glist <- gList(
		rect,
		linesGrob(name = "l"),
		polygonGrob(c(0, 1, 0.5), c(0, 0, 1), name = "p")
	)
	b <- area_boundaries(glist)
	expect_s3_class(b, "gList")
	expect_length(b, 3L)
	expect_s3_class(b[[3L]], "polygon")
	expect_length(area_boundaries(gList()), 0L)
	out <- with_pattern(layers = "grob", glist, c("stripe", "circle", "crosshatch"), fill = "red")
	expect_s3_class(out, "gList")
	expect_length(out, 3L)
	expect_identical(out[[2L]], glist[[2L]])
	expect_equal(out[[1L]]$name, "rects")
	expect_equal(getGrob(out[[3L]], gPath("pattern", "area.1"))$pattern, "crosshatch")
	lines <- gList(linesGrob())
	expect_identical(with_pattern(layers = "grob", lines, fill = "red"), lines)
	local_temp_dev()
	expect_no_error(grid.draw(gTree(children = out)))
})

test_that("`area_boundaries()` has one area per filled symbol of `points`", {
	points <- pointsGrob(
		x = c(0.1, 0.3, 0.5, 0.7, 0.9),
		y = rep(0.5, 5L),
		pch = c(21, 19, 22, 1, 25),
		size = unit(c(1, 2), "char")
	)
	b <- area_boundaries(points)
	expect_length(b, 3L)
	expect_s3_class(b[[2L]], "points")
	expect_equal(b[[1L]]$x, unit(0.1, "native"))
	expect_equal(b[[2L]]$x, unit(0.5, "native"))
	expect_equal(b[[2L]]$pch, 22)
	expect_equal(b[[2L]]$size, unit(1, "char"))
	expect_equal(b[[3L]]$pch, 25)
	expect_length(area_boundaries(pointsGrob(0.5, 0.5, pch = 19)), 0L)
	expect_length(area_boundaries(pointsGrob(0.5, 0.5, pch = "a")), 0L)
})

test_that("`leaves` selects which children of a gTree or gList have areas", {
	# e.g. a {ggplot2} boxplot's (filled) outliers
	points <- pointsGrob(c(0.2, 0.8), c(0.5, 0.5), pch = 21, name = "outliers")
	rect <- rectGrob(width = 0.5, name = "box")
	tree <- gTree(children = gList(rect, points), name = "boxplot")
	expect_length(area_boundaries(tree), 1L)
	expect_length(area_boundaries(gList(rect, points)), 1L)
	expect_length(area_boundaries(tree, leaves = c("rect", "points")), 3L)
	expect_length(area_boundaries(tree, leaves = "points"), 2L)
	expect_length(area_boundaries(gList(rect, points), leaves = "points"), 2L)
	# nested gTrees are always walked
	nested <- gTree(children = gList(tree))
	expect_length(area_boundaries(nested, leaves = "points"), 2L)

	out <- with_pattern(layers = "grob", tree, fill = "red")
	expect_identical(getGrob(out, "outliers"), points)
	expect_equal(childNames(getGrob(out, "box")), c("fill", "pattern", "border"))
	out <- with_pattern(
		layers = "grob",
		tree,
		c("stripe", "circle", "crosshatch"),
		leaves = c("rect", "points")
	)
	expect_equal(getGrob(out, gPath("outliers", "pattern", "area.2"))$pattern, "crosshatch")
	out <- with_pattern(layers = "grob", gList(rect, points), leaves = "points")
	expect_identical(out[[1L]], rect)
	expect_equal(childNames(getGrob(out[[2L]], "pattern")), c("area.1", "area.2"))

	# a points grob given as `x` itself is patterned
	out <- with_pattern(layers = "grob", points, "stripe", fill = c("red", "blue"))
	expect_equal(childNames(out), c("fill", "pattern", "border"))
	expect_equal(getGrob(out, gPath("pattern", "area.2"))$args$pattern_fill, "blue")
	expect_error(with_pattern(layers = "grob", rect, leaves = "rect"), "`leaves` can only be given")
	skip_if(getRversion() < "4.2.0")
	local_temp_dev()
	expect_no_error(grid.draw(out))
})

test_that("`with_pattern()` fills a `group` of areas with one pattern", {
	# e.g. a {sf} MULTIPOLYGON column: one path per polygon
	path <- pathGrob(
		x = c(0, 0.2, 0.2, 0, 0.05, 0.15, 0.15, 0.05, 0.4, 0.6, 0.6, 0.8, 1, 0.9),
		y = c(0, 0, 0.2, 0.2, 0.05, 0.05, 0.15, 0.15, 0, 0, 0.2, 0, 0, 0.2),
		id = c(rep(1L, 4L), rep(2L, 4L), rep(3L, 3L), rep(1L, 3L)),
		pathId.lengths = c(8L, 3L, 3L),
		rule = "evenodd",
		name = "counties"
	)
	expect_length(area_boundaries(path), 3L)
	out <- with_pattern(
		layers = "grob",
		path,
		"gradient",
		fill = c("red", "blue"),
		group = c("a", "a", "b")
	)
	expect_equal(childNames(getGrob(out, "pattern")), c("area.1", "area.2"))
	p1 <- getGrob(out, gPath("pattern", "area.1"))
	expect_equal(p1$args$pattern_fill, "red")
	b <- p1$boundary
	expect_s3_class(b, "pathgrob")
	expect_equal(b$x, path$x[1:11])
	# each area stays a path (keeping its holes) with distinct rings
	expect_equal(b$pathId, rep(1:2, c(8L, 3L)))
	expect_equal(b$id, rep(1:3, c(4L, 4L, 3L)))
	expect_equal(b$rule, "evenodd")
	p2 <- getGrob(out, gPath("pattern", "area.2"))
	expect_equal(p2$args$pattern_fill, "blue")
	expect_equal(p2$boundary$x, path$x[12:14])
	local_temp_dev()
	expect_no_error(grid.draw(out))

	# polygons, rects, circles, and points
	polygon <- polygonGrob(
		c(0, 0.2, 0.1, 0.5, 0.7, 0.6, 0.8, 1, 0.9),
		c(0, 0, 0.2, 0, 0, 0.2, 0, 0, 0.2),
		id.lengths = c(3L, 3L, 3L)
	)
	out <- with_pattern(layers = "grob", polygon, group = c(1, 2, 1))
	b <- getGrob(out, gPath("pattern", "area.1"))
	expect_equal(b$x, unit(c(0, 0.2, 0.1, 0.8, 1, 0.9), "npc"))
	expect_equal(b$id, rep(1:2, each = 3L))
	expect_equal(getGrob(out, gPath("pattern", "area.2"))$x, unit(c(0.5, 0.7, 0.6), "npc"))
	rect <- rectGrob(x = c(0.2, 0.5, 0.8), width = 0.2, height = c(0.1, 0.2, 0.3))
	b <- getGrob(
		with_pattern(layers = "grob", rect, group = c(1, 1, 2)),
		gPath("pattern", "area.1")
	)$boundary
	expect_equal(b$x, unit(c(0.2, 0.5), "npc"))
	expect_equal(b$height, unit(c(0.1, 0.2), "npc"))
	circle <- circleGrob(x = c(0.2, 0.5, 0.8), r = c(0.1, 0.2, 0.1))
	b <- getGrob(
		with_pattern(layers = "grob", circle, group = c(1, 1, 1)),
		gPath("pattern", "area.1")
	)$boundary
	expect_equal(b$r, unit(c(0.1, 0.2, 0.1), "npc"))
	points <- pointsGrob(c(0.2, 0.5, 0.8), rep(0.5, 3L), pch = c(21, 19, 22))
	b <- getGrob(
		with_pattern(layers = "grob", points, group = c(1, 1)),
		gPath("pattern", "area.1")
	)$boundary
	expect_equal(b$x, unit(c(0.2, 0.8), "native"))
	expect_equal(b$pch, c(21, 22))

	# within a gTree the groups follow the areas in drawing order
	tree <- gTree(
		children = gList(rectGrob(x = c(0.25, 0.75), width = 0.4, name = "rects"), polygon),
		name = "tree"
	)
	out <- with_pattern(
		layers = "grob",
		tree,
		fill = c("red", "blue", "green"),
		group = c(1, 1, 2, 3, 2)
	)
	expect_equal(childNames(getGrob(out, gPath("rects", "pattern"))), "area.1")
	fill_of <- function(...) getGrob(out, gPath(...))$args$pattern_fill
	expect_equal(fill_of("rects", "pattern", "area.1"), "red")
	expect_equal(fill_of(polygon$name, "pattern", "area.1"), "blue")
	expect_equal(fill_of(polygon$name, "pattern", "area.2"), "green")

	expect_error(
		with_pattern(layers = "grob", tree, fill = c("red", "blue"), group = c(1, 1, 2, 3, 2)),
		"one value per group (3) but has length 2",
		fixed = TRUE
	)
	expect_error(
		with_pattern(layers = "grob", tree, group = 1:3),
		"`group` must have one (non-missing) value per area (5)",
		fixed = TRUE
	)
	expect_error(with_pattern(layers = "grob", tree, group = c(1, 1, 2, 2, NA)), "non-missing")
	expect_error(
		with_pattern(layers = "grob", tree, group = c(1, 2, 2, 3, 3)),
		"must all be areas of the same grob"
	)
	expect_error(
		with_pattern(
			layers = "grob",
			gTree(children = gList(roundrectGrob(), roundrectGrob())),
			group = c(1, 1)
		),
		"must all be areas of the same grob"
	)
})

test_that("a `fillStrokeGrob()` or `fillGrob()` is one area", {
	skip_if(getRversion() < "4.2.0")
	path <- gTree(children = gList(circleGrob(0.3, r = 0.3), circleGrob(0.3, r = 0.15)))
	fs <- fillStrokeGrob(
		path,
		rule = "evenodd",
		gp = gpar(fill = "yellow", col = "blue"),
		name = "fs"
	)
	b <- area_boundaries(fs)
	expect_length(b, 1L)
	expect_identical(b[[1L]], fs)
	expect_length(area_boundaries(fillGrob(path)), 1L)
	expect_length(area_boundaries(strokeGrob(path)), 0L)
	tree <- gTree(children = gList(fs, fillGrob(path, name = "f"), strokeGrob(path, name = "s")))
	expect_length(area_boundaries(tree), 2L)

	out <- with_pattern(layers = "grob", fs, "stripe", fill = "red")
	expect_equal(out$name, "fs")
	expect_equal(childNames(out), c("fill", "pattern", "border"))
	# (its `col` keeps e.g. the glyphs of text in its path in R before the fix of bug 19094)
	fill <- getGrob(out, "fill")
	expect_s3_class(fill, "GridFill")
	expect_identical(fill$path, fs$path)
	expect_equal(fill$rule, "evenodd")
	expect_equal(fill$gp, fs$gp)
	expect_identical(
		getGrob(with_pattern(layers = "grob", fillGrob(path), "stripe"), "fill")$path,
		path
	)
	border <- getGrob(out, "border")
	expect_true(is.na(border$gp$fill))
	expect_equal(border$gp$col, "blue")
	expect_s3_class(getGrob(out, gPath("pattern", "area.1"))$boundary, "GridFillStroke")
	expect_error(
		with_pattern(layers = "grob", tree, group = c(1, 1)),
		"must all be areas of the same grob"
	)

	# its mask paints only its fill (a stroke would encroach on its holes)
	masker <- masker_grob(fs)
	expect_s3_class(masker, "GridFill")
	expect_equal(masker$rule, "evenodd")
	expect_equal(masker$gp$fill, "black")
	expect_equal(masker$gp$col, "black")
	expect_equal(masker_grob(editGrob(fs, gp = gpar(col = NA)))$gp$col, "black")

	# its region includes the (implicitly closed) open shapes of its path
	local_temp_dev()
	lines <- fillStrokeGrob(linesGrob(c(0.1, 0.9, 0.5), c(0.1, 0.1, 0.9)))
	coords <- boundary_coords(lines)
	expect_equal(range(coords$x), c(0.1, 0.9))
	expect_equal(range(coords$y), c(0.1, 0.9))
	expect_false(is.null(coords$masker))
	expect_true(needs_mask(fs))
})

test_that("`area_boundaries()` has one area per closed spline of an `xspline`", {
	x <- c(0.1, 0.3, 0.2, 0.6, 0.9, 0.7)
	y <- c(0.1, 0.1, 0.4, 0.1, 0.1, 0.4)
	xs <- xsplineGrob(x, y, id = rep(1:2, each = 3L), open = FALSE, shape = c(1, 0, -1, 1, 1, 0.5))
	b <- area_boundaries(xs)
	expect_length(b, 2L)
	expect_s3_class(b[[2L]], "xspline")
	expect_equal(b[[2L]]$x, unit(x[4:6], "npc"))
	expect_equal(b[[2L]]$shape, c(1, 1, 0.5))
	expect_null(b[[2L]]$id)
	expect_false(b[[2L]]$open)
	expect_equal(
		area_boundaries(xsplineGrob(x[1:3], y[1:3], open = FALSE, shape = 1))[[1L]]$shape,
		rep(1, 3L)
	)
	expect_length(area_boundaries(editGrob(xs, open = TRUE)), 0L)
	# e.g. a `{ggplot2}` curve is an open spline
	tree <- gTree(children = gList(xs, editGrob(xs, open = TRUE, name = "curve")))
	expect_length(area_boundaries(tree), 2L)

	out <- with_pattern(layers = "grob", xs, "stripe", fill = c("red", "blue"))
	expect_equal(childNames(getGrob(out, "pattern")), c("area.1", "area.2"))
	merged <- getGrob(
		with_pattern(layers = "grob", xs, group = c(1, 1)),
		gPath("pattern", "area.1")
	)$boundary
	expect_equal(merged$x, xs$x)
	expect_equal(merged$id, rep(1:2, each = 3L))
	expect_equal(merged$shape, xs$shape)
	skip_if(getRversion() < "4.2.0")
	local_temp_dev()
	expect_no_error(grid.draw(out))
	coords <- boundary_coords(b[[2L]])
	expect_true(min(coords$x) >= 0.6 - 1e-6)
})

test_that("`layers = \"area\"` draws the layers of each area in turn", {
	rect <- rectGrob(
		x = c(0.3, 0.5, 0.7),
		width = 0.3,
		name = "rects",
		gp = gpar(fill = c("red", "blue", "green"), col = "black", lwd = c(1, 2, 3)),
		vp = viewport(width = 0.9)
	)
	attr(rect, "width") <- unit(1, "cm")
	out <- with_pattern(rect, "stripe", fill = c("yellow", "orange", "pink"))
	expect_equal(out$name, "rects")
	expect_identical(out$vp, rect$vp)
	expect_equal(attr(out, "width"), unit(1, "cm"))
	expect_equal(childNames(out), c("area.1", "area.2", "area.3"))
	area2 <- getGrob(out, "area.2")
	expect_equal(childNames(area2), c("fill", "pattern", "border"))
	fill <- getGrob(area2, "fill")
	expect_s3_class(fill, "rect")
	expect_null(fill$vp)
	expect_equal(fill$x, unit(0.5, "npc"))
	expect_equal(fill$gp$fill, "blue")
	expect_true(is.na(fill$gp$col))
	border <- getGrob(area2, "border")
	expect_equal(border$x, unit(0.5, "npc"))
	expect_true(is.na(border$gp$fill))
	expect_equal(border$gp$lwd, 2)
	expect_equal(getGrob(area2, gPath("pattern", "area.2"))$content_gp$col, "black")
	p2 <- getGrob(out, gPath("area.2", "pattern", "area.2"))
	expect_equal(p2$args$pattern_fill, "orange")
	expect_equal(p2$boundary$x, unit(0.5, "npc"))
	local_temp_dev()
	expect_no_error(grid.draw(out))

	# the default `"grob"` layers are kept with one shape
	expect_equal(childNames(with_pattern(rectGrob())), c("fill", "pattern", "border"))

	# shapes without areas keep their place
	points <- pointsGrob(
		c(0.2, 0.4, 0.6),
		rep(0.5, 3L),
		pch = c(21, 19, 22),
		gp = gpar(fill = c("red", "blue", "green"), cex = c(1, 2, 3))
	)
	out <- with_pattern(points, "stripe")
	expect_equal(childNames(out), c("area.1", "shape.2", "area.2"))
	shape <- getGrob(out, "shape.2")
	expect_equal(shape$pch, 19)
	expect_equal(shape$gp$cex, 2)
	expect_equal(getGrob(out, gPath("area.2", "fill"))$gp$fill, "green")
	expect_equal(getGrob(out, gPath("area.2", "fill"))$gp$cex, 3)

	# polygons by (sorted) `id` and paths by `pathId`
	polygon <- polygonGrob(
		c(0, 1, 0.5, 0, 1, 0.5),
		c(0, 0, 1, 0.2, 0.2, 0.8),
		id = rep(2:1, each = 3L),
		gp = gpar(fill = c("red", "blue"))
	)
	out <- with_pattern(polygon)
	expect_equal(getGrob(out, gPath("area.1", "fill"))$y, unit(c(0.2, 0.2, 0.8), "npc"))
	expect_equal(getGrob(out, gPath("area.1", "fill"))$gp$fill, "red")
	path <- pathGrob(
		x = c(0, 1, 1, 0, 0.25, 0.75, 0.75, 0.25, 2, 3, 3),
		y = c(0, 0, 1, 1, 0.25, 0.25, 0.75, 0.75, 0, 0, 1),
		id.lengths = c(4L, 4L, 3L),
		pathId.lengths = c(8L, 3L),
		rule = "evenodd",
		gp = gpar(fill = c("red", "blue"))
	)
	out <- with_pattern(path)
	fill <- getGrob(out, gPath("area.1", "fill"))
	expect_equal(fill$id, rep(1:2, each = 4L))
	expect_null(fill$pathId)
	expect_equal(getGrob(out, gPath("area.2", "fill"))$gp$fill, "blue")

	# a group's areas are drawn together where its first area is
	out <- with_pattern(rect, group = c(1, 2, 1))
	expect_equal(childNames(out), c("area.1", "area.2"))
	fills <- getGrob(out, gPath("area.1", "fill"))
	expect_s3_class(fills, "gTree")
	expect_equal(
		unname(vapply(fills$children, function(g) g$gp$fill, character(1L))),
		c("red", "green")
	)
	expect_equal(childNames(fills), c("fill.1", "fill.2"))
	expect_equal(getGrob(fills, "fill.2")$x, unit(0.7, "npc"))
	expect_equal(childNames(getGrob(out, gPath("area.1", "border"))), c("border.1", "border.2"))

	# a fill relative to all the shapes (a gradient spanning them) keeps the `"grob"` layers
	skip_if(getRversion() < "4.2.0")
	gradient <- linearGradient(c("red", "blue"))
	expect_equal(
		childNames(with_pattern(editGrob(rect, gp = gpar(fill = gradient)))),
		c("fill", "pattern", "border")
	)
	expect_equal(
		childNames(with_pattern(editGrob(rect, gp = gpar(fill = list(gradient, gradient))))),
		c("fill", "pattern", "border")
	)
	separate <- linearGradient(c("red", "blue"), group = FALSE)
	out <- with_pattern(editGrob(rect, gp = gpar(fill = separate)))
	expect_equal(childNames(out), c("area.1", "area.2", "area.3"))
	expect_identical(getGrob(out, gPath("area.3", "fill"))$gp$fill, separate)
})

test_that("each boundary keeps its own graphical parameters", {
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, gp = gpar(fill = c("red", "blue"), lwd = 3))
	b <- area_boundaries(rect)
	expect_equal(b[[1L]]$gp$fill, "red")
	expect_equal(b[[2L]]$gp$fill, "blue")
	expect_equal(b[[2L]]$gp$lwd, 3)
	polygon <- polygonGrob(
		c(0, 1, 0.5, 0, 1, 0.5),
		c(0, 0, 1, 0.2, 0.2, 0.8),
		id = rep(2:1, each = 3L),
		gp = gpar(fill = c("red", "blue"))
	)
	b <- area_boundaries(polygon)
	# (grid fills its polygons in sorted `id` order)
	expect_equal(b[[1L]]$y, unit(c(0.2, 0.2, 0.8), "npc"))
	expect_equal(b[[1L]]$gp$fill, "red")
	points <- pointsGrob(
		c(0.2, 0.5, 0.8),
		rep(0.5, 3L),
		pch = c(21, 19, 22),
		gp = gpar(fill = c("red", "green", "blue"))
	)
	expect_equal(
		vapply(area_boundaries(points), function(g) g$gp$fill, character(1L)),
		c("red", "blue")
	)
	skip_if(getRversion() < "4.2.0")
	gradients <- list(linearGradient(c("red", "blue")), radialGradient(c("red", "blue")))
	b <- area_boundaries(editGrob(rect, gp = gpar(fill = gradients)))
	expect_s3_class(b[[2L]]$gp$fill, "GridRadialGradient")

	# a group's merged boundary has each of its shapes' parameters
	merged <- getGrob(
		with_pattern(polygon, group = c(1, 1), layers = "grob"),
		gPath("pattern", "area.1")
	)
	expect_equal(merged$id, rep(1:2, each = 3L))
	merged <- getGrob(
		with_pattern(rect, group = c(1, 1), layers = "grob"),
		gPath("pattern", "area.1")
	)
	expect_equal(merged$boundary$gp$fill, c("red", "blue"))
})

test_that("a grob class with its own `area_boundaries()` method is supported", {
	# (e.g. registered by another package for its own class)
	registerS3method(
		"area_boundaries",
		"myrects",
		function(grob, ...) gList(rectGrob(0.25, width = 0.4), rectGrob(0.75, width = 0.4)),
		envir = asNamespace("gridpattern")
	)
	mine <- grob(name = "mine", cl = "myrects")
	expect_length(area_boundaries(gTree(children = gList(mine)), leaves = "myrects"), 2L)
	out <- with_pattern(mine, "stripe", fill = c("red", "blue"))
	# (it can't be split into its shapes so its layers are drawn for all its areas)
	expect_equal(childNames(out), c("fill", "pattern", "border"))
	expect_equal(getGrob(out, gPath("pattern", "area.2"))$args$pattern_fill, "blue")
})

test_that("filling a grob measured by (R 4.2's) `grobCoords()` errors clearly in R 4.1", {
	local_mocked_bindings(has_R4.2_grobCoords = function() FALSE)
	points <- pointsGrob(c(0.2, 0.8), c(0.5, 0.5), pch = 21)
	# (when the pattern grob is made, not when it is drawn)
	expect_error(
		with_pattern(points, "stripe"),
		"Filling a `points` grob with a pattern requires R (>= 4.2.0)",
		fixed = TRUE
	)
	expect_error(as.patternGrob(points, "stripe"), "requires R (>= 4.2.0)", fixed = TRUE)
	expect_error(with_pattern(circleGrob()), "Filling a `circle` grob")
	tree <- gTree(children = gList(rectGrob(), circleGrob()))
	expect_error(with_pattern(tree), "Filling a `circle` grob")
	expect_error(
		as.patternGrob(circleGrob()),
		"can only fill `polygon`, `pathgrob`, and `rect` grobs"
	)
	# polygons, paths, and rects are measured without `grobCoords()`
	expect_no_error(with_pattern(gTree(children = gList(rectGrob(), polygonGrob())), "stripe"))
	# a (filled) points child isn't patterned by default
	expect_no_error(with_pattern(gTree(children = gList(rectGrob(), points)), "stripe"))
})

test_that("a pattern's content (but not its boundary) has grid's default graphical parameters", {
	rect <- rectGrob(width = unit(2, "lines"), name = "r")
	tree <- gTree(children = gList(rect), gp = gpar(fill = "red", col = "blue", fontsize = 40))
	out <- with_pattern(tree)
	expect_null(getGrob(out, gPath("r", "pattern"))$gp)
	pattern <- getGrob(out, gPath("r", "pattern", "area.1"))
	expect_equal(pattern$content_gp$fill, "transparent")
	expect_equal(pattern$content_gp$col, "black")
	expect_null(pattern$content_gp$alpha)
	# its boundary is measured with the inherited `fontsize` (its content with grid's default)
	local_temp_dev()
	forced <- grid.force(out)
	content <- getGrob(forced, gPath("r", "pattern", "area.1", "pattern_content"))
	expect_equal(content$gp$fontsize, 12)
	pushViewport(viewport(gp = gpar(fontsize = 40)))
	expect_equal(
		range(boundary_coords(rect)$x),
		0.5 + c(-1, 1) * convertWidth(unit(1, "lines"), "npc", valueOnly = TRUE)
	)
	popViewport()

	# an area's own `alpha` (as its fill and border do)
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, gp = gpar(alpha = c(0.3, 0.6)))
	out <- with_pattern(rect)
	expect_equal(getGrob(out, gPath("area.2", "pattern", "area.2"))$content_gp$alpha, 0.6)
	out <- with_pattern(rect, layers = "grob")
	expect_equal(getGrob(out, gPath("pattern", "area.1"))$content_gp$alpha, 0.3)
	expect_equal(getGrob(out, gPath("pattern", "area.2"))$content_gp$alpha, 0.6)
})

test_that("`area_boundaries()` combines a boundary's graphical parameters with its gTrees'", {
	tree <- gTree(
		children = gList(gTree(
			children = gList(rectGrob(gp = gpar(cex = 1.5, alpha = 0.5, col = "blue"))),
			gp = gpar(fontsize = 30, cex = 2)
		)),
		gp = gpar(col = "red", fill = "green", alpha = 0.5)
	)
	gp <- area_boundaries(tree)[[1L]]$gp
	expect_equal(gp$fontsize, 30)
	expect_equal(gp$cex, 3)
	expect_equal(gp$alpha, 0.25)
	expect_equal(gp$col, "blue")
	expect_equal(gp$fill, "green")
	expect_identical(area_boundaries(gTree(children = gList(rectGrob())))[[1L]]$gp, gpar())
})

test_that("`with_pattern()` reports its errors as its own", {
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4)
	cnd <- tryCatch(with_pattern(rect, c("stripe", "circle", "none")), error = identity)
	expect_match(conditionMessage(cnd), "one value per area (2)", fixed = TRUE)
	expect_identical(conditionCall(cnd)[[1L]], quote(with_pattern))
	cnd <- tryCatch(with_pattern(gList(rect), fill = 1:3), error = identity)
	expect_identical(conditionCall(cnd)[[1L]], quote(with_pattern))
	tree <- gTree(children = gList(rect, rectGrob()))
	cnd <- tryCatch(with_pattern(tree, group = c(1, 2, 1)), error = identity)
	expect_match(conditionMessage(cnd), "same grob")
	expect_identical(conditionCall(cnd)[[1L]], quote(with_pattern))
	cnd <- tryCatch(
		with_pattern(gTree(children = gList(roundrectGrob(), roundrectGrob())), group = c(1, 1)),
		error = identity
	)
	expect_identical(conditionCall(cnd)[[1L]], quote(with_pattern))
	cnd <- tryCatch(with_pattern(rect, "stripe", "red"), error = identity)
	expect_identical(conditionCall(cnd)[[1L]], quote(with_pattern))
})

test_that("a gTree subclass with its own `area_boundaries()` method is patterned as a whole", {
	# (e.g. registered by another package for its own class)
	registerS3method(
		"area_boundaries",
		"mytree",
		function(grob, ..., leaves) {
			gList(rectGrob(0.5, width = 0.8, height = 0.8))
		},
		envir = asNamespace("gridpattern")
	)
	mine <- gTree(
		children = gList(rectGrob(0.25, width = 0.4), rectGrob(0.75, width = 0.4)),
		name = "mine",
		cl = "mytree"
	)
	expect_length(area_boundaries(mine), 1L)
	tree <- gTree(children = gList(rectGrob(name = "r"), mine), name = "tree")
	expect_length(area_boundaries(tree), 2L)
	out <- with_pattern(tree, "stripe", fill = c("red", "blue"))
	expect_equal(childNames(getGrob(out, "mine")), c("fill", "pattern", "border"))
	expect_equal(getGrob(out, gPath("mine", "pattern", "area.1"))$args$pattern_fill, "blue")
	out <- with_pattern(mine, "stripe", fill = "red")
	expect_equal(childNames(out), c("fill", "pattern", "border"))
	local_temp_dev()
	expect_no_error(grid.draw(out))
})

test_that("the default `leaves` agree", {
	leaves <- lapply(
		list(with_pattern.gTree, with_pattern.gList, area_boundaries.gTree, area_boundaries.gList),
		function(f) eval(formals(f)$leaves)
	)
	for (l in leaves[-1L]) {
		expect_identical(l, leaves[[1L]])
	}
})

test_that("`with_pattern()` visual tests", {
	skip_if_not_installed("vdiffr")
	skip_on_ci()
	tree <- gTree(
		children = gList(
			rectGrob(
				x = c(0.25, 0.75),
				y = 0.3,
				width = 0.35,
				height = 0.4,
				name = "rects",
				gp = gpar(fill = c("lightblue", "lightyellow"), col = "black", lwd = 3)
			),
			polygonGrob(
				c(0.3, 0.7, 0.5),
				c(0.6, 0.6, 0.95),
				name = "triangle",
				gp = gpar(fill = "pink", col = "red", lwd = 3)
			),
			textGrob("text is left alone", y = 0.05, name = "label"),
			segmentsGrob(0.05, 0.97, 0.95, 0.97, name = "line")
		),
		name = "tree"
	)
	vdiffr::expect_doppelganger("tree", function() {
		grid.draw(with_pattern(
			tree,
			"stripe",
			fill = c("blue", "orange", "red"),
			angle = c(45, -45, 0),
			density = 0.4
		))
	})
	# a legend key puts its `gp` on the parent gTree: the pattern layer doesn't inherit it
	key <- gTree(
		children = gList(rectGrob(name = "key")),
		gp = gpar(fill = "red", col = "blue", lwd = 6, lty = "dashed"),
		vp = viewport(width = 0.5, height = 0.5)
	)
	vdiffr::expect_doppelganger("key", function() {
		grid.draw(with_pattern(key, "stripe", fill = "yellow", legend = 2))
	})
	angle <- seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6)
	path <- pathGrob(
		x = c(0.5 + 0.5 * cos(angle), 0.5 + 0.25 * cos(angle)),
		y = c(0.5 + 0.5 * sin(angle), 0.5 + 0.25 * sin(angle)),
		id = rep(1:2, each = 6),
		rule = "evenodd",
		gp = gpar(fill = "grey90", col = "black", lwd = 4)
	)
	vdiffr::expect_doppelganger("path_hole", function() {
		grid.draw(with_pattern(path, "crosshatch", fill = "darkgreen", density = 0.3))
	})
	skip_if(getRversion() < "4.2.0")
	circles <- circleGrob(
		x = c(0.3, 0.7),
		r = 0.2,
		gp = gpar(fill = c("pink", "lightgreen"), col = "black", lwd = 3)
	)
	vdiffr::expect_doppelganger("circles", function() {
		grid.draw(with_pattern(
			circles,
			"weave",
			fill = "yellow",
			fill2 = c("blue", "red")
		))
	})
	# overlapping shapes are drawn in their original order
	overlapping <- circleGrob(
		x = c(0.35, 0.5, 0.65),
		r = 0.25,
		gp = gpar(
			fill = c("pink", "lightgreen", "lightblue"),
			col = c("red", "darkgreen", "blue"),
			lwd = 6
		)
	)
	vdiffr::expect_doppelganger("overlapping_area_layers", function() {
		grid.draw(with_pattern(
			overlapping,
			"stripe",
			fill = c("red", "darkgreen", "blue"),
			density = 0.3
		))
	})
	vdiffr::expect_doppelganger("overlapping_grob_layers", function() {
		grid.draw(with_pattern(
			overlapping,
			"stripe",
			fill = c("red", "darkgreen", "blue"),
			density = 0.3,
			layers = "grob"
		))
	})
})

# a grob which only builds its shapes when it is drawn:
# a pattern grob whose "fill" pattern draws its two boundary polygons
lazy_rects <- function(name = "lazy") {
	patternGrob(
		"fill",
		x = c(0.05, 0.45, 0.45, 0.05, 0.55, 0.95, 0.95, 0.55),
		y = rep(c(0.1, 0.1, 0.9, 0.9), 2L),
		id = rep(1:2, each = 4L),
		fill = c("red", "blue"),
		name = name
	)
}

test_that("`delay = TRUE` fills the areas a grob has when it is drawn", {
	lazy <- lazy_rects()
	expect_length(area_boundaries(lazy), 0L)
	expect_identical(with_pattern(lazy, "stripe"), lazy)
	delayed <- with_pattern(lazy, "stripe", fill = c("red", "blue"), delay = TRUE)
	expect_s3_class(delayed, "delayed_pattern")
	expect_identical(delayed$name, "lazy")
	expect_length(area_boundaries(delayed), 0L)
	# (keeping its attributes e.g. the `width` of a {ggplot2} legend key)
	key <- lazy_rects()
	attr(key, "width") <- 1.2
	expect_identical(attr(with_pattern(key, "stripe", delay = TRUE), "width"), 1.2)
	local_temp_dev()
	pushViewport(viewport())
	out <- makeContent(delayed)
	expect_null(getGrob(out, "lazy")$.ORIGINAL)
	# (the forced pattern grob's (auto-named) polygon child is replaced by its layers)
	layered <- getGrob(out, "lazy")$children[[1L]]
	expect_equal(childNames(layered), c("area.1", "area.2"))
	expect_equal(
		getGrob(layered, gPath("area.2", "pattern", "area.2"))$args$pattern_fill,
		"blue"
	)
	expect_no_error(grid.draw(delayed))
	# a gList can be delayed too (then also a gTree)
	expect_s3_class(with_pattern(gList(lazy), "stripe", delay = TRUE), "delayed_pattern")
	# as can a grob with its shapes already (to the same effect)
	rect <- rectGrob(x = c(0.25, 0.75), width = 0.4, name = "rects")
	delayed <- with_pattern(rect, "stripe", fill = "red", delay = TRUE)
	expect_equal(makeContent(delayed)$children[[1L]], with_pattern(rect, "stripe", fill = "red"))
	# the checks which don't need the number of areas are still made now
	expect_error(with_pattern(lazy, "stripe", "red", delay = TRUE), "must be named")
	expect_error(with_pattern(lazy, "stripe", gp = gpar(), delay = TRUE), "`gp` can't be given")
	expect_error(with_pattern(lazy, "stripe", delay = NA), "`delay` must be")
	expect_error(with_pattern(lazy, "stripe", layers = "bogus", delay = TRUE), "must be one of")
	expect_error(with_pattern(lazy, "stripe", legend = "foo", delay = TRUE), "`legend` must be")
	expect_error(with_pattern(lazy, "stripe", group = c(1, NA), delay = TRUE), "`group` must be")
	# while those which do are made when it is drawn
	delayed <- with_pattern(lazy, "stripe", fill = c("red", "blue", "green"), delay = TRUE)
	cnd <- tryCatch(grid.draw(delayed), error = identity)
	expect_match(conditionMessage(cnd), "delay = TRUE", fixed = TRUE)
	expect_identical(conditionCall(cnd)[[1L]], quote(with_pattern))
	expect_match(conditionMessage(cnd$parent), "one value per area (2)", fixed = TRUE)
})

test_that("`delay = TRUE` fills each dot of a `{ggplot2}` dot stack", {
	skip_if_not_installed("ggplot2", "3.5.0")
	skip_if(getRversion() < "4.2.0")
	dotstackGrob <- get0("dotstackGrob", asNamespace("ggplot2"), mode = "function")
	skip_if(is.null(dotstackGrob))
	dots <- dotstackGrob(
		x = unit(0.5, "npc"),
		y = unit(0.2, "npc"),
		stackaxis = "y",
		dotdia = 0.15,
		stackposition = 0:2,
		stackdir = "up",
		stackratio = 1,
		gp = gpar(fill = c("red", "blue", "green"), col = "black")
	)
	expect_length(area_boundaries(dots), 0L)
	delayed <- with_pattern(
		dots,
		c("stripe", "circle", NA),
		fill = c("red", "blue", "green"),
		delay = TRUE
	)
	local_temp_dev()
	pushViewport(viewport())
	out <- makeContent(delayed)$children[[1L]]
	expect_equal(childNames(out), c("area.1", "area.2", "shape.3"))
	expect_equal(getGrob(out, gPath("area.2", "pattern", "area.2"))$args$pattern_fill, "blue")
	expect_no_error(grid.draw(delayed))
})

test_that("`pattern = NA` leaves an area as it is", {
	rect <- rectGrob(
		x = c(0.25, 0.5, 0.75),
		width = 0.2,
		name = "rects",
		gp = gpar(fill = c("red", "blue", "green"))
	)
	out <- with_pattern(rect, c("stripe", NA, "circle"), fill = "black")
	expect_equal(childNames(out), c("area.1", "shape.2", "area.3"))
	shape <- getGrob(out, "shape.2")
	expect_equal(shape$x, unit(0.5, "npc"))
	expect_equal(shape$gp$fill, "blue")
	# (the pattern grobs keep the numbers of their areas)
	expect_equal(getGrob(out, gPath("area.3", "pattern", "area.3"))$args$pattern_fill, "black")
	# a grob whose areas are all left as they are is returned unchanged
	expect_identical(with_pattern(rect, NA), rect)
	expect_identical(with_pattern(rect, NA_character_), rect)
	tri <- polygonGrob(c(0.1, 0.9, 0.5), c(0.1, 0.1, 0.9), name = "tri")
	tree <- gTree(children = gList(rect, tri), name = "tree")
	out <- with_pattern(tree, c(NA, NA, NA, "stripe"))
	expect_identical(getGrob(out, "rects"), rect)
	expect_equal(childNames(getGrob(out, "tri")), c("fill", "pattern", "border"))
	# the pattern parameters still have one value per area
	out <- with_pattern(rect, c("stripe", NA, "stripe"), fill = c("red", "blue", "green"))
	expect_equal(getGrob(out, gPath("area.3", "pattern", "area.3"))$args$pattern_fill, "green")
	# a `group` can be left as it is
	out <- with_pattern(rect, c("stripe", NA), group = c(1, 2, 1))
	expect_equal(childNames(out), c("area.1", "shape.2"))
	# with `layers = "grob"` the area is drawn with the fills and borders of the others
	out <- with_pattern(rect, c("stripe", NA, "circle"), layers = "grob")
	expect_equal(childNames(out), c("fill", "pattern", "border"))
	expect_equal(childNames(getGrob(out, "pattern")), c("area.1", "area.3"))
	expect_error(with_pattern(rect, TRUE), "must be a character vector")
})

test_that("`delay = TRUE` and `pattern = NA` visual tests", {
	skip_if_not_installed("vdiffr")
	skip_on_ci()
	lazy <- lazy_rects()
	vdiffr::expect_doppelganger("delay", function() {
		grid.draw(with_pattern(lazy, c("stripe", NA), fill = "blue", density = 0.4, delay = TRUE))
	})
})
