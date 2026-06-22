test_that("`update_alpha()` works as expected", {
	expect_equal(update_alpha("blue", 0.5), "#0000FF80")
	expect_equal(update_alpha(list(c("blue", "red")), NA), c("#0000FFFF", "#FF0000FF"))
	expect_equal(update_alpha("#00FF0080", NA), "#00FF0080")
	expect_equal(update_alpha("#FF000080", 0.25), "#FF000040")

	expect_error(update_alpha(list("blue", "red"), NA))
	skip_if_not(getRversion() >= "4.1.0")
	expect_equal(
		update_pattern_alpha("red", 0.5, name = "nonrandom"),
		pattern(rectGrob(name = "nonrandom"), gp = gpar(fill = "#FF000080"))
	)
	expect_equal(
		update_pattern_alpha(linearGradient(c("black", "white")), 0.5),
		linearGradient(c("#00000080", "#FFFFFF80"))
	)
})

test_that("`mean_col()` works as expected", {
	expect_equal(mean_col("black", "white"), "#B4B4B4FF")
	expect_equal(mean_col(c("black", "white")), "#B4B4B4FF")
	expect_equal(mean_col("red", "blue"), "#B400B4FF")
})

test_that("`get_params()` works as expected", {
	params <- get_params()
	expect_equal(params$pattern_colour, "grey20")
	expect_equal(params$pattern_fill, "grey80")
	expect_equal(params$pattern_angle, 30)
	expect_equal(params$pattern_density, 0.2)
	expect_equal(params$pattern_spacing, 0.05)
	expect_equal(params$pattern_xoffset, 0)
	expect_equal(params$pattern_yoffset, 0)
	expect_equal(params$pattern_alpha, NA_real_)
	expect_equal(params$pattern_linetype, 1)
	expect_equal(params$pattern_linewidth, 1)

	params <- get_params(alpha = 0.5, spacing = 0.1)
	expect_equal(params$pattern_alpha, 0.5)
	expect_equal(params$pattern_spacing, 0.1)

	gp <- gpar(col = "blue", lty = 2)
	params <- get_params(gp = gp)
	expect_equal(params$pattern_colour, "blue")
	expect_equal(params$pattern_linetype, 2)
})

test_that("`star_scale()` works as expected", {
	# |8/3| star has internal angle 45 degrees and external angle 90 degrees
	scale <- star_scale(8, 45)
	scale2 <- star_scale(8, 90, external = TRUE)
	expect_equal(scale, scale2)
	expect_equal(star_angle(8, scale), 45)
	expect_equal(star_angle(8, scale, external = TRUE), 90)
	expect_equal(star_angle(2, star_scale(2, 30)), 30)
	expect_equal(star_angle(2, star_scale(2, 210, TRUE), TRUE), 210)
})

test_that("`assert_patterns_unique()` works as expected", {
	expect_null(assert_patterns_unique(list(), list()))
	expect_null(assert_patterns_unique(list(custom1 = 2), list(custom2 = 2)))
	expect_error(
		assert_patterns_unique(list(custom = 2, custom = 3), list()),
		'There are multiple custom "geometry" patterns named "custom"'
	)
	expect_error(
		assert_patterns_unique(list(), list(custom = 2, custom = 3)),
		'There are multiple custom "array" patterns named "custom"'
	)
	expect_error(
		assert_patterns_unique(list(custom = 2), list(custom = 2)),
		'There is a custom "geometry" pattern and custom "array" pattern both named "custom"'
	)
	expect_error(
		assert_patterns_unique(list(circle = 2), list()),
		'There is a custom "geometry" pattern and builtin \\{gridpattern\\} pattern both named "circle"'
	)
	expect_error(
		assert_patterns_unique(list(), list(image = 2)),
		'There is a custom "array" pattern and builtin \\{gridpattern\\} pattern both named "image"'
	)
})

test_that("`mix_col()` works as expected", {
	skip_on_cran()
	skip_if_not_installed("aqp")
	expect_equal(mix_col(c("red", "blue")), "#C2008FFF")
	expect_equal(mix_col(c("yellow", "green")), "#ACFF00FF")
	expect_equal(mix_col(c("red", "yellow", "blue"), w = c(2, 1, 1)), "#F46666FF")
})

test_that("`names_hatch()` works as expected", {
	# default is combinatorial, returns color equivalents
	nms <- names_hatch()
	expect_type(nms, "character")
	expect_true(all(!duplicated(nms)))
	expect_true(all(c("red", "blue", "green", "purple", "yellow", "black", "white") %in% nms))
	expect_true(all(c("magenta", "teal", "violet", "orange", "lime green") %in% nms))
	expect_true(all(c("pink", "grey", "mint green", "light blue", "lavender") %in% nms))

	# fox-davies returns heraldic names
	fd <- names_hatch("fox-davies")
	expect_true(all(!duplicated(fd)))
	expect_true(all(c("gules", "azure", "vert", "purpure", "sable", "argent", "or") %in% fd))
	expect_true(all(
		c(
			"eisenfarbe",
			"brunatre",
			"sanguine",
			"tenne",
			"carnation",
			"cendree",
			"orange",
			"bleu celeste",
			"proper"
		) %in%
			fd
	))

	# goodman: has new tinctures, lacks eisenfarbe and proper
	gd <- names_hatch("goodman")
	expect_true(all(!duplicated(gd)))
	expect_true(all(c("murrey", "steel", "copper", "bronze", "lead") %in% gd))
	expect_false("eisenfarbe" %in% gd)
	expect_false("proper" %in% gd)

	# unicode returns color equivalents matching emoji heart colors
	un <- names_hatch("unicode")
	expect_true(all(!duplicated(un)))
	expect_true(all(
		c(
			"red",
			"blue",
			"green",
			"yellow",
			"purple",
			"black",
			"white",
			"brown",
			"orange",
			"light blue",
			"grey",
			"pink"
		) %in%
			un
	))

	# accent substitutes accented spellings
	fd_acc <- names_hatch("fox-davies", accent = TRUE)
	expect_true("tenné" %in% fd_acc)
	expect_true("brunâtre" %in% fd_acc)
	expect_true("cendrée" %in% fd_acc)
	expect_true("bleu céleste" %in% fd_acc)
	expect_false("tenne" %in% fd_acc)

	# subtype matching is case-insensitive and ignores hyphens
	expect_equal(names_hatch("Fox-Davies"), names_hatch("fox-davies"))
	expect_equal(names_hatch("UNICODE"), names_hatch("unicode"))
	expect_equal(names_hatch("Goodman"), names_hatch("goodman"))
})

test_that("`grid.pattern_hatch()` warns on unsupported type/subtype combinations", {
	x <- c(0, 0, 1, 1)
	y <- c(1, 0, 0, 1)
	render <- function(expr) {
		f <- tempfile(fileext = ".pdf")
		on.exit(unlink(f))
		prev_dev <- dev.cur()
		pdf(f)
		on.exit(
			{
				suppressWarnings(dev.off())
				if (prev_dev > 1L) dev.set(prev_dev)
			},
			add = TRUE
		)
		force(expr)
	}
	# unknown type errors
	expect_error(
		render(grid.pattern_hatch(x, y, type = "notacolor")),
		"Unknown hatching type"
	)
	# type valid globally but not in this subtype errors
	expect_error(
		render(grid.pattern_hatch(x, y, type = "violet", subtype = "fox-davies")),
		"not supported by the 'fox-davies' subtype"
	)
	expect_error(
		render(grid.pattern_hatch(x, y, type = "proper", subtype = "goodman")),
		"not supported by the 'goodman' subtype"
	)
})

test_that("`assert_suggested()` works as expected", {
	expect_error(
		assert_suggested("doesnotexist", pattern = "blueberry"),
		"The suggested package \\{doesnotexist\\} must be installed in order to use the \"blueberry\" pattern"
	)
	expect_error(
		assert_suggested("doesnotexist", fn = "blueberry"),
		"The suggested package \\{doesnotexist\\} must be installed in order to use `blueberry\\(\\)`"
	)
})
