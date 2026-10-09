create_pattern_polygon <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	grid::polygonGrob(
		boundary_df$x,
		boundary_df$y,
		boundary_df$id,
		gp = grid::gpar(fill = params$pattern_fill)
	)
}

# pattern that records the `params` it is drawn with
captured <- new.env()
create_pattern_capture <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
	captured$params <- params
	grid::nullGrob()
}

draw <- function(grob) {
	grDevices::pdf(NULL)
	on.exit(grDevices::dev.off())
	grid::grid.draw(grob)
	invisible(NULL)
}

key <- function(pattern) {
	entry <- resolve_pattern(pattern)
	pattern_key(entry$owner, entry$name)
}

# these tests (un)register and (un)prefer patterns with these names so
# skip them if there are pre-existing custom patterns or preferences
# (e.g. registered by another loaded package) that they could clobber or be affected by
local({
	# fmt: skip
	names <- c("brick", "capture", "custom", "later", "local",
	           "nope", "polygon", "stripe", "unknown", "wave")
	df <- registered_patterns()
	preexisting <- intersect(names, c(df$name[df$source != "builtin"], ls(pattern_preferences)))
	if (length(preexisting)) {
		skip(paste("Pre-existing custom patterns or preferences:", toString(preexisting)))
	}
})

test_that("`register_pattern()` works for user patterns", {
	on.exit(unregister_pattern("capture", env = globalenv()), add = TRUE)
	expect_null(register_pattern("capture", create_pattern_capture, env = globalenv()))
	expect_equal(key("capture"), "R_GlobalEnv::capture")
	draw(patternGrob("capture", fill = "blue"))
	expect_equal(captured$params$pattern, "capture")
	expect_equal(captured$params$pattern_fill, "blue")
	df <- registered_patterns()
	expect_true(df$active[df$name == "capture"])
	expect_equal(df$source[df$name == "capture"], "register_pattern")

	# re-registering returns previous registration and doesn't message
	expect_silent(old <- register_pattern("capture", create_pattern_capture, env = globalenv()))
	expect_equal(old$owner, "R_GlobalEnv")

	expect_equal(unregister_pattern("capture", env = globalenv())$name, "capture")
	expect_null(unregister_pattern("capture", env = globalenv()))
	expect_error(makeContent(patternGrob("capture")), 'Unknown pattern "capture"')
})

test_that("patterns are resolved at draw time", {
	on.exit(unregister_pattern("later", env = globalenv()), add = TRUE)
	# grob may be created before its pattern is registered
	grob <- patternGrob("later", fill = "red")
	register_pattern("later", create_pattern_capture, env = globalenv())
	draw(grob)
	expect_equal(captured$params$pattern, "later")

	# `editGrob()` can change the pattern
	captured$params <- NULL
	draw(grid::editGrob(patternGrob("stripe"), pattern = "later"))
	expect_equal(captured$params$pattern, "later")
	draw(grid::editGrob(grob, args = list(pattern_fill = "green")))
	expect_equal(captured$params$pattern_fill, "green")
})

test_that("redraws use the pattern resolved at creation", {
	grDevices::pdf(NULL)
	on.exit(grDevices::dev.off(), add = TRUE)
	grDevices::dev.control("enable")
	draw_local <- function(name, fill) {
		local_pattern(name, create_pattern_capture, env = globalenv())
		prefer_pattern(paste0("R_GlobalEnv::", name))
		on.exit(unprefer_pattern(name), add = TRUE)
		suppressMessages(grid.pattern(name, fill = fill))
	}

	# pattern no longer registered
	draw_local("local", "red")
	expect_error(resolve_pattern("local"), "Unknown pattern")
	captured$params <- NULL
	grid::grid.refresh() # replays display list like resizing a device
	expect_equal(captured$params$pattern_fill, "red")

	# pattern no longer preferred over a builtin
	grid::grid.newpage()
	draw_local("stripe", "blue")
	captured$params <- NULL
	grid::grid.refresh()
	expect_equal(captured$params$pattern_fill, "blue")

	# pattern since conflicted
	on.exit(
		{
			unregister_pattern("brick", env = asNamespace("grid"))
			unregister_pattern("brick", env = asNamespace("utils"))
		},
		add = TRUE
	)
	grid::grid.newpage()
	register_pattern("brick", create_pattern_capture, env = asNamespace("grid"))
	grid.pattern("brick", fill = "orange")
	register_pattern("brick", create_pattern_polygon, env = asNamespace("utils"))
	expect_error(resolve_pattern("brick"), "registered by 2 owners")
	captured$params <- NULL
	grid::grid.refresh()
	expect_equal(captured$params$pattern_fill, "orange")

	# can force re-resolution
	grid::grid.newpage()
	grob <- local({
		local_pattern("stripe", create_pattern_capture, env = globalenv())
		prefer_pattern("R_GlobalEnv::stripe")
		withr::defer(unprefer_pattern("stripe"))
		suppressMessages(patternGrob("stripe", fill = "green"))
	})
	captured$params <- NULL
	grid::grid.draw(grid::editGrob(grob, entry = NULL))
	expect_null(captured$params)
})

test_that("user patterns conflict with builtin patterns", {
	on.exit(unregister_pattern("wave", env = globalenv()), add = TRUE)
	expect_message(
		register_pattern("wave", create_pattern_capture, env = globalenv()),
		'conflicts with "gridpattern::wave"'
	)
	expect_error(key("wave"), 'Pattern "wave" registered by 2 owners')
	expect_equal(key("R_GlobalEnv::wave"), "R_GlobalEnv::wave")
	expect_equal(key("gridpattern::wave"), "gridpattern::wave")
	draw(patternGrob("R_GlobalEnv::wave"))
	# builtin "wave" defaults aren't used for custom "wave" pattern
	expect_equal(captured$params$pattern, "wave")
	expect_true(is.na(captured$params$pattern_type))
	wave_defaults <- resolve_pattern("gridpattern::wave")$defaults
	expect_equal(complete_params(list(), "wave", defaults = wave_defaults)$pattern_type, "indented")
	df <- registered_patterns()
	expect_equal(df$active[df$name == "wave"], c(FALSE, FALSE))
	unregister_pattern("wave", env = globalenv())
	expect_equal(key("wave"), "gridpattern::wave")
})

test_that("conflict error message", {
	on.exit(
		{
			unregister_pattern("brick", env = asNamespace("grid"))
			unregister_pattern("brick", env = asNamespace("utils"))
		},
		add = TRUE
	)
	register_pattern("brick", create_pattern_capture, env = asNamespace("utils"))
	register_pattern("brick", create_pattern_polygon, env = asNamespace("grid"))
	err <- tryCatch(resolve_pattern("brick"), error = identity)
	lines <- strsplit(conditionMessage(err), "\n")[[1L]]
	expected <- c(
		'Pattern "brick" registered by 2 owners.',
		"Either pick the one you want with `::`:",
		'"grid::brick"',
		'"utils::brick"',
		"Or declare a preference with `prefer_pattern()`:",
		'`prefer_pattern("grid::brick")`',
		'`prefer_pattern("utils::brick")`'
	)
	expect_length(lines, length(expected))
	for (i in seq_along(expected)) {
		expect_match(lines[i], expected[i], fixed = TRUE)
	}
})

test_that("package patterns", {
	env_grid <- asNamespace("grid")
	env_utils <- asNamespace("utils")
	on.exit(
		{
			unregister_pattern("brick", env = env_grid)
			unregister_pattern("brick", env = env_utils)
			unregister_pattern("stripe", env = env_grid)
		},
		add = TRUE
	)
	register_pattern("brick", create_pattern_capture, env = env_grid)
	expect_equal(key("brick"), "grid::brick")
	register_pattern("brick", create_pattern_polygon, env = env_utils)
	expect_error(makeContent(patternGrob("brick")), 'Pattern "brick" registered by 2 owners')
	expect_equal(key("utils::brick"), "utils::brick")
	expect_equal(key("grid::brick"), "grid::brick")
	# qualified names are passed to pattern functions as short names
	draw(patternGrob("grid::brick"))
	expect_equal(captured$params$pattern, "brick")
	df <- registered_patterns()
	expect_equal(df$active[df$name == "brick"], c(FALSE, FALSE))

	# package patterns conflict with builtins
	expect_silent(register_pattern("stripe", create_pattern_polygon, env = env_grid))
	expect_error(key("stripe"), 'Pattern "stripe" registered by 2 owners')
	expect_equal(key("gridpattern::stripe"), "gridpattern::stripe")
	expect_equal(key("grid::stripe"), "grid::stripe")

	# user patterns conflict with package patterns
	on.exit(unregister_pattern("brick", env = globalenv()), add = TRUE)
	expect_message(
		register_pattern("brick", create_pattern_polygon, env = globalenv()),
		"conflicts with \"grid::brick\", \"utils::brick\""
	)
	expect_error(key("brick"), 'Pattern "brick" registered by 3 owners')
	expect_equal(key("R_GlobalEnv::brick"), "R_GlobalEnv::brick")
})

test_that("`local_pattern()` works", {
	f <- function() {
		local_pattern("local", create_pattern_capture, env = globalenv())
		key("local")
	}
	expect_equal(f(), "R_GlobalEnv::local")
	expect_error(resolve_pattern("local"), "Unknown pattern")

	# restores previous registration
	on.exit(unregister_pattern("local", env = globalenv()), add = TRUE)
	register_pattern(
		"local",
		create_pattern_capture,
		defaults = list(spacing = 1),
		env = globalenv()
	)
	g <- function() {
		old <- local_pattern("local", create_pattern_polygon, env = globalenv())
		expect_equal(old$defaults, list(pattern_spacing = 1))
		h <- function() {
			local_pattern(
				"local",
				create_pattern_capture,
				defaults = list(spacing = 3),
				env = globalenv()
			)
			resolve_pattern("local")$defaults$pattern_spacing
		}
		expect_equal(h(), 3)
		resolve_pattern("local")$fn
	}
	expect_identical(g(), create_pattern_polygon)
	expect_equal(resolve_pattern("local")$defaults, list(pattern_spacing = 1))
	unregister_pattern("local", env = globalenv())

	# nested registrations in the same frame are undone in reverse order
	k <- function() {
		local_pattern("local", create_pattern_capture, env = globalenv())
		local_pattern("local", create_pattern_polygon, env = globalenv())
		resolve_pattern("local")$fn
	}
	expect_identical(k(), create_pattern_polygon)
	expect_error(resolve_pattern("local"), "Unknown pattern")

	# can be scoped to a test
	local({
		local_pattern("local", create_pattern_capture, env = globalenv())
		expect_equal(key("local"), "R_GlobalEnv::local")
	})
	expect_error(resolve_pattern("local"), "Unknown pattern")
})

test_that("pattern-specific defaults", {
	on.exit(unregister_pattern("capture", env = globalenv()), add = TRUE)
	register_pattern(
		"capture",
		create_pattern_capture,
		defaults = list(
			type = "running",
			spacing = 0.1,
			# may use earlier (generic) defaults
			amplitude = function(params, gp) 2 * params$pattern_spacing,
			# may use the gpar
			size = function(params, gp) gp$fontsize %||% 10,
			# new parameters are appended
			mortar = function(params, gp) params$pattern_amplitude / 2
		),
		env = globalenv()
	)
	draw(patternGrob("capture"))
	expect_equal(captured$params$pattern_type, "running")
	expect_equal(captured$params$pattern_spacing, 0.1)
	expect_equal(captured$params$pattern_amplitude, 0.2)
	expect_equal(captured$params$pattern_size, 10)
	expect_equal(captured$params$pattern_mortar, 0.1)
	expect_equal(captured$params$pattern_density, 0.2) # generic default

	# supplied parameters take priority (including `NA` "type" from `{ggpattern}`)
	draw(patternGrob("capture", spacing = 0.2, size = 3, gp = gpar(fontsize = 20)))
	expect_equal(captured$params$pattern_spacing, 0.2)
	expect_equal(captured$params$pattern_amplitude, 0.4)
	expect_equal(captured$params$pattern_size, 3)
	draw(patternGrob("capture", type = NA, gp = gpar(fontsize = 20)))
	expect_equal(captured$params$pattern_type, "running")
	expect_equal(captured$params$pattern_size, 20)

	expect_error(register_pattern("capture", create_pattern_capture, defaults = 1), "named list")
	expect_error(
		register_pattern("capture", create_pattern_capture, defaults = list(1)),
		"unique, non-empty names"
	)
	expect_error(
		register_pattern("capture", create_pattern_capture, defaults = list(a = 1, a = 2)),
		"unique, non-empty names"
	)
	expect_error(
		register_pattern("capture", create_pattern_capture, defaults = list(a = function(x) x)),
		"function\\(params, gp\\)"
	)
})

test_that("helper functions can register patterns on behalf of the user", {
	on.exit(unregister_pattern("polygon", env = globalenv()), add = TRUE)
	helper <- function(name, fn, env = parent.frame()) {
		register_pattern(name, fn, env = env)
	}
	environment(helper) <- asNamespace("grid") # pretend `helper()` is in a package
	user_env <- new.env(parent = globalenv())
	user_env$helper <- helper
	user_env$fn <- create_pattern_polygon
	evalq(helper("polygon", fn), user_env)
	expect_equal(key("polygon"), "R_GlobalEnv::polygon")
})

test_that("legacy options", {
	old <- options(
		ggpattern_geometry_funcs = list(polygon = create_pattern_polygon),
		ggpattern_array_funcs = NULL
	)
	on.exit(options(old), add = TRUE)
	expect_equal(key("polygon"), "R_GlobalEnv::polygon")
	df <- registered_patterns()
	expect_equal(df$source[df$name == "polygon"], "options")
	expect_true(df$active[df$name == "polygon"])

	# `register_pattern()` and options patterns with the same name conflict
	on.exit(unregister_pattern("polygon", env = globalenv()), add = TRUE)
	expect_message(
		register_pattern("polygon", create_pattern_polygon, env = globalenv()),
		"conflicts with a pattern set by `options\\(\\)`"
	)
	df <- registered_patterns()
	expect_equal(df$active[df$name == "polygon"], c(FALSE, FALSE))
	expect_equal(conflicting_patterns()$name, c("polygon", "polygon"))
	expect_error(resolve_pattern("polygon"), 'multiple patterns named "polygon"')
	expect_error(
		resolve_pattern("R_GlobalEnv::polygon"),
		'multiple patterns named "polygon"'
	)
	err <- tryCatch(resolve_pattern("polygon"), error = identity)
	lines <- strsplit(conditionMessage(err), "\n")[[1L]]
	expected <- c(
		'There are multiple patterns named "polygon" owned by "R_GlobalEnv".',
		'Registered by `register_pattern()` (remove with `unregister_pattern("polygon")`)',
		'Set by `options("ggpattern_geometry_funcs")`',
		"Remove all but one of them (`prefer_pattern()` can't choose between them)."
	)
	expect_length(lines, length(expected))
	for (i in seq_along(expected)) {
		expect_match(lines[i], expected[i], fixed = TRUE)
	}
	# can't be resolved with a preference
	expect_error(prefer_pattern("R_GlobalEnv::polygon"), "multiple patterns")
	unregister_pattern("polygon", env = globalenv())
	expect_equal(key("polygon"), "R_GlobalEnv::polygon")

	# options conflict with builtins
	options(ggpattern_geometry_funcs = list(stripe = create_pattern_polygon))
	expect_error(key("stripe"), 'Pattern "stripe" registered by 2 owners')
	expect_equal(key("R_GlobalEnv::stripe"), "R_GlobalEnv::stripe")
	expect_equal(key("gridpattern::stripe"), "gridpattern::stripe")
	on.exit(unprefer_pattern("stripe"), add = TRUE)
	prefer_pattern("R_GlobalEnv::stripe")
	expect_equal(key("stripe"), "R_GlobalEnv::stripe")
	unprefer_pattern("stripe")

	options(
		ggpattern_geometry_funcs = list(custom = create_pattern_polygon),
		ggpattern_array_funcs = list(custom = create_pattern_polygon)
	)
	expect_error(resolve_pattern("custom"), 'multiple patterns named "custom"')
	expect_error(resolve_pattern("R_GlobalEnv::custom"), 'multiple patterns named "custom"')
	options(ggpattern_geometry_funcs = list(custom = 2, custom = 3), ggpattern_array_funcs = NULL)
	expect_error(resolve_pattern("custom"), 'multiple patterns named "custom"')
})

test_that("`grid.pattern()` throws resolution errors before drawing", {
	grDevices::pdf(NULL)
	on.exit(grDevices::dev.off(), add = TRUE)
	expect_error(grid.pattern("unknown"), 'Unknown pattern "unknown"')
	expect_s3_class(grid.pattern("unknown", draw = FALSE), "pattern")
})

test_that("`has_pattern()`", {
	env_grid <- asNamespace("grid")
	on.exit(
		{
			unprefer_pattern("stripe")
			unregister_pattern("brick", env = env_grid)
			unregister_pattern("stripe", env = globalenv())
		},
		add = TRUE
	)
	expect_error(has_pattern(1), "must be a character vector")
	expect_equal(has_pattern(character(0)), logical(0))
	expect_equal(
		has_pattern(c("stripe", "gridpattern::stripe", "brick", "grid::brick", NA, "")),
		c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
	)
	register_pattern("brick", create_pattern_capture, env = env_grid)
	expect_equal(has_pattern(c("brick", "grid::brick", "utils::brick")), c(TRUE, TRUE, FALSE))

	# unresolved conflicts are `FALSE` but qualified names still work
	suppressMessages(register_pattern("stripe", create_pattern_polygon, env = globalenv()))
	expect_equal(
		has_pattern(c("stripe", "gridpattern::stripe", "R_GlobalEnv::stripe")),
		c(FALSE, TRUE, TRUE)
	)
	prefer_pattern("R_GlobalEnv::stripe")
	expect_true(has_pattern("stripe"))
})

test_that("`conflicting_patterns()`, `prefer_pattern()`, and `unprefer_pattern()`", {
	env_grid <- asNamespace("grid")
	env_utils <- asNamespace("utils")
	on.exit(
		{
			unprefer_pattern("brick")
			unprefer_pattern("stripe")
			unregister_pattern("brick", env = env_grid)
			unregister_pattern("brick", env = env_utils)
			unregister_pattern("stripe", env = globalenv())
		},
		add = TRUE
	)
	expect_equal(nrow(conflicting_patterns()), 0L)
	register_pattern("brick", create_pattern_capture, env = env_grid)
	expect_equal(nrow(conflicting_patterns()), 0L)
	register_pattern("brick", create_pattern_polygon, env = env_utils)
	df <- conflicting_patterns()
	expect_equal(df$name, c("brick", "brick"))
	expect_equal(df$owner, c("grid", "utils"))
	expect_equal(df$active, c(FALSE, FALSE))
	expect_error(resolve_pattern("brick"), "prefer_pattern")

	# preferences resolve ambiguous names
	expect_equal(prefer_pattern("utils::brick"), character(0L))
	expect_equal(key("brick"), "utils::brick")
	expect_equal(conflicting_patterns()$active, c(FALSE, TRUE))
	expect_equal(prefer_pattern("grid::brick"), "utils::brick")
	expect_equal(key("brick"), "grid::brick")

	# preferences have priority over user patterns
	expect_message(
		register_pattern("brick", create_pattern_polygon, env = globalenv()),
		'masked by preferred "grid::brick"'
	)
	on.exit(unregister_pattern("brick", env = globalenv()), add = TRUE)
	expect_equal(key("brick"), "grid::brick")
	prefer_pattern("R_GlobalEnv::brick")
	expect_equal(key("brick"), "R_GlobalEnv::brick")

	# can prefer builtins over user patterns
	suppressMessages(register_pattern("stripe", create_pattern_polygon, env = globalenv()))
	expect_error(key("stripe"), "registered by 2 owners")
	prefer_pattern("gridpattern::stripe")
	expect_equal(key("stripe"), "gridpattern::stripe")
	df <- conflicting_patterns()
	expect_equal(df$active[df$name == "stripe"], c(TRUE, FALSE))

	# removing preferences
	expect_equal(unprefer_pattern("stripe"), "gridpattern::stripe")
	expect_equal(unprefer_pattern("stripe"), character(0L))
	expect_error(key("stripe"), "registered by 2 owners")

	# no message if user pattern is already preferred
	prefer_pattern("R_GlobalEnv::stripe")
	unregister_pattern("stripe", env = globalenv())
	expect_silent(register_pattern("stripe", create_pattern_polygon, env = globalenv()))
	expect_equal(key("stripe"), "R_GlobalEnv::stripe")
	unprefer_pattern("stripe")

	# preferred pattern no longer available
	prefer_pattern("utils::brick")
	unregister_pattern("brick", env = env_utils)
	expect_error(
		resolve_pattern("brick"),
		'Preferred pattern "utils::brick" for "brick" is no longer registered'
	)
	expect_error(resolve_pattern("brick"), 'unprefer_pattern("utils::brick")', fixed = TRUE)

	expect_error(prefer_pattern("notapackage::brick"), 'Unknown pattern "notapackage::brick"')
	expect_error(prefer_pattern("gridpattern::nope"), 'Unknown pattern "gridpattern::nope"')
	expect_error(prefer_pattern("stripe"), "unprefer_pattern")
	expect_error(prefer_pattern("a::b::c"), "qualified name")
	expect_error(prefer_pattern("::stripe"), "qualified name")
	expect_error(prefer_pattern("gridpattern::"), "qualified name")
	expect_error(prefer_pattern(NA_character_), "qualified name")
	expect_error(
		prefer_pattern(c("gridpattern::stripe", "a::b::c", "d")),
		'Invalid: "a::b::c", "d"'
	)
	expect_error(prefer_pattern(1), "qualified name")
	expect_error(unprefer_pattern("a::b::c"), "bare name")
	expect_error(unprefer_pattern(""), "bare name")
	expect_error(unprefer_pattern(NA_character_), "bare name")
	expect_error(unprefer_pattern(NULL), "bare name")

	# qualified names only remove a preference for that pattern
	prefer_pattern("gridpattern::stripe")
	expect_equal(unprefer_pattern("R_GlobalEnv::stripe"), "gridpattern::stripe")
	expect_equal(key("stripe"), "gridpattern::stripe")
	expect_equal(unprefer_pattern("gridpattern::stripe"), "gridpattern::stripe")
	expect_equal(unprefer_pattern("gridpattern::stripe"), character(0L))

	# temporarily changing a preference
	prefer_pattern("gridpattern::stripe")
	new <- "R_GlobalEnv::stripe"
	old <- prefer_pattern(new)
	expect_equal(key("stripe"), new)
	unprefer_pattern(new)
	prefer_pattern(old)
	expect_equal(key("stripe"), "gridpattern::stripe")
	unprefer_pattern("stripe")

	# empty input does nothing
	expect_equal(prefer_pattern(NULL), character(0L))
	expect_equal(prefer_pattern(character(0L)), character(0L))
	expect_equal(unprefer_pattern(character(0L)), character(0L))
	expect_error(unprefer_pattern(NULL), "bare names")

	# vectorized
	register_pattern("brick", create_pattern_polygon, env = env_utils)
	prefer_pattern("utils::brick")
	new <- c("grid::brick", "R_GlobalEnv::stripe")
	old <- prefer_pattern(new)
	expect_equal(old, "utils::brick") # no previous "stripe" preference
	expect_equal(key("brick"), "grid::brick")
	expect_equal(key("stripe"), "R_GlobalEnv::stripe")
	expect_equal(sort(unprefer_pattern(new)), sort(new))
	expect_error(key("stripe"), "registered by 2 owners")
	prefer_pattern(old)
	expect_equal(key("brick"), "utils::brick")
	expect_error(key("stripe"), "registered by 2 owners")
	# validates everything before changing anything
	expect_error(prefer_pattern(c("grid::brick", "gridpattern::nope")), "Unknown pattern")
	expect_equal(key("brick"), "utils::brick")
	expect_error(
		prefer_pattern(c("grid::brick", "utils::brick")),
		'more than one pattern named "brick"'
	)
	expect_equal(key("brick"), "utils::brick")
	# exact duplicates are fine
	expect_equal(prefer_pattern(c("grid::brick", "grid::brick")), "utils::brick")
	expect_equal(key("brick"), "grid::brick")
	prefer_pattern("utils::brick")
	# bare and qualified names may be mixed
	prefer_pattern("gridpattern::stripe")
	expect_equal(
		unprefer_pattern(c("grid::brick", "stripe", "brick")),
		c("utils::brick", "gridpattern::stripe")
	)
	expect_equal(nrow(conflicting_patterns()[conflicting_patterns()$active, ]), 0L)

	# legacy options patterns may be preferred
	unregister_pattern("stripe", env = globalenv())
	old <- options(ggpattern_geometry_funcs = list(stripe = create_pattern_polygon))
	on.exit(options(old), add = TRUE)
	prefer_pattern("R_GlobalEnv::stripe")
	expect_equal(resolve_pattern("stripe")$source, "options")
	unprefer_pattern("stripe")
})

test_that("`names_pattern` matches the registered builtin patterns", {
	df <- registered_patterns()
	expect_setequal(names_pattern, df$name[df$owner == "gridpattern"])
})

test_that("input validation", {
	expect_error(register_pattern("stripe", create_pattern_polygon), "Builtin")
	expect_error(unregister_pattern("stripe"), "Builtin")
	expect_error(register_pattern("a::b", create_pattern_polygon), 'must not contain ":"')
	expect_error(register_pattern("a:b", create_pattern_polygon), 'must not contain ":"')
	expect_error(local_pattern("a:b", create_pattern_polygon), 'must not contain ":"')
	expect_error(unregister_pattern("a:b"), 'must not contain ":"')
	expect_error(register_pattern("", create_pattern_polygon), "non-empty string")
	expect_error(register_pattern("polygon", "polygon"), "must be a function")
	expect_error(resolve_pattern("gridpattern::nope"), 'Unknown pattern "gridpattern::nope"')
	expect_error(resolve_pattern("notapackage::nope"), 'Unknown pattern "notapackage::nope"')
	expect_error(resolve_pattern(NA_character_), "non-empty string")
})
