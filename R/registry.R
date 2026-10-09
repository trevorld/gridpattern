#' Register patterns
#'
#' `register_pattern()` registers a pattern so it can be used by
#' [grid.pattern()] / [patternGrob()] (and `{ggpattern}`).
#' `unregister_pattern()` removes a pattern previously registered by the same owner
#' (as determined by `env`); it never removes patterns registered by other packages
#' or the builtin `{gridpattern}` patterns.
#'
#' Each pattern is \dQuote{owned} by either the package that registered it
#' or by the user (if registered from the global environment e.g. in a script or at the console).
#' Patterns may always be referred to by their qualified name
#' `"owner::name"` e.g. `"gridpattern::stripe"`, `"pkg::name"`, or `"R_GlobalEnv::name"`.
#' A bare pattern name like `"stripe"` is resolved as follows:
#'
#' 1. If the user has set a preference with [prefer_pattern()] then the preferred pattern.
#' 2. If exactly one owner (the user, `{gridpattern}`, or a loaded package) has a pattern with that name
#'    then that pattern.
#'    Patterns set by the user with the (legacy) `ggpattern_geometry_funcs` / `ggpattern_array_funcs` options
#'    are owned by the user.
#'    If the user has more than one pattern with that name (from `register_pattern()` and/or the options)
#'    an error is thrown asking them to remove all but one of them
#'    (since they share the same qualified name `"R_GlobalEnv::name"` a preference can't choose between them).
#' 3. Otherwise there is a conflict and an error is thrown, asking for either a qualified `"owner::name"`
#'    or a [prefer_pattern()] preference.
#'    In particular, a user pattern with the same name as a builtin `{gridpattern}` pattern
#'    does not mask the builtin pattern without a preference.
#'
#' [conflicting_patterns()] lists the patterns whose bare names are shared by more than one pattern.
#'
#' [patternGrob()] resolves the pattern name when the grob is created (if the pattern is registered)
#' so redrawing the grob (e.g. when resizing an interactive graphics device)
#' still works even if the pattern is later unregistered or conflicted
#' (e.g. by [local_pattern()] going out of scope).
#' Changing the grob's pattern with [grid::editGrob()] resolves the new pattern name when drawn.
#'
#' Packages should register their patterns in their `.onLoad()` function.
#' We recommend wrapping the registration in [rlang::on_package_load()]
#' so the pattern is registered whenever `{gridpattern}` is loaded
#' (allowing `{gridpattern}` to be in `Suggests` and surviving `{gridpattern}` being reloaded).
#' Since a `Suggests` version requirement isn't enforced when installing or loading packages,
#' also check that the installed `{gridpattern}` has `register_pattern()`
#' (and otherwise fall back to setting the legacy `ggpattern_geometry_funcs` option):
#'
#' ```r
#' .onLoad <- function(libname, pkgname) {
#'   rlang::on_package_load("gridpattern", {
#'     if (exists("register_pattern", envir = asNamespace("gridpattern"), inherits = FALSE)) {
#'       gridpattern::register_pattern("brick", create_pattern_brick)
#'     } else {
#'       funcs <- getOption("ggpattern_geometry_funcs", list())
#'       funcs[["brick"]] <- create_pattern_brick
#'       options(ggpattern_geometry_funcs = funcs)
#'     }
#'   })
#' }
#' ```
#'
#' Package code that relies on a particular pattern should use its qualified name
#' (e.g. `"gridpattern::stripe"`) since other packages (or users) may register patterns
#' with the same bare name.
#'
#' @param name Pattern name.  Must not contain `":"`.
#' @param fn Pattern function.  Its required signature depends on `kind`:
#'        \describe{
#'          \item{geometry}{`function(params, boundary_df, aspect_ratio, legend)`
#'            returning a grid grob that lies within the boundary
#'            (e.g. clipped to it).
#'            `params` is a list of (prefixed) pattern parameters e.g. `params$pattern_fill`,
#'            `boundary_df` is a data frame with `x`, `y`, and `id` columns
#'            describing the boundary polygon(s) in \dQuote{npc} coordinates,
#'            `aspect_ratio` is the (best guess) aspect ratio of the viewport, and
#'            `legend` is whether the pattern is being drawn in a `{ggpattern}` legend key.}
#'          \item{array}{`function(width, height, params, legend)`
#'            returning a 3D array of RGBA values (all values in the range \[0, 1\])
#'            which `{gridpattern}` will mask to the boundary.
#'            `width` and `height` are the dimensions (in pixels) of the boundary's bounding box
#'            and `params` and `legend` are as for geometry patterns.}
#'        }
#'        See the \dQuote{Developing Patterns} vignette
#'        `vignette("developing-patterns", package = "gridpattern")`
#'        for more details and examples.
#' @param kind Either `"geometry"` or `"array"`.
#' @param defaults A named list of pattern-specific default parameter values
#'                 (without the `pattern_` prefix) e.g. `list(type = "running", spacing = 0.1)`.
#'                 Values may be constants or functions of the form `function(params, gp)`
#'                 where `params` is the list of (prefixed) pattern parameters filled in so far
#'                 and `gp` is the grob's [grid::gpar()] object
#'                 e.g. `list(amplitude = function(params, gp) 0.5 * params$pattern_spacing)`.
#'                 These replace the generic defaults for parameters that weren't supplied.
#'                 Note `{ggpattern}` supplies its own (generic) defaults for most of its pattern aesthetics
#'                 so these will mainly be used with [grid.pattern()] / [patternGrob()].
#' @param env The environment used to determine the \dQuote{owner} of the pattern.
#'            If writing a function that registers patterns on behalf of its caller
#'            add an `env = rlang::caller_env()` argument to that function and pass it on.
#' @return `register_pattern()` and `unregister_pattern()` invisibly return the
#'         previous registration (a list) or `NULL` if there wasn't one.
#' @seealso [registered_patterns()] and [has_pattern()] to list available patterns.
#'          [local_pattern()] to temporarily register a pattern.
#'          [prefer_pattern()] and [conflicting_patterns()] to resolve pattern name conflicts.
#' @examples
#' create_pattern_polygon <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
#'   grid::polygonGrob(boundary_df$x, boundary_df$y, boundary_df$id,
#'                     gp = grid::gpar(fill = params$pattern_fill))
#' }
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' # don't clobber any pre-existing "polygon" pattern
#' if (!has_pattern("polygon")) {
#'   register_pattern("polygon", create_pattern_polygon)
#'   grid::grid.newpage()
#'   grid.pattern("polygon", x_hex, y_hex, fill = "blue")
#'   unregister_pattern("polygon")
#' }
#' @export
register_pattern <- function(
	name,
	fn,
	kind = c("geometry", "array"),
	defaults = list(),
	env = caller_env()
) {
	assert_pattern_name(name)
	if (!is.function(fn)) {
		abort("`fn` must be a function.")
	}
	kind <- match.arg(kind)
	defaults <- prefix_defaults(defaults)
	owner <- registration_owner(env)
	key <- pattern_key(owner, name)
	old <- pattern_registry[[key]]
	if (owner == USER_OWNER && is.null(old)) {
		inform_conflict(name)
	}
	assign(
		key,
		pattern_entry(name, owner, kind, fn, defaults = defaults),
		envir = pattern_registry
	)
	invisible(old)
}

#' @rdname register_pattern
#' @export
unregister_pattern <- function(name, env = caller_env()) {
	assert_pattern_name(name)
	key <- pattern_key(registration_owner(env), name)
	old <- pattern_registry[[key]]
	rlang::env_unbind(pattern_registry, key)
	invisible(old)
}

#' List available patterns
#'
#' `registered_patterns()` returns a data frame of all available patterns.
#' `has_pattern()` returns whether pattern names currently resolve to a pattern
#' i.e. whether [grid.pattern()] / [patternGrob()] could use them.
#' See [register_pattern()] for how bare pattern names are resolved.
#'
#' @param pattern A character vector of bare pattern names `"name"` and/or
#'                qualified pattern names `"owner::name"`.
#' @return `registered_patterns()` returns `r r2i_registered_patterns_df`
#'         `has_pattern()` returns a logical vector the same length as `pattern`.
#'         A bare name with an unresolved conflict (see [conflicting_patterns()]) returns `FALSE`.
#' @seealso [register_pattern()] to register patterns.
#' @examples
#' head(registered_patterns())
#' has_pattern(c("stripe", "gridpattern::stripe", "polygon"))
#'
#' create_pattern_polygon <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
#'   grid::polygonGrob(boundary_df$x, boundary_df$y, boundary_df$id,
#'                     gp = grid::gpar(fill = params$pattern_fill))
#' }
#' # don't clobber any pre-existing "polygon" pattern
#' run_example <- !has_pattern("polygon")
#' if (run_example) {
#'   register_pattern("polygon", create_pattern_polygon)
#'   print(subset(registered_patterns(), name == "polygon"))
#' }
#' if (run_example) {
#'   print(has_pattern("polygon"))
#' }
#' if (run_example) {
#'   unregister_pattern("polygon")
#' }
#' @export
registered_patterns <- function() {
	entries <- c(registry_entries(), option_entries())
	field <- function(f) vapply(entries, `[[`, character(1L), f)
	df <- data.frame(
		name = field("name"),
		owner = field("owner"),
		kind = field("kind"),
		source = field("source")
	)
	# resolve each name once
	active <- lapply(rlang::set_names(unique(df$name)), function(name) {
		tryCatch(resolve_pattern(name), error = function(e) NULL)
	})
	df$active <- vapply(
		seq_along(entries),
		function(i) identical(active[[df$name[i]]], entries[[i]]),
		logical(1L)
	)
	df <- df[order(tolower(df$name), df$owner != "gridpattern", df$owner, df$source), ]
	rownames(df) <- NULL
	df
}

#' @rdname registered_patterns
#' @export
has_pattern <- function(pattern) {
	if (!is.character(pattern)) {
		abort("`pattern` must be a character vector.")
	}
	vapply(
		pattern,
		function(p) !is.null(tryCatch(resolve_pattern(p), error = function(e) NULL)),
		logical(1L),
		USE.NAMES = FALSE
	)
}

#' Temporarily register a pattern
#'
#' `local_pattern()` registers a pattern (see [register_pattern()])
#' until the current function (or test, see `.local_envir`) exits
#' and then restores the previous registration (if any).
#' Requires the suggested `{withr}` package.
#'
#' @inheritParams register_pattern
#' @param .local_envir The environment whose exit should undo `local_pattern()`'s registration.
#' @return `local_pattern()` invisibly returns the
#'         previous registration (a list) or `NULL` if there wasn't one.
#' @seealso [register_pattern()] to (permanently) register patterns.
#' @examples
#' create_pattern_polygon <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
#'   grid::polygonGrob(boundary_df$x, boundary_df$y, boundary_df$id,
#'                     gp = grid::gpar(fill = params$pattern_fill))
#' }
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' draw_polygon <- function() {
#'   local_pattern("polygon", create_pattern_polygon)
#'   grid::grid.newpage()
#'   grid.pattern("polygon", x_hex, y_hex, fill = "red")
#' }
#' # don't clobber (or be confused by) any pre-existing "polygon" pattern
#' if (requireNamespace("withr", quietly = TRUE) && !has_pattern("polygon")) {
#'   draw_polygon()
#' }
#' print(has_pattern("polygon"))
#' @export
local_pattern <- function(
	name,
	fn,
	kind = c("geometry", "array"),
	defaults = list(),
	env = caller_env(),
	.local_envir = parent.frame()
) {
	rlang::check_installed("withr", "to use `local_pattern()`.")
	old <- register_pattern(name, fn, kind = kind, defaults = defaults, env = env)
	key <- pattern_key(registration_owner(env), name)
	withr::defer(restore_pattern(key, old), envir = .local_envir)
	invisible(old)
}

# restores registration `old` (possibly `NULL`) for `key`
restore_pattern <- function(key, old) {
	if (is.null(old)) {
		rlang::env_unbind(pattern_registry, key)
	} else {
		assign(key, old, envir = pattern_registry)
	}
	invisible(NULL)
}

#' Resolve pattern name conflicts
#'
#' `prefer_pattern()` sets which pattern a bare pattern name resolves to,
#' taking priority over all other patterns with that name.
#' `unprefer_pattern()` removes such preferences.
#' `conflicting_patterns()` returns the available patterns whose bare name is shared
#' by more than one pattern (see [register_pattern()] for how bare names are resolved).
#' Using such a bare name throws an error unless a preference has been set.
#'
#' `prefer_pattern()` and `unprefer_pattern()` are vectorized and return the previous preferences (if any)
#' so they may be used to temporarily change preferences:
#'
#' ```r
#' old <- prefer_pattern(new)
#' # ... use patterns ...
#' unprefer_pattern(new)
#' prefer_pattern(old) # does nothing if `old` is empty
#' ```
#'
#' Preferences only last for the current R session.
#' Users may put `prefer_pattern()` calls in their `.Rprofile` (or scripts)
#' to make them persistent.
#' Packages should not call `prefer_pattern()` and should instead
#' refer to patterns by their qualified name `"pkg::name"`.
#'
#' @param pattern A character vector.
#'                For `prefer_pattern()` the qualified names `"owner::name"` of the preferred patterns
#'                where `owner` is either the package that registered it
#'                (`"gridpattern"` for the builtin patterns) or `"R_GlobalEnv"`
#'                for patterns registered by the user
#'                e.g. `c("gridpattern::stripe", "R_GlobalEnv::wave")`.
#'                Can't contain more than one pattern with the same bare name.
#'                If length zero (e.g. `character(0)` or `NULL`) then `prefer_pattern()` does nothing.
#'                For `unprefer_pattern()` bare pattern names e.g. `"stripe"`
#'                (removes any preference for that name) and/or qualified names
#'                (only removes the preference if it is for that pattern).
#' @return `prefer_pattern()` and `unprefer_pattern()` invisibly return a character vector of the
#'         qualified names of the previous preferences (for the bare names in `pattern`)
#'         that existed (`character(0)` if there weren't any).
#'         `conflicting_patterns()` returns (for just the conflicting patterns)
#'         `r r2i_registered_patterns_df`
#' @examples
#' create_pattern_polygon <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
#'   grid::polygonGrob(boundary_df$x, boundary_df$y, boundary_df$id,
#'                     gp = grid::gpar(fill = params$pattern_fill))
#' }
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' # don't clobber any pre-existing user "stripe" pattern
#' run_example <- !has_pattern("R_GlobalEnv::stripe")
#' if (run_example) {
#'   # temporarily remove any pre-existing "stripe" preference
#'   old <- unprefer_pattern("stripe")
#'   register_pattern("stripe", create_pattern_polygon)
#' }
#'
#' if (run_example) {
#'   print(conflicting_patterns())
#' }
#'
#' if (run_example) {
#'   # bare "stripe" is now ambiguous
#'   grid::grid.newpage()
#'   try(grid.pattern("stripe", x_hex, y_hex))
#' }
#'
#' if (run_example) {
#'   # prefer the user's "stripe" pattern over the builtin "stripe" pattern
#'   prefer_pattern("R_GlobalEnv::stripe")
#'   grid::grid.newpage()
#'   grid.pattern("stripe", x_hex, y_hex, fill = "blue")
#' }
#'
#' if (run_example) {
#'   print(conflicting_patterns())
#' }
#'
#' if (run_example) {
#'   # remove the preference
#'   unprefer_pattern("stripe")
#'   # remove "R_GlobalEnv::stripe" (not "gridpattern::stripe")
#'   unregister_pattern("stripe")
#'   # restore any pre-existing preference
#'   prefer_pattern(old)
#' }
#' @export
prefer_pattern <- function(pattern) {
	if (length(pattern) == 0L) {
		return(invisible(character(0L)))
	}
	assert_pattern_names(
		pattern,
		QUALIFIED_REGEX,
		c(
			'`pattern` must be qualified names `"owner::name"`.',
			i = "Use `unprefer_pattern()` to remove a preference."
		)
	)
	entries <- lapply(unique(pattern), resolve_qualified_pattern) # errors if no such pattern
	names <- vapply(entries, `[[`, character(1L), "name")
	if (anyDuplicated(names)) {
		name <- names[anyDuplicated(names)]
		abort(glue('Can\'t prefer more than one pattern named "{name}".'))
	}
	old <- preferred_patterns(names)
	for (entry in entries) {
		assign(entry$name, entry$owner, envir = pattern_preferences)
	}
	invisible(old)
}

#' @rdname prefer_pattern
#' @export
unprefer_pattern <- function(pattern) {
	if (is.character(pattern) && length(pattern) == 0L) {
		return(invisible(character(0L)))
	}
	assert_pattern_names(
		pattern,
		"^([^:]+::)?[^:]+$",
		'`pattern` must be bare names `"name"` or qualified names `"owner::name"`.'
	)
	names <- bare_name(pattern)
	old <- preferred_patterns(unique(names))
	for (i in seq_along(pattern)) {
		current <- preferred_patterns(names[i])
		# qualified names only remove a preference for that pattern
		if (length(current) && (names[i] == pattern[i] || current == pattern[i])) {
			rlang::env_unbind(pattern_preferences, names[i])
		}
	}
	invisible(old)
}

#' @rdname prefer_pattern
#' @export
conflicting_patterns <- function() {
	df <- registered_patterns()
	df <- df[df$name %in% df$name[duplicated(df$name)], ]
	rownames(df) <- NULL
	df
}

QUALIFIED_REGEX <- "^[^:]+::[^:]+$"

assert_pattern_names <- function(pattern, regex, msg, call = caller_env()) {
	if (!is.character(pattern)) {
		abort(msg, call = call)
	}
	bad <- is.na(pattern) | !grepl(regex, pattern)
	if (any(bad)) {
		invalid <- paste(encodeString(pattern[bad], quote = '"'), collapse = ", ")
		abort(c(msg[1L], x = paste("Invalid:", invalid), msg[-1L]), call = call)
	}
	invisible(NULL)
}

# qualified names of the existing preferences for bare pattern names `names`
preferred_patterns <- function(names) {
	owners <- mget(names, envir = pattern_preferences, ifnotfound = NA_character_)
	owners <- unlist(owners, use.names = FALSE)
	pattern_key(owners, names)[!is.na(owners)]
}

pattern_registry <- new.env(parent = emptyenv())

# user preferences set by `prefer_pattern()`: bare pattern name -> owner
pattern_preferences <- new.env(parent = emptyenv())

USER_OWNER <- "R_GlobalEnv"

pattern_entry <- function(
	name,
	owner,
	kind,
	fn,
	source = "register_pattern",
	defaults = list()
) {
	list(name = name, owner = owner, kind = kind, fn = fn, source = source, defaults = defaults)
}

# validates `defaults` and adds "pattern_" prefix to names
prefix_defaults <- function(defaults) {
	if (!is.list(defaults)) {
		abort("`defaults` must be a named list.")
	}
	if (length(defaults) == 0L) {
		return(list())
	}
	nms <- names(defaults)
	if (is.null(nms) || any(is.na(nms) | !nzchar(nms)) || anyDuplicated(nms)) {
		abort("`defaults` must be a list with unique, non-empty names.")
	}
	for (nm in nms) {
		value <- defaults[[nm]]
		if (is.function(value) && length(formals(value)) < 2L) {
			abort(glue("Function default `{nm}` must have the form `function(params, gp)`."))
		}
	}
	names(defaults) <- paste0("pattern_", nms)
	defaults
}

pattern_owner <- function(env) {
	top <- topenv(env)
	if (isNamespace(top)) {
		unname(getNamespaceName(top))
	} else {
		USER_OWNER
	}
}

assert_pattern_name <- function(name) {
	if (!rlang::is_string(name) || !nzchar(name)) {
		abort("`name` must be a non-empty string.")
	}
	if (grepl(":", name, fixed = TRUE)) {
		abort(c('`name` must not contain ":".', i = "The pattern owner is determined by `env`."))
	}
	invisible(NULL)
}

# the (non-builtin) owner of patterns (un)registered from `env`
registration_owner <- function(env) {
	owner <- pattern_owner(env)
	if (owner == "gridpattern") {
		abort(c(
			"Builtin {gridpattern} patterns can't be registered or unregistered.",
			i = "Is `env` the {gridpattern} namespace?"
		))
	}
	owner
}

inform_conflict <- function(name) {
	if (length(option_entries(name))) {
		inform(c(
			glue('Pattern "{name}" conflicts with a pattern set by `options()`.'),
			i = "Remove one of them (`prefer_pattern()` can't choose between them)."
		))
	}
	preferred <- pattern_preferences[[name]]
	if (!is.null(preferred)) {
		if (preferred != USER_OWNER) {
			inform(glue('Pattern "{name}" is masked by preferred "{preferred}::{name}".'))
		}
		return(invisible(NULL))
	}
	others <- Filter(function(e) e$owner != USER_OWNER, registry_entries(name))
	if (length(others)) {
		keys <- vapply(others, function(e) pattern_key(e$owner, e$name), character(1L))
		conflicts <- paste0('"', keys, '"', collapse = ", ")
		inform(c(
			glue('Pattern "{name}" conflicts with {conflicts}.'),
			i = glue(
				'Use `prefer_pattern("{USER_OWNER}::{name}")` to use your pattern by default.'
			)
		))
	}
	invisible(NULL)
}

# qualified pattern name(s) "owner::name"
pattern_key <- function(owner, name) {
	paste0(owner, "::", name)
}

# bare pattern name(s) from (possibly) qualified pattern name(s)
bare_name <- function(pattern) {
	sub("^[^:]*::", "", pattern)
}

register_builtin_patterns <- function() {
	geometry <- list(
		aRtsy = create_pattern_aRtsy,
		circle = create_pattern_circle_via_sf,
		crosshatch = create_pattern_crosshatch_via_sf,
		fill = create_pattern_fill,
		gradient = create_pattern_gradient,
		hatch = create_pattern_hatch,
		line = create_pattern_line,
		none = create_pattern_none,
		pch = create_pattern_pch,
		polygon_tiling = create_pattern_polygon_tiling,
		regular_polygon = create_pattern_regular_polygon_via_sf,
		rose = create_pattern_rose,
		stripe = create_pattern_stripes_via_sf,
		text = create_pattern_text,
		wave = create_pattern_wave_via_sf,
		weave = create_pattern_weave_via_sf
	)
	array <- list(
		ambient = create_pattern_ambient,
		image = img_read_as_array_wrapper,
		magick = create_magick_pattern_as_array,
		placeholder = fetch_placeholder_array,
		plasma = create_magick_plasma_as_array
	)
	fns <- list(geometry = geometry, array = array)
	defaults <- builtin_defaults()
	for (kind in names(fns)) {
		for (name in names(fns[[kind]])) {
			entry <- pattern_entry(
				name,
				"gridpattern",
				kind,
				fns[[kind]][[name]],
				"builtin",
				prefix_defaults(defaults[[name]] %||% list())
			)
			assign(pattern_key("gridpattern", name), entry, envir = pattern_registry)
		}
	}
	invisible(NULL)
}

# builtin pattern-specific defaults (replacing `GENERIC_DEFAULTS`)
builtin_defaults <- function() {
	list(
		ambient = list(type = "simplex", frequency = 0.01),
		aRtsy = list(type = "strokes"),
		crosshatch = list(fill2 = function(params, gp) params$pattern_fill),
		hatch = list(type = "gules"),
		image = list(type = "fit"),
		magick = list(type = "hexagons", filter = "box"),
		placeholder = list(type = "bear"),
		polygon_tiling = list(type = "square"),
		regular_polygon = list(shape = "convex4", scale = 0.5),
		rose = list(frequency = 0.1),
		text = list(size = function(params, gp) gp$fontsize %||% 12),
		wave = list(type = "indented", amplitude = function(params, gp) {
			0.5 * params$pattern_spacing
		}),
		weave = list(type = "plain")
	)
}

# registry entries (optionally only those named `name`) whose owner is the user or a loaded package
registry_entries <- function(name = NULL) {
	keys <- ls(pattern_registry, all.names = TRUE)
	if (!is.null(name)) {
		keys <- keys[endsWith(keys, paste0("::", name))]
	}
	entries <- mget(keys, envir = pattern_registry)
	entries <- Filter(function(e) e$owner == USER_OWNER || isNamespaceLoaded(e$owner), entries)
	unname(entries)
}

# (legacy) patterns set by the user via `options()`
option_entries <- function(name = NULL) {
	geometry <- getOption("ggpattern_geometry_funcs")
	array <- getOption("ggpattern_array_funcs")
	if (!is.null(name)) {
		geometry <- geometry[names(geometry) == name]
		array <- array[names(array) == name]
	}
	entries <- c(
		lapply(names(geometry), function(n) {
			pattern_entry(n, USER_OWNER, "geometry", geometry[[n]], "options")
		}),
		lapply(names(array), function(n) {
			pattern_entry(n, USER_OWNER, "array", array[[n]], "options")
		})
	)
	entries
}

# the user's pattern named `name` (or `NULL`)
# errors if the user has more than one (from `register_pattern()` and/or `options()`)
# since they share the same qualified name so can't be chosen between
user_entry <- function(name) {
	entries <- c(list(pattern_registry[[pattern_key(USER_OWNER, name)]]), option_entries(name))
	entries <- Filter(Negate(is.null), entries)
	if (length(entries) > 1L) {
		abort_user_conflict(name, entries)
	}
	if (length(entries)) entries[[1L]]
}

abort_user_conflict <- function(name, entries) {
	sources <- vapply(
		entries,
		function(e) {
			if (e$source == "register_pattern") {
				glue(
					'Registered by `register_pattern()` (remove with `unregister_pattern("{name}")`)'
				)
			} else {
				glue('Set by `options("ggpattern_{e$kind}_funcs")`')
			}
		},
		character(1L)
	)
	abort(
		c(
			glue('There are multiple patterns named "{name}" owned by "{USER_OWNER}".'),
			rlang::set_names(sources, rep_len("*", length(sources))),
			i = "Remove all but one of them (`prefer_pattern()` can't choose between them)."
		),
		call = NULL
	)
}

# candidate entries for bare pattern name `name` with (at most) one entry per owner
pattern_candidates <- function(name) {
	entries <- Filter(function(e) e$owner != USER_OWNER, registry_entries(name))
	user <- user_entry(name)
	if (!is.null(user)) {
		entries <- c(entries, list(user))
	}
	entries
}

# returns registry entry for pattern `pattern`
resolve_pattern <- function(pattern) {
	if (!rlang::is_string(pattern) || !nzchar(pattern)) {
		abort("`pattern` must be a non-empty string.")
	}
	if (grepl("::", pattern, fixed = TRUE)) {
		return(resolve_qualified_pattern(pattern))
	}
	preferred <- pattern_preferences[[pattern]]
	if (!is.null(preferred)) {
		key <- pattern_key(preferred, pattern)
		return(lookup_qualified_pattern(key) %||% abort_missing_preference(pattern, key))
	}
	candidates <- pattern_candidates(pattern)
	if (length(candidates) == 1L) {
		return(candidates[[1L]])
	}
	if (length(candidates) == 0L) {
		abort_unknown_pattern(pattern)
	}
	abort_pattern_conflict(pattern, vapply(candidates, `[[`, character(1L), "owner"))
}

abort_pattern_conflict <- function(name, owners) {
	owners <- sort(owners)
	keys <- pattern_key(owners, name)
	qualified <- paste0('"', keys, '"')
	prefer <- paste0('`prefer_pattern("', keys, '")`')
	bullets <- rep_len("*", length(owners))
	abort(
		c(
			glue('Pattern "{name}" registered by {length(owners)} owners.'),
			" " = "Either pick the one you want with `::`:",
			rlang::set_names(qualified, bullets),
			" " = "Or declare a preference with `prefer_pattern()`:",
			rlang::set_names(prefer, bullets)
		),
		call = NULL
	)
}

abort_unknown_pattern <- function(pattern) {
	abort(c(
		glue('Unknown pattern "{pattern}".'),
		i = "See `registered_patterns()` for available patterns."
	))
}

abort_missing_preference <- function(name, key) {
	abort(
		c(
			glue('Preferred pattern "{key}" for "{name}" is no longer registered.'),
			i = glue('Use `unprefer_pattern("{key}")` to remove this preference.')
		),
		call = NULL
	)
}

resolve_qualified_pattern <- function(pattern) {
	lookup_qualified_pattern(pattern) %||% abort_unknown_pattern(pattern)
}

# returns registry entry for qualified pattern `pattern` or `NULL`
lookup_qualified_pattern <- function(pattern) {
	owner <- sub("::.*$", "", pattern)
	if (owner == USER_OWNER) {
		entry <- user_entry(bare_name(pattern))
	} else {
		if (!isNamespaceLoaded(owner)) {
			requireNamespace(owner, quietly = TRUE)
		}
		entry <- pattern_registry[[pattern]]
	}
	entry
}

get_pattern_fn <- function(entry) {
	if (entry$kind == "array") {
		function(...) create_pattern_array(..., array_fn = entry$fn)
	} else {
		entry$fn
	}
}
