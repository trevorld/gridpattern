# adds `prefix` to the names of list `l`
prefix_args <- function(l, prefix = "pattern_") {
	if (length(l)) {
		names(l) <- paste0(prefix, names(l))
	}
	l
}

# parameters for which a length-one `NA` is treated as "not supplied" (see `complete_params()`)
NA_IS_UNSET <- c("pattern_type", "pattern_subtype", "pattern_gravity", "pattern_res")

# lengths that `pattern_key_scale_factor` zooms in a (numeric) legend key if `pattern_units` isn't relative
# (relative lengths are instead zoomed by the larger legend viewport, see `makeContent.pattern()`)
KEY_SCALED_LENGTHS <- c("pattern_spacing", "pattern_xoffset", "pattern_yoffset")
RELATIVE_UNITS <- c("snpc", "npc")

# fills in missing pattern parameters in list `l` (whose names are already prefixed)
# `defaults` are pattern-specific defaults that replace (or are appended to) `GENERIC_DEFAULTS`
# `key_zoom` is whether to zoom by `pattern_key_scale_factor` (i.e. drawn in a numeric legend key)
complete_params <- function(
	l,
	pattern = "none",
	gp = gpar(),
	defaults = list(),
	key_zoom = FALSE
) {
	l$pattern <- pattern
	# The generic defaults not overridden by the pattern are filled first (in `GENERIC_DEFAULTS` order)
	# and then the pattern-specific `defaults` in the order given, so a pattern-specific
	# (function) default may use any generic parameter and any earlier pattern-specific one.
	generic <- GENERIC_DEFAULTS[!(names(GENERIC_DEFAULTS) %in% names(defaults))]
	all_defaults <- c(generic, defaults)
	# `{ggpattern}` passes `NaN` for an unset aesthetic so treat a length-one `NaN` as "not supplied"
	# and let the defaults (if any) fill them in.
	# This is done for every parameter (not just those with a default) so an unset aesthetic looks the same
	# as one that doesn't exist (`NULL`) i.e. `{ggpattern}` can add new aesthetics and patterns can add
	# new defaults without changing what a `NaN` means.
	# NB. `is.na(NaN)` is `TRUE` but `is.nan(NA)` is `FALSE`
	for (nm in names(l)) {
		value <- l[[nm]]
		if (is.numeric(value) && length(value) == 1L && is.nan(value)) {
			l[[nm]] <- NULL
		}
	}
	# Also treat a length-one `NA` as "not supplied" for these parameters (for which `NA` has no meaning).
	# NB. `NA` is meaningful for other parameters e.g. a transparent colour, an invisible linewidth,
	# or the "pch" shape that draws nothing
	for (nm in NA_IS_UNSET) {
		value <- l[[nm]]
		if (is.atomic(value) && length(value) == 1L && is.na(value)) {
			l[[nm]] <- NULL
		}
	}
	fill_default <- function(l, nm) {
		value <- all_defaults[[nm]]
		if (is.function(value)) {
			value <- value(l, gp)
			if (length(value) == 0L) {
				abort(c(
					glue(
						"The default for `{nm}` of pattern \"{pattern}\" returned a length-zero value."
					),
					i = "A function default may only use parameters filled in before it (see `?register_pattern`)."
				))
			}
		}
		value
	}
	# In a (numeric) legend key `pattern_key_scale_factor` zooms the absolute (non-relative unit) lengths
	# as soon as each is filled in so any later (function) defaults computed from them
	# (e.g. the "wave" amplitude) are zoomed too.
	zoom <- 1
	if (key_zoom) {
		for (nm in c("pattern_key_scale_factor", "pattern_units")) {
			l[[nm]] <- l[[nm]] %||% fill_default(l, nm)
		}
		k <- check_key_scale_factor(l$pattern_key_scale_factor)
		zoom <- ifelse(l$pattern_units %in% RELATIVE_UNITS, 1, k)
		for (nm in intersect(KEY_SCALED_LENGTHS, names(l))) {
			l[[nm]] <- zoom * l[[nm]]
		}
	}
	for (nm in names(all_defaults)) {
		if (is.null(l[[nm]])) {
			l[[nm]] <- fill_default(l, nm)
			if (nm %in% KEY_SCALED_LENGTHS) {
				l[[nm]] <- zoom * l[[nm]]
			}
		}
	}
	l
}

check_key_scale_factor <- function(k) {
	if (!isTRUE(is.numeric(k) && length(k) == 1L && is.finite(k) && k > 0)) {
		abort("`pattern_key_scale_factor` must be a single positive number")
	}
	k
}

# Values are either constants or `function(params, gp)`
GENERIC_DEFAULTS <- list(
	# possibly get from gpar()
	pattern_alpha = function(params, gp) gp$alpha %||% NA_real_,
	pattern_colour = function(params, gp) params$pattern_color %||% gp$col %||% "grey20",
	pattern_fill = function(params, gp) gp$fill %||% "grey80",
	pattern_lineend = function(params, gp) gp$lineend %||% "round",
	pattern_linetype = function(params, gp) gp$lty %||% 1,
	pattern_linewidth = function(params, gp) params$pattern_size %||% gp$lwd %||% 0.5,
	pattern_size = function(params, gp) gp$lwd %||% 0.5,
	pattern_fontfamily = function(params, gp) gp$fontfamily %||% "sans",
	pattern_fontface = function(params, gp) gp$fontface %||% "plain",

	# never get from gpar()
	pattern_angle = 30,
	pattern_aspect_ratio = NA_real_,
	pattern_density = 0.2,
	pattern_filename = "",
	pattern_fill2 = "#4169E1",
	pattern_filter = "lanczos",
	pattern_frequency = 0.1,
	pattern_gravity = "center",
	pattern_grid = "square",
	pattern_key_scale_factor = 1,
	pattern_orientation = "vertical",
	pattern_rot = 0,
	pattern_shape = 1,
	pattern_scale = 1,
	pattern_spacing = 0.05,
	pattern_subtype = NA,
	pattern_type = NA_character_,
	pattern_units = "snpc",
	pattern_reverse = FALSE,
	pattern_stagger = FALSE,
	pattern_xoffset = 0,
	pattern_yoffset = 0,
	# spare `{ggpattern}` aesthetics not used by any builtin pattern
	pattern_phase = 0,
	pattern_option_1 = 0,
	pattern_option_2 = 0,
	pattern_option_3 = 0,
	pattern_option_4 = 0,
	pattern_option_5 = 0,
	pattern_res = function(params, gp) getOption("ggpattern_res", 72) # in PPI
	# NB. generic defaults are filled before the pattern-specific ones so a generic function default
	# must not rely on a pattern-specific default (`params$pattern_size` above is user-supplied or `NULL`)
)

get_R4.1_params <- function(l) {
	# R 4.1 features
	l$pattern_use_R4.1_clipping <- l$pattern_use_R4.1_clipping %||%
		getOption("ggpattern_use_R4.1_clipping") %||%
		getOption("ggpattern_use_R4.1_features") %||%
		guess_has_R4.1_features("clippingPaths")
	l$pattern_use_R4.1_gradients <- l$pattern_use_R4.1_gradients %||%
		getOption("ggpattern_use_R4.1_gradients") %||%
		getOption("ggpattern_use_R4.1_features") %||%
		guess_has_R4.1_features("gradients")
	l$pattern_use_R4.1_masks <- l$pattern_use_R4.1_masks %||%
		getOption("ggpattern_use_R4.1_masks") %||%
		getOption("ggpattern_use_R4.1_features") %||%
		guess_has_R4.1_features("masks")
	l$pattern_use_R4.1_patterns <- l$pattern_use_R4.1_patterns %||%
		getOption("ggpattern_use_R4.1_patterns") %||%
		getOption("ggpattern_use_R4.1_features") %||%
		guess_has_R4.1_features("patterns")
	l
}

convert_params_units <- function(params, units = "bigpts") {
	p_units <- params$pattern_units
	params$pattern_spacing <- convertX(
		unit(params$pattern_spacing, p_units),
		units,
		valueOnly = TRUE
	)
	params$pattern_xoffset <- convertX(
		unit(params$pattern_xoffset, p_units),
		units,
		valueOnly = TRUE
	)
	params$pattern_yoffset <- convertX(
		unit(params$pattern_yoffset, p_units),
		units,
		valueOnly = TRUE
	)
	params
}
