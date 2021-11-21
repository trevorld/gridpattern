# returns list of pattern parameters using defaults if necessary
get_params <- function(..., pattern = "none", prefix = "pattern_", gp = gpar()) {
    l <- list(...)
    if (length(l)) names(l) <- paste0(prefix, names(l))
    l$pattern <- pattern

    # possibly get from gpar()
    l$pattern_alpha <- l$pattern_alpha %||% gp$alpha %||% NA_real_
    l$pattern_colour <- l$pattern_colour %||% l$pattern_color %||% gp$col %||% "grey20"
    l$pattern_fill <- l[["pattern_fill"]] %||% gp$fill %||% "grey80"
    l$pattern_linetype <- l$pattern_linetype %||% gp$lty %||% 1
    if (pattern == "text")
        l$pattern_size <- l$pattern_size %||% gp$fontsize %||% 12
    else
        l$pattern_size <- l$pattern_size %||% gp$lwd %||% 1
    l$pattern_fontfamily <- l$pattern_fontfamily %||% gp$fontfamily %||% "sans"
    l$pattern_fontface <- l$pattern_fontface %||% gp$fontface %||% "plain"

    # never get from gpar()
    l$pattern_angle <- l$pattern_angle %||% 30
    l$pattern_aspect_ratio <- l$pattern_aspect_ratio %||% NA_real_
    l$pattern_density <- l$pattern_density %||% 0.2
    l$pattern_filename <- l$pattern_filename %||% ""
    l$pattern_fill2 <- l$pattern_fill2 %||%
        switch(pattern, crosshatch = l$pattern_fill, "#4169E1")
    l$pattern_filter <- l$pattern_filter %||%
        switch(pattern, magick = "box", "lanczos")
    l$pattern_gravity <- l$pattern_gravity %||% "center"
    l$pattern_grid <- l$pattern_grid %||% "square"
    l$pattern_key_scale_factor <- l$pattern_key_scale_factor %||% 1
    l$pattern_orientation <- l$pattern_orientation %||% "vertical"
    l$pattern_rot <- l$pattern_rot %||% 0
    l$pattern_shape <- l$pattern_shape %||%
        switch(pattern, regular_polygon = "convex4", 1)
    l$pattern_scale <- l$pattern_scale %||%
        switch(pattern, regular_polygon = 0.5, 1)
    l$pattern_spacing <- l$pattern_spacing %||% 0.05
    # l$pattern_subtype <- l$pattern_subtype
    l$pattern_type <- l$pattern_type %||% default_pattern_type(pattern)
    if (is.na(l$pattern_type))
        l$pattern_type <- default_pattern_type(pattern)
    l$pattern_xoffset <- l$pattern_xoffset %||% 0
    l$pattern_yoffset <- l$pattern_yoffset %||% 0

    l$pattern_res <- l$pattern_res %||% getOption("ggpattern_res", 72) # in PPI

    # Additional ambient defaults
    l$pattern_frequency <- l$pattern_frequency %||%
        switch(pattern, ambient = 0.01, rose = 0.1, 1 / l$pattern_spacing)
    l$pattern_interpolator <- l$pattern_interpolator %||% "quintic" # perlin, simplex, value
    l$pattern_fractal <- l$pattern_fractal %||%
        switch(l$pattern_type, worley = "none", "fbm")
    l$pattern_pertubation <- l$pattern_pertubation %||% "none" # all
    l$pattern_octaves <- l$pattern_octaves %||% 3 # all but white
    l$pattern_lacunarity <- l$pattern_lacunarity %||% 2 # all but white
    l$pattern_gain <- l$pattern_gain %||% 0.5 # all but white
    l$pattern_amplitude <- l$pattern_amplitude %||%
        switch(pattern, wave = 0.5 * l$pattern_spacing, 1) # all
    l$pattern_value <- l$pattern_value %||% "cell"
    l$pattern_distance_ind <- l$pattern_distance_ind %||% c(1, 2)
    l$pattern_jitter <- l$pattern_jitter %||% 0.45


    l
}

guess_has_R4.1_features <- function() {
    device <- names(grDevices::dev.cur())
    if (device %in% c("cairo_pdf", "pdf", "png", "svg", "X11cairo"))
        getRversion() >= '4.1.0'
    else
        FALSE
}

get_R4.1_params <- function(l) {
    # R 4.1 features
    default <- guess_has_R4.1_features()
    l$pattern_use_R4.1_clipping <- l$pattern_use_R4.1_clipping %||%
        getOption("ggpattern_use_R4.1_clipping") %||%
        getOption("ggpattern_use_R4.1_features") %||%
        default
    l$pattern_use_R4.1_gradients <- l$pattern_use_R4.1_gradients %||%
        getOption("ggpattern_use_R4.1_gradients") %||%
        getOption("ggpattern_use_R4.1_features") %||%
        default
    l$pattern_use_R4.1_masks <- l$pattern_use_R4.1_masks %||%
        getOption("ggpattern_use_R4.1_masks") %||%
        getOption("ggpattern_use_R4.1_features") %||%
        default
    l$pattern_use_R4.1_patterns <- l$pattern_use_R4.1_patterns %||%
        getOption("ggpattern_use_R4.1_patterns") %||%
        getOption("ggpattern_use_R4.1_features") %||%
        default
    l
}

convert_params_units <- function(params, units = "bigpts") {
    params$pattern_amplitude <- convertX(unit(params$pattern_amplitude, "snpc"), units, valueOnly = TRUE)
    params$pattern_spacing <- convertX(unit(params$pattern_spacing, "snpc"), units, valueOnly = TRUE)
    params$pattern_xoffset <- convertX(unit(params$pattern_xoffset, "snpc"), units, valueOnly = TRUE)
    params$pattern_yoffset <- convertX(unit(params$pattern_yoffset, "snpc"), units, valueOnly = TRUE)
    params$pattern_wavelength <- convertX(unit(1/params$pattern_frequency, "snpc"), units, valueOnly = TRUE)
    params
}

default_pattern_type <- function(pattern) {
    switch(pattern,
           ambient = "simplex",
           image = "fit",
           placeholder = "kitten",
           polygon_tiling = "square",
           magick = "hexagons",
           wave = "triangle",
           weave = "plain",
           NA_character_)
}
