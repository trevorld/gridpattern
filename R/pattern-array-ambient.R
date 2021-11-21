#' Ambient patterned grobs
#'
#' `grid.pattern_ambient()` draws noise patterns onto the graphic device powered by the `ambient` package.
#'
#' @param type Either cubic, perlin, simplex, value, white, or worley
#' @inheritParams grid.pattern_gradient
#' @inheritParams ambient::noise_simplex
#' @inheritParams ambient::noise_worley
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'  if (requireNamespace("ambient")) {
#'    x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    grid.pattern_ambient(x_hex, y_hex, fill = "green", fill2 = "blue")
#'    grid::grid.newpage()
#'    grid.pattern_ambient(x_hex, y_hex, fill = "green", fill2 = "blue", type = "cubic")
#'  }
#' @seealso For more information about the noise types please see the relevant `ambient` documentation:
#'   [ambient::noise_cubic()], [ambient::noise_perlin()], [ambient::noise_simplex()],
#'   [ambient::noise_value()], [ambient::noise_white()], and [ambient::noise_worley()].
#'  [grid.pattern_plasma()] provides an alternative noise pattern that depends on `magick`.
#' @export
grid.pattern_ambient <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                 type = "simplex", fill = gp$fill %||% "grey80", fill2 = "#4169E1",
                                 frequency = 0.01, interpolator = "quintic",
                                 fractal = switch(type, worley = "none", "fbm"),
                                 octaves = 3, lacunarity = 2, gain = 0.5,
                                 pertubation = "none", pertubation_amplitude = 1,
                                 value = "cell", distance_ind = c(1, 2), jitter = 0.45,
                                 res = getOption("ggpattern_res", 72), alpha = NA_real_,
                                 default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("ambient", x, y, id,
                 type = type, fill = fill, fill2 = fill2,
                 frequency = frequency, interpolator = interpolator, fractal = fractal,
                 octaves = octaves, lacunarity = lacunarity, gain = gain,
                 pertubation = pertubation, pertubation_amplitude = pertubation_amplitude,
                 value = value, distance_ind = distance_ind, jitter = jitter,
                 res = res, alpha = alpha,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#' Create an array of noise using the 'ambient' package
#'
#' @param width,height area dimensions
#' @param params aesthetic parameters passed from the geom e.g. 'pattern_fill',
#'        'pattern_frequency' etc.
#' @param legend logical. If the request to create a pattern comes during
#'        creation of the legend, then this is TRUE, otherwise FALSE
#'
#' @return an RGBA numeric array with dimensions [height, width, 4]
#' @noRd
create_pattern_ambient <- function(width, height, params, legend) {

  assert_suggested("ambient", "ambient")

  colour1 <- as.character(params$pattern_fill )
  colour2 <- as.character(params$pattern_fill2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a ramp function from these 2 colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ramp_func <- grDevices::colorRamp(c(colour1, colour2), alpha = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a noise matrix of the requested dimensions using 'ambient'.
  # The contents are normalised to all be in the range [0,1]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fn <- ambient_fn(params)
  noise_matrix  <- fn(dim = c(height, width))
  noise_matrix  <- ambient::normalise(noise_matrix)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use each value in the noise matrix to lookup a colour using the
  # colour ramp function, then ensure the results are an RGBA array of the
  # correct dimensions.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  colour_matrix <- ramp_func(noise_matrix)/255
  noise_array   <- array(colour_matrix, dim = c(height, width, 4))

  noise_array
}

ambient_fn <- function(params) {
    type <- params$pattern_type
    args <- list()
    args$frequency <- params$pattern_frequency
    args$pertubation <- params$pattern_pertubation
    args$pertubation_amplitude <- params$pattern_pertubation_amplitude
    if (type %in% c("perlin", "simplex", "value")) {
        args$interpolator <- params$pattern_interpolator
    }
    if (type != "white") {
        args$fractal <- params$pattern_fractal
        args$octaves <- params$pattern_octaves
        args$lacunarity <- params$pattern_lacunarity
        args$gain <- params$pattern_gain
    }
    if (type == "worley") {
        args$value <- params$pattern_value
        args$distance_ind <- params$pattern_distance_ind
        args$jitter <- params$pattern_jitter
    }
    function(dim) {
        args$dim <- dim
        fn <- switch(type,
               cubic = ambient::noise_cubic,
               perlin = ambient::noise_perlin,
               simplex = ambient::noise_simplex,
               value = ambient::noise_value,
               white = ambient::noise_white,
               worley = ambient::noise_worley,
               abort(glue("Don't know ambient type {type}")))
        do.call(fn, args)
    }
}
