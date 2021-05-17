#' Create patterned grobs
#'
#' `grid.pattern()` draws patterned shapes onto the graphic device.
#' `patternGrob()` returns the grid grob objects.
#'
#' Here is a list of the various patterns supported:
#'
#' \describe{
#' \item{ambient}{Noise array patterns onto the graphic device powered by the `ambient` package.
#'                See [grid.pattern_ambient()] for more information.}
#' \item{circle}{Circle geometry patterns.
#'               See [grid.pattern_circle()] for more information.}
#' \item{crosshatch}{Crosshatch geometry patterns.
#'                   See [grid.pattern_crosshatch()] for more information.}
#' \item{gradient}{Gradient array patterns.
#'                 See [grid.pattern_gradient()] for more information.}
#' \item{image}{Image array patterns.
#'              See [grid.pattern_image()] for more information.}
#' \item{magick}{`imagemagick` array patterns.
#'               See [grid.pattern_magick()] for more information.}
#' \item{none}{Does nothing.
#'             See [grid::grid.null()] for more information.}
#' \item{placeholder}{Placeholder image array patterns.
#'                    See [grid.pattern_placeholder()] for more information.}
#' \item{plasma}{Plasma array patterns.
#'               See [grid.pattern_plasma()] for more information.}
#' \item{stripe}{Stripe geometry patterns.
#'               See [grid.pattern_stripe()] for more information.}
#' \item{Custom geometry-based patterns}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html} for more information.}
#' \item{Custom array-based patterns}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html} for more information.}
#' }
#'
#' @inheritParams grid::polygonGrob
#' @import grid
#' @importFrom utils hasName
#' @param pattern Name of pattern.  See Details section for a list of supported patterns.
#' @param x A numeric vector or unit object specifying x-locations of the pattern boundary
#' @param y A numeric vector or unit object specifying y-locations of the pattern boundary
#' @param id A numeric vector used to separate locations in x, y into multiple boundaries.
#'           All locations within the same `id` belong to the same boundary.
#' @param ... Pattern parameters
#' @param legend Whether this is intended to be drawn in a legend or not
#' @param prefix Prefix to prepend to the name of each of the pattern parameters in `...`.
#'               For compatibility with `ggpattern` most underlying functions assume parameters beginning with `pattern_`.
#' @param default.units A string indicating the default units to use if `x` or `y`
#'                      are only given as numeric vectors.
#' @return A grid grob object (invisibly in the case of `grid.pattern()`).
#'         If `draw` is `TRUE` then `grid.pattern()` also draws to the graphic device as a side effect.
#' @examples
#'  if (require("grid")) {
#'    grid.newpage()
#'    grid.pattern()
#'    grid.newpage()
#'    grid.pattern("stripe", colour="blue", fill="yellow", density = 0.5, angle = 135)
#'    grid.newpage()
#'    # In some cases can also alternatively use "gpar()" to specify colour and line attributes
#'    x <- c(0.1, 0.6, 0.8, 0.3)
#'    y <- c(0.2, 0.3, 0.8, 0.5)
#'    grid.pattern("stripe", x, y, gp = gpar(col="blue", fill="red", lwd=2))
#'    grid.newpage()
#'    grid.pattern("crosshatch", colour="blue", fill="yellow", density = 0.5, angle = 135)
#'    grid.newpage()
#'    grid.pattern("circle", colour="blue", fill="yellow", size = 2, density = 0.5)
#'    grid.newpage()
#'    logo_filename <- system.file("img", "Rlogo.png" , package="png")
#'    grid.pattern("image", filename=logo_filename, type="tile")
#'    grid.newpage()
#'    grid.pattern("magick", type="octagons", fill="blue", scale=2)
#'    \dontrun{
#'      # requires internet connection to download from placeholder image websites
#'      grid.newpage()
#'      grid.pattern("placeholder", type="bear")
#'    }
#'    grid.newpage()
#'    grid.pattern("gradient", fill="blue", fill2="green", orientation="radial")
#'    grid.newpage()
#'    grid.pattern("plasma", fill="green")
#'  }
#' @seealso \url{https://coolbutuseless.github.io/package/ggpattern/index.html}
#'          for more details on the patterns and their parameters.
#' @export
grid.pattern <- function(pattern = "stripe",
                         x = c(0.5, 0.067, 0.067, 0.5, 0.933, 0.933),
                         y = c(1.0, 0.75, 0.25, 0.0, 0.25, 0.75), id = 1L, ...,
                         legend = FALSE, prefix = "pattern_",
                         default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grob <- patternGrob(pattern, x, y, id, ...,
                        legend = legend, prefix = prefix,
                        default.units = default.units, name = name, gp = gp, vp = vp)
    if (draw) grid.draw(grob)
    invisible(grob)
}

#' @rdname grid.pattern
#' @export
patternGrob <- function(pattern = "strip",
                        x = c(0.5, 0.067, 0.067, 0.5, 0.933, 0.933),
                        y = c(1.0, 0.75, 0.25, 0.0, 0.25, 0.75), id = 1L, ...,
                        legend = FALSE, prefix = "pattern_",
                        default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    params <- get_params(..., pattern = pattern, prefix = prefix, gp = gp)
    if (!inherits(x, "unit")) x <- unit(x, default.units)
    if (!inherits(y, "unit")) y <- unit(y, default.units)

    gTree(pattern=pattern, x=x, y=y, id=id, params=params, legend=legend,
          name=name, gp=gp, vp=vp, cl="pattern")
}

#' @export
makeContent.pattern <- function(x) {
    # avoid weird errors with array patterns if there is an active device open
    current_dev <- grDevices::dev.cur()
    on.exit(grDevices::dev.set(current_dev))

    xp <- as.numeric(convertX(x$x, "npc"))
    yp <- as.numeric(convertY(x$y, "npc"))
    id <- x$id
    boundary_df <- create_polygon_df(xp, yp, id)

    if (!is.na(x$params$pattern_aspect_ratio)) {
        aspect_ratio <- x$params$pattern_aspect_ratio
    } else {
        width <- as.numeric(convertWidth(unit(1, "npc"), "in"))
        height <- as.numeric(convertHeight(unit(1, "npc"), "in"))
        aspect_ratio <-  width / height
    }

    fn <- get_pattern_fn(x$pattern)
    grob <- fn(x$params, boundary_df, aspect_ratio, x$legend)
    gl <- gList(grob)
    setChildren(x, gl)
}

get_pattern_fn <- function(pattern) {
    geometry_fns <- c(getOption("ggpattern_geometry_funcs"),
                      list(circle = create_pattern_circles,
                           crosshatch = create_pattern_crosshatch_via_sf,
                           none = create_pattern_none,
                           stripe = create_pattern_stripes_via_sf))
    array_fns <- c(getOption("ggpattern_array_funcs"),
                   list(ambient = create_pattern_ambient,
                        gradient = create_gradient_as_array,
                        image = img_read_as_array_wrapper,
                        magick = create_magick_pattern_as_array,
                        placeholder = fetch_placeholder_array,
                        plasma = create_magick_plasma_as_array))
    array_fns <- lapply(array_fns, function(fn) {
                            function(...) create_pattern_array(..., array_fn=fn)
                   })
    fns <- c(geometry_fns, array_fns)
    fns[[pattern]] %||% abort("Don't know the function for pattern ", pattern)
}

# returns list of pattern parameters using defaults if necessary
get_params <- function(..., pattern = "none", prefix = "pattern_", gp = gpar()) {
    l <- list(...)
    if (length(l)) names(l) <- paste0(prefix, names(l))

    # possibly get from gpar()
    l$pattern_alpha <- l$pattern_alpha %||% gp$alpha %||% 1
    l$pattern_colour <- l$pattern_colour %||% l$pattern_color %||% gp$col %||% "grey20"
    l$pattern_fill <- l$pattern_fill %||% gp$fill %||% "grey80"
    l$pattern_linetype <- l$pattern_linetype %||% gp$lty %||% 1
    l$pattern_size <- l$pattern_size %||% gp$lwd %||% 1

    # never get from gpar()
    l$pattern_angle <- l$pattern_angle %||% 30
    l$pattern_aspect_ratio <- l$pattern_aspect_ratio %||% NA_real_
    l$pattern_density <- l$pattern_density %||% 0.2
    l$pattern_filename <- l$pattern_filename %||% ""
    l$pattern_fill2 <- l$pattern_fill2 %||% "#4169E1"
    l$pattern_filter <- l$pattern_filter %||% switch(pattern, magick = "box", "lanczos")
    l$pattern_gravity <- l$pattern_gravity %||% "center"
    l$pattern_key_scale_factor <- l$pattern_key_scale_factor %||% 1
    l$pattern_orientation <- l$pattern_orientation %||% "vertical"
    l$pattern_shape <- l$pattern_shape %||% 1
    l$pattern_spacing <- l$pattern_spacing %||% 0.05
    l$pattern_type <- l$pattern_type %||%
        switch(pattern,
               ambient = "simplex",
               placeholder = "kitten",
               magick = "hexagons",
               "fit")
    l$pattern_xoffset <- l$pattern_xoffset %||% 0
    l$pattern_yoffset <- l$pattern_yoffset %||% 0

    # Additional ambient defaults
    l$pattern_frequency <- l$pattern_frequency %||% 0.01 # all
    l$pattern_interpolator <- l$pattern_interpolator %||% "quintic" # perlin, simplex, value
    l$pattern_fractal <- l$pattern_fractal %||% "fbm" # all but white
    l$pattern_pertubation <- l$pattern_pertubation %||% "none" # all
    l$pattern_octaves <- l$pattern_octaves %||% 3 # all but white
    l$pattern_lacunarity <- l$pattern_lacunarity %||% 2 # all but white
    l$pattern_gain <- l$pattern_gain %||% 0.5 # all but white
    l$pattern_amplitude <- l$pattern_amplitude %||% 1 # all
    l$pattern_seed <- l$pattern_seed %||% NA_integer_

    l
}
