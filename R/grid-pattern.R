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
#' @param pattern Name of pattern.  See Details section for a list of supported patterns.
#' @param x A numeric vector or unit object specifying x-locations of the pattern boundary.
#' @param y A numeric vector or unit object specifying y-locations of the pattern boundary.
#' @param id A numeric vector used to separate locations in x, y into multiple boundaries.
#'           All locations within the same `id` belong to the same boundary.
#' @param ... Pattern parameters.
#' @param legend Whether this is intended to be drawn in a legend or not.
#' @param prefix Prefix to prepend to the name of each of the pattern parameters in `...`.
#'               For compatibility with `ggpattern` most underlying functions assume parameters beginning with `pattern_`.
#' @param default.units A string indicating the default units to use if `x` or `y`
#'                      are only given as numeric vectors.
#' @return A grid grob object (invisibly in the case of `grid.pattern()`).
#'         If `draw` is `TRUE` then `grid.pattern()` also draws to the graphic device as a side effect.
#' @examples
#'  if (require("grid")) {
#'    x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    # geometry-based patterns
#'    grid.newpage()
#'    grid.pattern()
#'    grid.newpage()
#'    grid.pattern("stripe", x_hex, y_hex,
#'                 colour="black", fill=c("yellow", "blue"), density = 0.5)
#'    grid.newpage()
#'    # In some cases can also alternatively use "gpar()" to specify colour and line attributes
#'    x <- c(0.1, 0.6, 0.8, 0.3)
#'    y <- c(0.2, 0.3, 0.8, 0.5)
#'    grid.pattern("stripe", x, y, gp = gpar(col="blue", fill="red", lwd=2))
#'    grid.newpage()
#'    grid.pattern("crosshatch", x_hex, y_hex,
#'                 colour="black", fill="yellow", fill2="blue", angle=45)
#'    grid.newpage()
#'    grid.pattern("circle", x_hex, y_hex,
#'                 colour="blue", fill="yellow", size = 2, density = 0.5)
#'
#'    # array-based patterns
#'    if (requireNamespace("ambient")) {
#'      grid::grid.newpage()
#'      grid.pattern("ambient", x_hex, y_hex, fill = "green", fill2 = "blue")
#'    }
#'    grid.newpage()
#'    grid.pattern("gradient", x_hex, y_hex,
#'                 fill="blue", fill2="green", orientation="radial")
#'    logo_filename <- system.file("img", "Rlogo.png" , package="png")
#'    grid.newpage()
#'    grid.pattern("image", x_hex, y_hex, filename=logo_filename, type="fit")
#'    grid.newpage()
#'    grid.pattern("magick", x_hex, y_hex, type="octagons", fill="blue", scale=2)
#'    \dontrun{
#'      # requires internet connection to download from placeholder image websites
#'      grid.newpage()
#'      grid.pattern("placeholder", x_hex, y_hex, type="bear")
#'    }
#'    grid.newpage()
#'    grid.pattern("plasma", x_hex, y_hex, fill="green")
#'  }
#' @seealso \url{https://coolbutuseless.github.io/package/ggpattern/index.html}
#'          for more details on the patterns and their parameters.
#' @export
grid.pattern <- function(pattern = "stripe",
                         x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
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
patternGrob <- function(pattern = "stripe",
                        x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
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
    geometry_fns <- c(list(circle = create_pattern_circle_via_sf,
                           crosshatch = create_pattern_crosshatch_via_sf,
                           none = create_pattern_none,
                           regular_polygon = create_pattern_regular_polygon_via_sf,
                           stripe = create_pattern_stripes_via_sf),
                      getOption("ggpattern_geometry_funcs"))
    array_fns <- c(list(ambient = create_pattern_ambient,
                        gradient = create_gradient_as_array,
                        image = img_read_as_array_wrapper,
                        magick = create_magick_pattern_as_array,
                        placeholder = fetch_placeholder_array,
                        plasma = create_magick_plasma_as_array),
                   getOption("ggpattern_array_funcs"))
    array_fns <- lapply(array_fns, function(fn) {
                            function(...) create_pattern_array(..., array_fn=fn)
                   })
    fns <- c(geometry_fns, array_fns)
    fns[[pattern]] %||% abort(glue("Don't know the function for pattern {pattern}"))
}
