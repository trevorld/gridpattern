#' Create patterned grobs
#'
#' `grid.pattern()` draws patterned shapes onto the graphic device.
#' `patternGrob()` returns the grid grob objects.
#' `names_pattern` is a character vector of builtin patterns.
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
#' \item{gradient}{Gradient array/geometry patterns.
#'                 See [grid.pattern_gradient()] for more information.}
#' \item{image}{Image array patterns.
#'              See [grid.pattern_image()] for more information.}
#' \item{magick}{`imagemagick` array patterns.
#'               See [grid.pattern_magick()] for more information.}
#' \item{none}{Does nothing.
#'             See [grid::grid.null()] for more information.}
#' \item{pch}{Plotting character geometry patterns.
#'            See [grid.pattern_pch()] for more information.}
#' \item{placeholder}{Placeholder image array patterns.
#'                    See [grid.pattern_placeholder()] for more information.}
#' \item{plasma}{Plasma array patterns.
#'               See [grid.pattern_plasma()] for more information.}
#' \item{polygon_tiling}{Polygon tiling patterns.
#'                        See [grid.pattern_polygon_tiling()] for more information.}
#' \item{regular_polygon}{Regular polygon patterns.
#'                        See [grid.pattern_regular_polygon()] for more information.}
#' \item{stripe}{Stripe geometry patterns.
#'               See [grid.pattern_stripe()] for more information.}
#' \item{text}{Text array/geometry patterns.
#'             See [grid.pattern_text()] for more information.}
#' \item{wave}{Wave geometry patterns.
#'               See [grid.pattern_wave()] for more information.}
#' \item{weave}{Weave geometry patterns.
#'               See [grid.pattern_weave()] for more information.}
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
#'  print(names_pattern)
#'  if (require("grid")) {
#'    x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'    y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'
#'    # geometry-based patterns
#'    # 'stripe' pattern
#'    grid.newpage()
#'    grid.pattern("stripe", x_hex, y_hex,
#'                 colour="black", fill=c("yellow", "blue"), density = 0.5)
#'
#'    # Can alternatively use "gpar()" to specify colour and line attributes
#'    grid.newpage()
#'    grid.pattern("stripe", x_hex, y_hex, gp = gpar(col="blue", fill="red", lwd=2))
#'
#'    # 'weave' pattern
#'    grid.newpage()
#'    grid.pattern("weave", x_hex, y_hex, type = "satin",
#'                 colour = "black", fill = "lightblue", fill2 =  "yellow",
#'                 density = 0.3)
#'
#'    # 'regular_polygon' pattern
#'    grid.newpage()
#'    grid.pattern_regular_polygon(x_hex, y_hex, colour = "black",
#'                                 fill = c("blue", "yellow", "red"),
#'                                 shape = c("convex4", "star8", "circle"),
#'                                 density = c(0.45, 0.42, 0.4),
#'                                 spacing = 0.08, angle = 0)
#'
#'    # can be used to achieve a variety of 'tiling' effects
#'    grid.newpage()
#'    grid.pattern_regular_polygon(x_hex, y_hex, color = "transparent",
#'                                 fill = c("white", "grey", "black"),
#'                                 density = 1.0, spacing = 0.1,
#'                                 shape = "convex6", type = "hex")
#'
#'    # array-based patterns
#'    # 'image' pattern
#'    logo_filename <- system.file("img", "Rlogo.png" , package="png")
#'    grid.newpage()
#'    grid.pattern("image", x_hex, y_hex, filename=logo_filename, type="fit")
#'
#'    # 'plasma' pattern
#'    grid.newpage()
#'    grid.pattern("plasma", x_hex, y_hex, fill="green")
#'  }
#' @seealso \url{https://coolbutuseless.github.io/package/ggpattern/index.html}
#'          for more details on the `ggpattern` patterns and their parameters.
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
names_pattern <- c("ambient", "circle", "crosshatch", "gradient", "image",
                   "magick", "none", "pch", "placeholder", "plasma", "polygon_tiling",
                   "regular_polygon", "rose", "stripe", "text", "wave", "weave")

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

    xp <- convertX(x$x, "npc", valueOnly = TRUE)
    yp <- convertY(x$y, "npc", valueOnly = TRUE)
    id <- x$id
    boundary_df <- create_polygon_df(xp, yp, id)

    if (!is.na(x$params$pattern_aspect_ratio)) {
        aspect_ratio <- x$params$pattern_aspect_ratio
    } else {
        width <- convertWidth(unit(1, "npc"), "in", valueOnly = TRUE)
        height <- convertHeight(unit(1, "npc"), "in", valueOnly = TRUE)
        aspect_ratio <-  width / height
    }

    fn <- get_pattern_fn(x$pattern)
    grob <- fn(x$params, boundary_df, aspect_ratio, x$legend)
    gl <- gList(grob)
    setChildren(x, gl)
}

get_pattern_fn <- function(pattern) {
    user_geometry_fns <- getOption("ggpattern_geometry_funcs")
    user_array_fns <- getOption("ggpattern_array_funcs")
    assert_patterns_unique(user_geometry_fns, user_array_fns)
    geometry_fns <- c(list(circle = create_pattern_circle_via_sf,
                           crosshatch = create_pattern_crosshatch_via_sf,
                           gradient = create_pattern_gradient,
                           none = create_pattern_none,
                           pch = create_pattern_pch,
                           polygon_tiling = create_pattern_polygon_tiling,
                           regular_polygon = create_pattern_regular_polygon_via_sf,
                           rose = create_pattern_rose,
                           stripe = create_pattern_stripes_via_sf,
                           text = create_pattern_text,
                           wave = create_pattern_wave_via_sf,
                           weave = create_pattern_weave_via_sf),
                      user_geometry_fns)
    array_fns <- c(list(ambient = create_pattern_ambient,
                        image = img_read_as_array_wrapper,
                        magick = create_magick_pattern_as_array,
                        placeholder = fetch_placeholder_array,
                        plasma = create_magick_plasma_as_array),
                   user_array_fns)
    array_fns <- lapply(array_fns, function(fn) {
                            function(...) create_pattern_array(..., array_fn=fn)
                   })
    fns <- c(geometry_fns, array_fns)
    fns[[pattern]] %||% abort(paste("Don't know the function for pattern", pattern))
}

assert_patterns_unique <- function(user_geometry_fns, user_array_fns) {
    names_geometry <- names(user_geometry_fns)
    names_array <- names(user_array_fns)
    msg_geometry <- '`options("ggpattern_geometry_funcs")` sets custom "geometry" patterns'
    msg_array <- '`options("ggpattern_array_funcs")` sets custom "array" patterns'
    # check pattern names not duplicated within custom pattern types
    duplicated_geometry <- duplicated(names_geometry)
    if (any(duplicated_geometry)) {
        name <- names_geometry[which(duplicated_geometry)[1]]
        msg <- c(glue('There are multiple custom "geometry" patterns named "{name}"'),
                 i = msg_geometry)
        abort(msg)
    }
    duplicated_array <- duplicated(names_array)
    if (any(duplicated_array)) {
        name <- names_array[which(duplicated_array)[1]]
        msg <- c(glue('There are multiple custom "array" patterns named "{name}"'),
                 i = msg_array)
        abort(msg)
    }
    # check pattern names not duplicated between custom pattern types
    match_user <- match(names_geometry, names_array)
    if (any(!is.na(match_user))) {
        index <- which(!is.na(match_user))[1]
        name <- names_geometry[index]
        msg <- c(glue('There is a custom "geometry" pattern and custom "array" pattern both named "{name}"'),
                 i = msg_geometry,
                 i = msg_array)
        abort(msg)
    }
    # check pattern names not duplicated between custom patterns and builtin patterns
    match_geometry <- match(names_geometry, names_pattern)
    if (any(!is.na(match_geometry))) {
        index <- which(!is.na(match_geometry))[1]
        name <- names_geometry[index]
        msg <- c(glue('There is a custom "geometry" pattern and builtin {{gridpattern}} pattern both named "{name}"'),
                 i = msg_geometry)
        abort(msg)
    }
    match_array <- match(names_array, names_pattern)
    if (any(!is.na(match_array))) {
        index <- which(!is.na(match_array))[1]
        name <- names_array[index]
        msg <- c(glue('There is a custom "array" pattern and builtin {{gridpattern}} pattern both named "{name}"'),
                 i = msg_array)
        abort(msg)
    }
    invisible(NULL)
}
