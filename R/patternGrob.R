#' Patterned grobs
#'
#' \code{grid.pattern} draws patterned shapes onto the graphic device.
#' \code{patternGrob} returns the grid grob objects.
#'
#' Here are links to more information about the various patterns:
#'
#' \describe{
#' \item{circle}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-circle.html}}
#' \item{crosshatch}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html}}
#' \item{gradient}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-gradient.html}}
#' \item{image}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-image.html}}
#' \item{magick}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-magick.html}}
#' \item{none}{Does nothing}
#' \item{placeholder}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-placeholder.html}}
#' \item{plasma}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-plasma.html}}
#' \item{stripe}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html}}
#' \item{Custom geometry-based patterns}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html}}
#' \item{Custom array-based patterns}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html}}
#' }
#'
#' @inheritParams grid::polygonGrob
#' @import grid
#' @importFrom utils hasName
#' @param pattern Name of pattern
#' @param x A numeric vector or unit object specifying x-locations of the pattern boundary
#' @param y A numeric vector or unit object specifying y-locations of the pattern boundary
#' @param ... Pattern parameters
#' @param id A numeric vector used to separate locations in x, y into multiple boundaries.
#'           All locations within the same \code{id} belong to the same boundary.
#' @param default.units A string indicating the default units to use if \code{x} or \code{y}
#'                      are only given as numeric vectors.
#' @param prefix Prefix to prepend to the name of each of the pattern parameters in \code{...}
#' @param legend Whether this is intended to be drawn in a legend or not
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
#'    \dontrun{
#'      grid.newpage()
#'      logo_filename   <- system.file("img", "Rlogo.png" , package="png")
#'      grid.pattern("image", filename=logo_filename, type="tile")
#'      grid.newpage()
#'      grid.pattern("magick", type="octagons", fill="blue", scale=2)
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
grid.pattern <- function(pattern = "stripe", x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), ...,
                         id = 1L, default.units = "npc", prefix = "pattern_", legend = FALSE,
                         name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grob <- patternGrob(pattern, x, y, ..., prefix = prefix, legend = legend, name = name, gp = gp, vp = vp)
    if (draw) {
        grid.draw(grob)
        invisible(grob)
    } else {
        grob
    }
}

#' @rdname grid.pattern
#' @export
patternGrob <- function(pattern = "strip", x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), ...,
                        id = 1L, default.units = "npc", prefix = "pattern_", legend = FALSE,
                        name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    params <- get_params(..., pattern = pattern, prefix = prefix, gp = gp)
    if (!inherits(x, "unit")) x <- unit(x, default.units)
    if (!inherits(y, "unit")) y <- unit(y, default.units)

    gTree(pattern=pattern, x=x, y=y, id=id, params=params, legend=legend,
          name=name, gp=gp, vp=vp, cl="pattern")
}

#' @export
makeContext.pattern <- function(x) {
    if (hasName(x$gp, "alpha")) x$gp$alpha <- NULL # avoid applying an alpha effect twice
    x
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

    fn <- get_fn(x$pattern)
    grob <- fn(x$params, boundary_df, aspect_ratio, x$legend)
    gl <- gList(grob)
    setChildren(x, gl)
}

get_fn <- function(pattern) {
    geometry_fns <- c(getOption("ggpattern_geometry_funcs"),
                      list(circle = create_pattern_circles,
                           crosshatch = create_pattern_crosshatch_via_sf,
                           none = create_pattern_none,
                           stripe = create_pattern_stripes_via_sf))
    array_fns <- c(getOption("ggpattern_array_funcs"),
                   list(gradient = create_gradient_as_array,
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
    l$pattern_type <- l$pattern_type %||% switch(pattern, placeholder = "kitten", "fit")
    l$pattern_xoffset <- l$pattern_xoffset %||% 0
    l$pattern_yoffset <- l$pattern_yoffset %||% 0

    l
}
