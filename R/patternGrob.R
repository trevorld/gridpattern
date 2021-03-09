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
#' \item{stripe}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html}}
#' \item{Custom geometry-based patterns}{See \url{https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html}}
#' }
#'
#' @inheritParams grid::polygonGrob
#' @import grid
#' @importFrom utils hasName
#' @param pattern Name of pattern
#' @param ... Pattern parameters
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

# fill2
# orientation
# aspect_ratio
# key_scale_factor

# array pattern: function(width, height, params, legend)
# geometry pattern: function(params, boundary_df, aspect_ratio, legend)

#' @rdname grid.pattern
#' @export
patternGrob <- function(pattern = "strip", x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), ...,
                        id = 1L, default.units = "npc", prefix = "pattern_", legend = FALSE,
                        name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    params <- get_params(..., prefix = prefix, gp = gp)
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

    width <- as.numeric(convertWidth(unit(1, "npc"), "in"))
    height <- as.numeric(convertHeight(unit(1, "npc"), "in"))
    aspect_ratio <-  width / height

    pattern <- x$pattern
    geometry_fns <- c(getOption("ggpattern_geometry_funcs", list()),
                      list(circle = create_pattern_circles,
                           crosshatch = create_pattern_crosshatch_via_sf,
                           stripe = create_pattern_stripes_via_sf))

    if (hasName(geometry_fns, pattern)) {
        xp <- as.numeric(convertX(x$x, "npc"))
        yp <- as.numeric(convertY(x$y, "npc"))
        id <- x$id
        boundary_df <- create_polygon_df(xp, yp, id)

        fn <- geometry_fns[[pattern]]
        grob <- fn(x$params, boundary_df, aspect_ratio, x$legend)
    } else {
        stop("Don't know the function for pattern ", pattern)
    }

    gl <- gList(grob)
    setChildren(x, gl)
}


get_params <- function(..., prefix = "pattern_", gp = gpar()) {
    params <- list(...)
    if (length(params)) names(params) <- paste0(prefix, names(params))
    if (!hasName(params, "pattern_colour"))
        params$pattern_colour <- get(gp, "col", "grey20")
    if (!hasName(params, "pattern_fill"))
        params$pattern_fill <- get(gp, "fill", "grey80")
    if (!hasName(params, "pattern_alpha"))
        params$pattern_alpha <- get(gp, "alpha", 1)
    if (!hasName(params, "pattern_linetype"))
        params$pattern_linetype <- get(gp, "lty", 1)
    if (!hasName(params, "pattern_size"))
        params$pattern_size <- get(gp, "lwd", 1)

    if (!hasName(params, "pattern_angle"))
        params$pattern_angle <- 30
    if (!hasName(params, "pattern_density"))
        params$pattern_density <- 0.2
    if (!hasName(params, "pattern_shape"))
        params$pattern_shape <- 1
    if (!hasName(params, "pattern_spacing"))
        params$pattern_spacing <- 0.05
    if (!hasName(params, "pattern_xoffset"))
        params$pattern_xoffset <- 0
    if (!hasName(params, "pattern_yoffset"))
        params$pattern_yoffset <- 0

    params
}
