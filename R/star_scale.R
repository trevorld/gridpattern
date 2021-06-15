#' Compute regular star polygon scale or angles
#'
#' `star_scale()` computes star `scale` value given
#' an internal or external angle.  `star_angle()` computes
#' star angle (internal or external) given a `scale` value.
#'
#' [grid.pattern_regular_polygon()] parameterizes regular star polygons
#' with the number of its external vertices and a `scale` that equals the
#' fraction of the radius of the circle that circumscribes the interior vertices
#' divided by the radius of the circle that circumscribes the exterior vertices.
#' These helper functions help convert between that parameterization
#' and either the internal or external angle of the regular star polygon.
#' @param n_vertices Number of exterior vertices.
#' @param angle Angle in degrees.
#' @param scale Scale from 0 to 1.
#' @param external If `TRUE` angle should be considered an external angle.
#' @return `star_scale()` returns a numeric value between 0 and 1 intended
#'         for use as the `scale` argument in [grid.pattern_regular_polygon()].
#'         `star_angle()` returns a numeric value between 0 and 360 (degrees).
#' @examples
#'   # |8/3| star has internal angle 45 degrees and external angle 90 degrees
#'   scale <- star_scale(8, 45)
#'   scale2 <- star_scale(8, 90, external = TRUE)
#'   all.equal(scale, scale2)
#'   star_angle(8, scale)
#'   star_angle(8, scale, external = TRUE)
#'
#'   if (require("grid")) {
#'     grid.pattern_regular_polygon(shape = "star8", scale = scale, angle = 0,
#'                                  spacing = 0.2, density = 0.8)
#'   }
#'
#' @export
star_scale <- function(n_vertices, angle, external = FALSE) {
    if (external)
        angle <- external_to_internal(n_vertices, angle)
    stopifnot(angle >= 0, angle <= 180 * (1 - 2/n_vertices))
    # we'll work with external degree
    angle <- internal_to_external(n_vertices, angle)
    t <- 360 / n_vertices
    xy1 <- list(x = 1, y = 0)
    xy2 <- list(x = to_x(t, 1), y = to_y(t, 1))
    xyc <- list(x = mean(c(xy1$x, xy2$x)), y = mean(c(xy1$y, xy2$y)))
    xyf <- list(x = to_x(t/2, 1), y = to_y(t/2, 1))
    dist_f <- dist(xyf, xyc)
    a2 <- dist(xy1, xyc)
    beta <- (180 - angle) / 2
    b <- a2 * sin(to_radians(beta)) / sin(to_radians(angle/2))
    r <- 1 - b - dist_f
    stopifnot(r >= 0)
    r
}

#' @rdname star_scale
#' @export
star_angle <- function(n_vertices, scale, external = FALSE) {
    stopifnot(scale >= 0, scale <= 1)
    t <- 360 / n_vertices
    xy1 <- list(x = 1, y = 0)
    xy2 <- list(x = to_x(t, 1), y = to_y(t, 1))
    xyv <- list(x = to_x(t/2, scale), y = to_y(t/2, scale))
    xyc <- list(x = mean(c(xy1$x, xy2$x)), y = mean(c(xy1$y, xy2$y)))
    a2 <- dist(xy1, xyc)
    c <- dist(xyv, xy1)
    d <- to_degrees(2 * asin(a2 / c))
    if (!external)
        d <- external_to_internal(n_vertices, d)
    d
}

external_to_internal <- function(n_vertices, external) {
    n <- 2 * n_vertices # exterior plus interior vertices
    total <- (n - 2) * 180
    inverse <- 360 - external
    internal <- (total - n_vertices * inverse) / n_vertices
    internal
}

internal_to_external <- function(n_vertices, internal) {
    n <- 2 * n_vertices # exterior plus interior vertices
    total <- (n - 2) * 180
    inverse <- (total - n_vertices * internal) / n_vertices
    external <- 360 - inverse
    external
}

dist <- function(p1, p2) sqrt((p2$x - p1$x)^2 + (p2$y - p1$y)^2)
