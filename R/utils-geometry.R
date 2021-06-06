# radians <-> degrees
to_radians <- function(t) pi * t / 180
to_degrees <- function(t) 180 * t / pi

# polar coordinates <-> cartesian coordinates
to_x <- function(t, r) {
    r * cos(to_radians(t))
}
to_y <- function(t, r) {
    r * sin(to_radians(t))
}
to_theta <- function(x, y) {
    to_degrees(atan2(y, x))
}
to_radius <- function(x, y) {
    sqrt(x^2 + y^2)
}

# rotate (x,y) `t` degrees centered around (x0, y0)
rotate_xy <- function(x, y, theta = 0, x0 = NULL, y0 = NULL) {
    x0 <- x0 %||% mean(x)
    y0 <- y0 %||% mean(y)
    xc <- x - x0
    yc <- y - y0
    theta <- to_theta(xc, yc) + theta
    radius <- to_radius(xc, yc)
    x1 <- to_x(theta, radius) + x0
    y1 <- to_y(theta, radius) + y0
    list(x = x1, y = y1)
}

# (x,y) coordinates of convex regular polygon centered at (0, 0)
convex_xy <- function(n_vertices, theta = 90, radius_outer = 0.5) {
    t <- theta + seq(0, 360, length.out = n_vertices + 1)
    x <- to_x(t, radius_outer)
    y <- to_y(t, radius_outer)
    list(x = head(x, -1),
         y = head(y, -1))
}

# (x,y) coordinates of concave (star) regular polygon centered at (0, 0)
concave_xy <- function(n_vertices, theta = 90, radius_outer = 0.5,
                       radius_inner = 0.5 * radius_outer) {
    t_outer <- theta + seq(0, 360, length.out = n_vertices + 1)
    n_degrees <- 360 / n_vertices / 2
    t_inner <- theta + seq(n_degrees, 360 - n_degrees, length.out = n_vertices)
    x_outer <- to_x(t_outer, radius_outer)
    x_inner <- to_x(t_inner, radius_inner)
    y_outer <- to_y(t_outer, radius_outer)
    y_inner <- to_y(t_inner, radius_inner)
    x <- splice(x_outer, x_inner)
    y <- splice(y_outer, y_inner)
    list(x = head(x, -1),
         y = head(y, -1))
}

splice <- function(x0, x1) {
    vec <- as.numeric()
    for (ii in seq_along(x1)) {
        vec <- append(vec, x0[ii])
        vec <- append(vec, x1[ii])
    }
    append(vec, x0[ii+1])
}

get_n_vertices <- function(shape) {
    as.numeric(gsub("convex|concave|star", "", shape))
}

# returns numeric(0) if 'from' greater than 'to'
seq_robust <- function(from = 1, to = 1, by = ((to - from)/(length.out - 1)), length.out = NULL) {
    if (from > to) {
        numeric(0)
    } else {
        if (is.null(length.out))
            seq(from, to, by)
        else
            seq(from, to, by, length.out)
    }
}

# cycle_elements(1:5, -2) = c(4, 5, 1, 2, 3)
# cycle_elements(1:5, -1) = c(5, 1, 2, 3, 4)
# cycle_elements(1:5,  0) = c(1, 2, 3, 4, 5)
# cycle_elements(1:5,  1) = c(2, 3, 4, 5, 1)
# cycle_elements(1:5,  2) = c(3, 4, 5, 1, 2)
cycle_elements <- function(x, n = 1) {
    l <- length(x)
    if (l < 2 || n == l || n == 0 || n == -l)
        return(x)
    if (n > 0) {
        if (n < l) {
            c(x[(n+1):l], x[1:n])
        } else {
            cycle_elements(x, n-l)
        }
    } else {
        if (-l < n) {
            c(x[(l+n+1):l], x[1:(l+n)])
        } else {
            cycle_elements(x, n+l)
        }
    }
}

nigh <- function(x, y) abs(x - y) < .Machine$double.eps^0.5
