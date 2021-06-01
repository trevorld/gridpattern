sf_multipolygon_to_polygon_grob <- function(multipolygons_sf, gp = gpar(), default.units = "npc") {
    df <- convert_polygon_sf_to_polygon_df(multipolygons_sf)
    if (is.null(df))
        nullGrob()
    else
        polygonGrob(x = df$x, y = df$y, id = df$id, default.units = default.units, gp = gp)
}

# build a circle of radius `r` centered on each point in `sf_points`
sf_points_to_circle_grob <- function(sf_points, r, gp = gpar(), default.units = "npc") {
    points_mat <- as.matrix(sf_points)
    if (is.null(points_mat) || nrow(points_mat) == 0) {
        nullGrob()
    } else {
        circleGrob(x = points_mat[, 1], y = points_mat[, 2], r = r,
                   default.units = default.units, gp = gp)
    }
}

# `xy_polygon` has `x` and `y` elements which will be added to each point in `sf_points`
sf_points_to_polygon_grob <- function(sf_points, xy_polygon, gp = gpar(), default.units = "npc") {
    points_mat <- as.matrix(sf_points)
    df_polygon <- as.data.frame(xy_polygon)
    l_xy <- lapply(seq(nrow(points_mat)),
                   function(i_r) {
                       x0 <- points_mat[i_r, 1]
                       y0 <- points_mat[i_r, 2]
                       df <- df_polygon
                       df$x <- df$x + x0
                       df$y <- df$y + y0
                       df
                   })
    df <- do.call(rbind, l_xy)
    if (is.null(df)) {
        nullGrob()
    } else {
        df$id <- rep(seq(nrow(points_mat)), each = nrow(df_polygon))
        polygonGrob(x = df$x, y = df$y, id = df$id,
                    default.units = default.units, gp = gp)
    }
}

# `xy_polygon` has `x` and `y` elements which will be added to each point in `sf_points`
sf_points_to_sf_multipolygon <- function(sf_points, xy_polygon) {
    points_mat <- as.matrix(sf_points)
    df_polygon <- as.data.frame(xy_polygon)
    df_polygon <- rbind(df_polygon, df_polygon[1L, ])
    l_xy <- lapply(seq(nrow(points_mat)),
                   function(i_r) {
                       x0 <- points_mat[i_r, 1]
                       y0 <- points_mat[i_r, 2]
                       df <- df_polygon
                       df$x <- df$x + x0
                       df$y <- df$y + y0
                       list(as.matrix(df))
                   })
    sf::st_multipolygon(l_xy)
}

