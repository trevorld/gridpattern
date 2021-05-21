context("geometry")
test_that("geometry patterns works as expected", {
    skip_if_not_installed("vdiffr")
    skip_on_appveyor()
    library("vdiffr")

    expect_doppelganger("default", grid.pattern)

    expect_doppelganger("none", function() grid.pattern("none"))

    x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    expect_doppelganger("crosshatch", function()
        grid.pattern_crosshatch(x, y, color="black", fill="blue", fill2="yellow", density = 0.5))

    expect_doppelganger("circle", function()
        grid.pattern_circle(x, y, color="blue", fill="yellow", size = 2, density = 0.5))

    expect_doppelganger("stripe", function()
        grid.pattern_stripe(x, y, color="black", fill=c("yellow", "blue"), density = 0.5))

    expect_doppelganger("stripe_gpar", function() {
        x <- c(0.1, 0.6, 0.8, 0.3)
        y <- c(0.2, 0.3, 0.8, 0.5)
        grid.pattern("stripe", x, y, gp = gpar(col="blue", fill="red", lwd=2))
    })

    centroid_dot_pattern <- function(params, boundary_df, aspect_ratio, legend) {
        boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)
        centroid    <- sf::st_centroid(boundary_sf)
        grid::pointsGrob(x    = centroid[1],
                         y    = centroid[2],
                         pch  = params$pattern_shape,
                         size = unit(params$pattern_size, 'char'),
                         default.units = "npc",
                         gp   = grid::gpar(col = alpha(params$pattern_fill, params$pattern_alpha))
        )
    }
    options(ggpattern_geometry_funcs = list(centroid = centroid_dot_pattern))
    x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    expect_doppelganger("centroid", function()
        grid.pattern("centroid", x, y, fill="blue", size = 5))
})
