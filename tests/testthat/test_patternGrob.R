test_that("geometry patterns works as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    expect_doppelganger("default", grid.pattern)

    expect_doppelganger("none", function() grid.pattern("none"))

    expect_doppelganger("crosshatch", function()
        grid.pattern("crosshatch", colour="blue", fill="yellow", density = 0.5, angle = 135))

    expect_doppelganger("circle", function()
        grid.pattern("circle", colour="blue", fill="yellow", size = 2, density = 0.5))

    expect_doppelganger("stripe", function()
        grid.pattern("stripe", colour="blue", fill="yellow", density = 0.5, angle = 135))

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
    expect_doppelganger("centroid", function()
        grid.pattern("centroid", fill="blue", size = 5))
})
