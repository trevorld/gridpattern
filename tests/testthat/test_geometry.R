context("geometry")
test_that("geometry helpers work as expected", {
    xy <- rotate_xy(c(0, 1), c(0, 1), 90)
    expect_equal(xy$x, c(1, 0))
    expect_equal(xy$y, c(0, 1))
})
test_that("geometry patterns work as expected", {

    png_file <- tempfile(fileext = ".png")
    png(png_file)
    expect_error(grid.pattern_crosshatch(x, y, density = 1.1))
    expect_error(grid.pattern_stripe(x, y, density = 1.1))
    dev.off()
    unlink(png_file)

    skip_if_not_installed("vdiffr")
    skip_on_ci()
    library("vdiffr")

    expect_doppelganger("default", grid.pattern)

    x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    expect_doppelganger("circle", function()
        grid.pattern_circle(x, y, color="blue", fill="yellow", size = 2, density = 0.5))

    expect_doppelganger("crosshatch", function()
        grid.pattern_crosshatch(x, y, color="black", fill="blue", fill2="yellow", density = 0.5))

    expect_doppelganger("none", function()
        grid.pattern_none(x, y))

    expect_error(assert_rp_shape(1), "Unknown shape 1")
    expect_null(assert_rp_shape(c("square", "convex4")))
    expect_null(assert_rp_shape(c("star5", "circle", "null")))
    expect_doppelganger("regular_polygon", function()
        grid.pattern_regular_polygon(x, y, color = "black", fill = "blue", density = 0.5))

    expect_doppelganger("hexagon", function()
        grid.pattern_regular_polygon(x, y, color = "transparent", fill = c("white", "grey", "black"),
                                     density = 1.0, shape = "convex6", grid = "hex"))

    expect_doppelganger("square", function()
        grid.pattern_regular_polygon(x, y, color = "black", fill = c("white", "grey"),
                                     density = 1.0, shape = "square"))

    expect_doppelganger("eight_sided_star", function()
        grid.pattern_regular_polygon(x, y, colour = "black", fill = c("blue", "yellow"),
                                     density = 1.0, spacing = 0.1, shape = "star8"))
    expect_doppelganger("stripe", function()
        grid.pattern_stripe(x, y, color="black", fill=c("yellow", "blue"), density = 0.5))

    expect_doppelganger("stripe_gpar", function() {
        x <- c(0.1, 0.6, 0.8, 0.3)
        y <- c(0.2, 0.3, 0.8, 0.5)
        grid.pattern("stripe", x, y, gp = gpar(col="blue", fill="red", lwd=2))
    })

    expect_doppelganger("wave_sine", function()
        grid.pattern_wave(x, y, colour = "black", type = "sine",
                          fill = c("red", "blue"), density = 0.4,
                          spacing = 0.15, angle = 0,
                          amplitude = 0.05, frequency = 1 / 0.20))

    expect_doppelganger("wave_triangle", function()
        grid.pattern_wave(x, y, color="black", fill="yellow",
                           type = "triangle", density = 0.5, spacing = 0.15))

    expect_doppelganger("weave", function()
        grid.pattern_weave(x, y, color="black", fill="yellow", fill2="blue",
                           type = "twill", density = 0.5))

    centroid_dot_pattern <- function(params, boundary_df, aspect_ratio, legend) {
        boundary_sf <- convert_polygon_df_to_polygon_sf(boundary_df)
        centroid    <- sf::st_centroid(boundary_sf)
        grid::pointsGrob(x    = centroid[1],
                         y    = centroid[2],
                         pch  = params$pattern_shape,
                         size = unit(params$pattern_size, 'char'),
                         default.units = "npc",
                         gp   = grid::gpar(col = update_alpha(params$pattern_fill, params$pattern_alpha))
        )
    }
    options(ggpattern_geometry_funcs = list(centroid = centroid_dot_pattern))
    x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    expect_doppelganger("centroid", function()
        grid.pattern("centroid", x, y, fill="blue", size = 5))

    x <- c(0, 0, 0.5, 0.5, 0.5, 0.5, 1, 1)
    y <- c(0, 0.5, 0.5, 0, 0.5, 1, 1, 0.5)
    id <- rep(1:2, each = 4L)
    expect_doppelganger("two_id", function()
        grid.pattern(x = x, y = y, id = id))
})
