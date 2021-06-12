context("tiling")
test_that("tiling patterns work as expected", {

    skip_if_not_installed("vdiffr")
    skip_on_appveyor()
    library("vdiffr")

    x <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    y <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
    g.ppt <- function(type, ...) {
        grid.pattern_polygon_tiling(x, y, angle = 0, type = type,
                                    spacing = 0.15, ...)
    }
    gp1 <- gpar(fill = "yellow", col = "black")
    gp2 <- gpar(fill = c("yellow", "red"), col = "black")
    gp3 <- gpar(fill = c("yellow", "red", "blue"), col = "black")

    expect_doppelganger("herringbone", function()
        g.ppt("herringbone", gp = gp2))
    expect_doppelganger("hexagonal_tiling", function()
        g.ppt("hexagonal", gp = gp2))
    expect_doppelganger("pythagorean", function()
        g.ppt("pythagorean", gp = gp2))
    expect_doppelganger("rhombitrihexagonal_tiling", function()
        g.ppt("rhombitrihexagonal", gp = gp3))
    expect_doppelganger("snub_square_tiling", function()
        g.ppt("snub_square", gp = gp3))
    expect_doppelganger("square_tiling", function()
        g.ppt("square", gp = gp2))
    expect_doppelganger("triangular_tiling", function()
        g.ppt("triangular", gp = gp2))
    expect_doppelganger("trihexagonal_tiling", function()
        g.ppt("trihexagonal", gp = gp2))
    expect_doppelganger("trunc_hex_tiling", function()
        g.ppt("truncated_hexagonal", gp = gp3))
    expect_doppelganger("trunc_trihex_tiling", function()
        g.ppt("truncated_trihexagonal", gp = gp2))
    expect_doppelganger("trunc_square_tiling", function()
        g.ppt("truncated_square", gp = gp3))

})
