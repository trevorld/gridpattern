test_that("alpha works as expected", {
    expect_equal(alpha("blue", 0.5), "#0000FF80")
    expect_equal(alpha("blue", NA), "#0000FFFF")
    expect_equal(alpha("#00FF0080", NA), "#00FF0080")
    expect_equal(alpha("#FF000080", 0.25), "#FF000040")
})

test_that("get_params works as expected", {
    params <- get_params()
    expect_equal(params$pattern_colour, "grey20")
    expect_equal(params$pattern_fill, "grey80")
    expect_equal(params$pattern_angle, 30)
    expect_equal(params$pattern_density, 0.2)
    expect_equal(params$pattern_spacing, 0.05)
    expect_equal(params$pattern_xoffset, 0)
    expect_equal(params$pattern_yoffset, 0)
    expect_equal(params$pattern_alpha, 1)
    expect_equal(params$pattern_linetype, 1)
    expect_equal(params$pattern_size, 1)

    params <- get_params(alpha = 0.5, spacing = 0.1)
    expect_equal(params$pattern_alpha, 0.5)
    expect_equal(params$pattern_spacing, 0.1)

    gp <- gpar(col = "blue", lty = 2)
    params <- get_params(gp = gp)
    expect_equal(params$pattern_colour, "blue")
    expect_equal(params$pattern_linetype, 2)
})
