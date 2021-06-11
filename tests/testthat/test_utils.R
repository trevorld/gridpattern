test_that("alpha works as expected", {
    expect_equal(alpha("blue", 0.5), "#0000FF80")
    expect_equal(alpha("blue", NA), "#0000FFFF")
    expect_equal(alpha("#00FF0080", NA), "#00FF0080")
    expect_equal(alpha("#FF000080", 0.25), "#FF000040")
})

test_that("mean_col() works as expected", {
    expect_equal(mean_col("black", "white"), "#B4B4B4FF")
    expect_equal(mean_col(c("black", "white")), "#B4B4B4FF")
    expect_equal(mean_col("red", "blue"), "#B400B4FF")
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
    expect_equal(params$pattern_alpha, NA_real_)
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

test_that("star_scale() works as expected", {
   # |8/3| star has internal angle 45 degrees and external angle 90 degrees
   scale <- star_scale(8, 45)
   scale2 <- star_scale(8, 90, external = TRUE)
   expect_equal(scale, scale2)
   expect_equal(star_angle(8, scale), 45)
   expect_equal(star_angle(8, scale, external = TRUE), 90)
})
