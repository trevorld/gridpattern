test_that("aRtsy patterns works as expected", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("aRtsy")
    skip_if_not(getRversion() >= "4.3.0")
    skip_if_not(isTRUE(all(capabilities(c("cairo", "png")))))

    f <- tempfile(fileext = ".png")
    png(f, type = "cairo")
    grid.pattern_aRtsy(type = "maze")
    dev.off()
    expect_true(file.size(f) > 0)
    unlink(f)

    f <- tempfile(fileext = ".png")
    png(f, type = "cairo")
    grid.pattern_aRtsy(type = "strokes")
    dev.off()
    expect_true(file.size(f) > 0)
    unlink(f)

    expect_true(length(names_aRtsy()) >= 34L)
})
