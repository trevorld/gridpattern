test_that("stripe pattern works as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    expect_doppelganger("default", grid.pattern)
    expect_doppelganger("stripe", function()
        grid.pattern("stripe", colour="blue", fill="yellow", density = 0.5, angle = 135))
})
