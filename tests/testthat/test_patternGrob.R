test_that("stripe pattern works as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    expect_doppelganger("default", grid.pattern)
    expect_doppelganger("stripe", function()
        grid.pattern("stripe", colour="blue", fill="yellow", density = 0.5, angle = 135))
})
test_that("circle pattern works as expected", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    expect_doppelganger("circle", function()
        grid.pattern("circle", colour="blue", fill="yellow", size = 2, density = 0.5))
})
