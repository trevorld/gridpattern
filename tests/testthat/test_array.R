test_raster <- function(ref_png, fn, update = FALSE) {
    f <- file.path("../figs/array", ref_png)
    if (update) {
        png(f, type = "cairo")
        fn()
        dev.off()
    }
    ref <- magick::image_read(f)

    tmpfile <- tempfile(fileext = ".png")
    png(tmpfile, type = "cairo")
    fn()
    dev.off()
    image <- magick::image_read(tmpfile)

    diff <- magick::image_compare(image, ref, "AE")
    expect_true(attr(diff, "distortion") < 0.01)
}

test_that("array patterns works as expected", {
    skip_on_cran()
    skip_if_not_installed("magick")
    skip_if_not(capabilities("cairo"))
    test_raster("magick.png", function() grid.pattern("magick", type="octagons", fill="blue", scale=2))
})
