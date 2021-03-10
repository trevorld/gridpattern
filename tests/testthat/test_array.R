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

    create_pattern_simple <- function(width, height, params, legend) {
      choice <- params$pattern_type
      if (is.null(choice) || is.na(choice) || !is.character(choice)) {
        choice <- 'a'
      }
      values <- switch(
        choice,
        a = rep(c(0, 1, 0, 1, 1, 0, 0, 1, 1, 1), each = 3),
        b = rep(c(1, 0, 0, 1, 0.5, 0.5, 1, 1, 0, 0, 0, 0, 0, 0.5), each = 7),
        c = rep(seq(0, 1, 0.05), each = 7),
        rep(c(0, 1, 0, 1, 1, 0, 0, 1, 1, 1), each = 3)
      )
      simple_array <- array(values, dim = c(height, width, 4))

      simple_array
    }
    options(ggpattern_array_funcs = list(simple = create_pattern_simple))
    test_raster("simple.png", function() grid.pattern("simple", type = "b"))
})
