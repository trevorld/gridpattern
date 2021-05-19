test_raster <- function(ref_png, fn, update = FALSE) {
    f <- file.path("../figs/array", ref_png)
    if (update) my_png(f, fn)
    ref <- magick::image_read(f)

    tmpfile <- tempfile(fileext = ".png")
    my_png(tmpfile, fn)
    image <- magick::image_read(tmpfile)
    unlink(tmpfile)

    diff <- magick::image_compare(image, ref, "AE")
    expect_true(attr(diff, "distortion") < 0.01)
}

my_png <- function(f, fn) {
    current_dev <- grDevices::dev.cur()
    grDevices::png(f, type = "cairo", width = 240, height = 240)
    val <- fn()
    grDevices::dev.off()
    if (current_dev > 1) grDevices::dev.set(current_dev)
    invisible(val)
}

test_that("array patterns works as expected", {
    skip_on_cran()
    skip_if_not_installed("magick")
    skip_if_not(capabilities("cairo"))
    test_raster("gradient.png",
                function() grid.pattern_gradient(fill="blue", fill2="green", orientation="radial"))
    test_raster("gradient_horizontal.png",
                function() grid.pattern_gradient(fill="blue", fill2="green", orientation="horizontal"))
    logo_filename   <- system.file("img", "Rlogo.png" , package="png")
    test_raster("image.png", function() {
                    grid.pattern_image(filename=logo_filename, type="fit")
                })
    test_raster("image_expand.png", function() {
                    grid.pattern_image(filename=logo_filename, type="expand")
                })
    test_raster("image_tile.png", function() {
                    grid.pattern_image(filename=logo_filename, type="tile", scale=-2)
                })
    test_raster("image_none.png", function() {
                    grid.pattern_image(filename=logo_filename, type="none", scale=-1)
                })
    test_raster("image_squish.png", function() {
                    grid.pattern_image(filename=logo_filename, type="squish")
                })
    test_raster("magick.png",
                function() grid.pattern_magick(type="octagons", fill="blue", scale=2))
    test_raster("placeholder.png",
                function() grid.pattern_placeholder(type="bear"))

    # plasma images are random and doesn't seem to be a way to set a seed
    tmpfile <- tempfile(fileext = ".png")
    grob <- my_png(tmpfile, function() grid.pattern_plasma(fill="green"))
    unlink(tmpfile)
    expect_true(inherits(grob, "pattern"))

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

    # ambient
    skip_if_not_installed("ambient")
    test_raster("ambient.png", function() grid.pattern_ambient(fill = "green", fill2 = "blue", seed = 42))
})
