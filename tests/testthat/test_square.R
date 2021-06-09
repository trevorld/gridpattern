test_that("square patterns work as expected", {

    pss <- function(...) print.pattern_square(pattern_square(...))

    expect_error(pattern_square("foobar"), "Don't recognize square pattern type foobar")

    verify_output("../text_diagrams/diagonal.txt", pss("diagonal", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/diagonal_skew.txt", pss("diagonal_skew", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/horizontal.txt", pss("horizontal", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_twill.txt", pss("twill", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/vertical.txt", pss("vertical", nrow = 7, ncol = 9))
})

test_that("hex patterns work as expected", {
    phh <- function(...) print.pattern_hex(pattern_hex(...))

    verify_output("../text_diagrams/hex.txt", phh("hex", 3L, nrow = 7, ncol = 9))
})
