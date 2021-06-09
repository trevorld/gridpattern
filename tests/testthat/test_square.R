test_that("square patterns work as expected", {

    pss <- function(...) print.pattern_square(pattern_square(...))

    expect_error(pattern_square("foobar"), "Don't recognize square pattern type foobar")

    verify_output("../text_diagrams/diagonal.txt", pss("diagonal", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/diagonal_1.txt", pss("diagonal", 1L, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/diagonal_skew.txt", pss("diagonal_skew", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/horizontal.txt", pss("horizontal", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_twill.txt", pss("twill", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/vertical.txt", pss("vertical", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_1122.txt", pss("square_tiling", "1122", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_3.txt", pss("square", NULL, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_tiling_3.txt", pss("square_tiling", NULL, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_4.txt", pss("square", 4L, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/square_5.txt", pss("square", 5L, nrow = 7, ncol = 9))
})
