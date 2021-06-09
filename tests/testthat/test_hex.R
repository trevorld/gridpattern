test_that("hex patterns work as expected", {
    phh <- function(...) print.pattern_hex(pattern_hex(...))

    verify_output("../text_diagrams/hex.txt", phh("hex_skew", 3L, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/hex1_1.txt", phh("hex", 1L, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/hex1_2.txt", phh("hex", NULL, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/hex2_2.txt", phh("hex2", 2L, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/hex2_4.txt", phh("hex", 4L, nrow = 9, ncol = 9))
    verify_output("../text_diagrams/hex3_2.txt", phh("hex3", 2L, nrow = 9, ncol = 9))
    verify_output("../text_diagrams/hex3_7.txt", phh("hex", 7L, nrow = 9, ncol = 9))
    verify_output("../text_diagrams/hex_skew_5.txt", phh("hex", 5L, nrow = 9, ncol = 9))
})
