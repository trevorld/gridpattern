test_that("weaves work as expected", {

    pww <- function(...) print.pattern_weave(pattern_weave(...))

    # irregular mat
    verify_output("../text_diagrams/plain.txt", pww("plain", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/basket.txt", pww("basket", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/matt.txt", pww("matt", 3, nrow = 7, ncol = 9))
    verify_output("../text_diagrams/matt_21.txt", pww("matt", "2/1", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/matt3221.txt", pww("matt", "3/2*2/1", nrow = 15, ncol = 9))
    verify_output("../text_diagrams/matt_irregular.txt",
                  pww("matt_irregular", "3/2(4+2)", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/rib_warp",
                  pww("rib_warp", "2/1", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/rib_warp.txt", pww("rib_warp", "2", nrow = 7, ncol = 9))

    # elongated twill
    verify_output("../text_diagrams/satin_5.txt", pww("satin", "5", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/twill_212.txt", pww("twill_elongated", "2/1(2)", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/twill_22.txt", pww("twill", "2/2", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/twill_13.txt", pww("twill", "1/3", nrow = 7, ncol = 9))
    verify_output("../text_diagrams/twill_22_zigzag.txt",
                  pww("twill_zigzag", "2/2", nrow = 15, ncol = 9))
    verify_output("../text_diagrams/twill_13_herringbone.txt",
                  pww("twill_herringbone", "1/3", nrow = 15, ncol = 9))
    verify_output("../text_diagrams/twill3221.txt", pww("twill", "3/2*2/1", nrow = 15, ncol = 9))
})
