#' Create patterned fills by pattern name
#'
#' `patternFill()` returns [grid::pattern()] fill objects.
#' It is a wrapper around [patternGrob()].
#'
#' @param ... Passed to [patternGrob()].
#' @param x,y,width,height  The size of the [grid::pattern()] tile.
#' @param default.units  The default [grid::unit()] unit to use for `x`, `y`, `width`, and `height`.
#' @param just,hjust,vjust  The justification of the tile relative to its location.
#' @param group A logical indicating whether the pattern is relative to the bounding box of the grob or whether it is relative to individual shapes within the grob.  Ignored if R is less than version 4.2.
#' @examples
#' if (guess_has_R4.1_features("patterns") &&
#'     require("grid", quietly = TRUE)) {
#'   grid.newpage()
#'   stripe_fill <- patternFill("stripe", fill = c("red", "blue"))
#'   grid.circle(gp = gpar(fill = stripe_fill))
#' }
#'  
#' if (guess_has_R4.1_features("patterns") && 
#'     require("ggplot2", quietly = TRUE) &&
#'     (getRversion() >= "4.2")) {
#'   grid.newpage()
#'   weave_fill <- patternFill("weave", fill = "red", fill2 = "blue", 
#'                             colour = "transparent")
#'   hex_fill <- patternFill("polygon_tiling", type = "hexagonal", 
#'                           fill = c("black", "white", "grey"),
#'                           colour = "transparent")
#'   df <- data.frame(trt = c("a", "b"), outcome = c(1.9, 3.2))
#'   gg <- ggplot(df, aes(trt, outcome)) +
#'     geom_col(fill = list(weave_fill, hex_fill))
#'   plot(gg)
#' }
#' @return A [grid::pattern()] fill object.
#' @export
patternFill <- function(..., 
                        x = 0.5, y = 0.5, width = 1, height = 1,
                        default.units = "npc",
                        just = "centre", hjust = NULL, vjust = NULL,
                        group = TRUE) {
    stopifnot(getRversion() >= "4.1.0")
    args <- list(grob = patternGrob(...),
                 x = x, y = y, width = width, height = height, 
                 default.units = default.units,
                 just = just, hjust = hjust, vjust = vjust)
    # `group` was introduced in R 4.2
    if (getRversion() >= "4.2.0")
        args$group <- group
    do.call(grid::pattern, args)
}
