#' Stripe patterned grobs
#'
#' `grid.pattern_stripe()` draws a stripe pattern onto the graphic device.
#'
#' @inheritParams grid.pattern_circle
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#'   if (require("grid")) {
#'     x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#'     grid.pattern_stripe(x_hex, y_hex, colour = "black",
#'                         fill = c("red", "blue"), density = 0.4)
#'
#'     # Can alternatively use "gpar()" to specify colour and line attributes
#'     grid.newpage()
#'     grid.pattern_stripe(x_hex, y_hex, density = 0.3,
#'                         gp = gpar(col = "blue", fill = "yellow"))
#'   }
#' @seealso `[grid.pattern_crosshatch()]` and `[grid.pattern_weave()]` for overlaying stripes
#'  as well as the `ggpattern` documentation:
#'  <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html>
#' @export
grid.pattern_stripe <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                                colour = gp$col %||% "grey20", fill = gp$fill %||% "grey80", angle = 30,
                                density = 0.2, spacing = 0.05, xoffset = 0, yoffset = 0,
                                alpha = gp$alpha %||% NA_real_, linetype = gp$lty %||% 1, size = gp$lwd %||% 1,
                                type = "diagonal",
                                default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    if (missing(colour) && hasName(l <- list(...), "color")) colour <- l$color
    grid.pattern("stripe", x, y, id,
                 colour = colour, fill = fill, angle = angle,
                 density = density, spacing = spacing, xoffset = xoffset, yoffset = yoffset,
                 alpha = alpha, linetype = linetype, size = size,
                 type = type,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

create_pattern_stripes_via_sf <- function(params, boundary_df, aspect_ratio,
                                          legend = FALSE) {
    create_crosshatch_via_sf_helper(params, boundary_df, add_top_hatch = FALSE)
}
