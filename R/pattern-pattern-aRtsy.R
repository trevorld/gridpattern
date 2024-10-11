#' Create grob objects for the pattern elements within a boundary
#'
#' @param params params/coords for a single element. named list or single row data.frame
#' @param boundary_df mask for the pattern rendering
#' @param aspect_ratio a aspect ratio of the plotting area.
#' @param legend is the pattern being created in the legend? default FALSE.
#'  Use this flag if you want different pattern drawing behviour for the legend.
#'
#' @return grid grob objects.
#' @noRd
create_pattern_aRtsy <- function(params, boundary_df, aspect_ratio,
                                 legend = FALSE) {
    assert_suggested("aRtsy", "aRtsy")
    requireNamespace("aRtsy", quietly = TRUE)
    stopifnot(guess_has_R4.1_features("patterns"))
    alpha <- ifelse(is.na(params$pattern_alpha), 1, params$pattern_alpha)
    colors <- update_alpha(params$pattern_fill, alpha)
    fn_name <- paste0("canvas_", params$pattern_type)
    fn <- utils::getFromNamespace(fn_name, "aRtsy")
    args <- list()
    nformals <- names(formals(fn))
    if ("color" %in% nformals) { # e.g. `canvas_maze()`
        args$color <- colors
    }
    if ("colors" %in% nformals) { # e.g. most canvas functions
        args$colors <- colors
    }
    pat <- ggplot2pat(do.call(fn, args))
    gp <- grid::gpar(col = NA_character_, fill = pat)
    convert_polygon_df_to_polygon_grob(boundary_df, gp = gp)
}

#' Grobs with patterns powered by the aRtsy package
#'
#' `grid.pattern_aRtsy()` draws patterns powered by the `aRtsy` package.
#' `names_aRtsy()` returns character vector of supported types.
#'
#' @param type Name of pattern.
#' @inheritParams grid.pattern_circle
#' @param fill Passed to the underlying `aRtsy` function's `color` / `colors` argument.
#' @param ... Currently ignored
#' @return A grid grob object invisibly.  If `draw` is `TRUE` then also draws to the graphic device as a side effect.
#' @examples
#' print(names_aRtsy())
#'
#' \donttest{# Make take more than 5 seconds on CRAN servers
#' x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
#' if (requireNamespace("aRtsy", quietly = TRUE) &&
#'     guess_has_R4.1_features("patterns")) {
#'   grid::grid.newpage()
#'   grid.pattern_aRtsy(x_hex, y_hex, type = "forest",
#'                      fill = c("black", "white", "grey"))
#' }
#' }
#' @seealso <https://koenderks.github.io/aRtsy/> for more information about the `aRtsy` package.
#' @export
grid.pattern_aRtsy <- function(x = c(0, 0, 1, 1), y = c(1, 0, 0, 1), id = 1L, ...,
                               type = "strokes",
                               fill = gp$fill %||% "grey80",
                               alpha = gp$alpha %||% NA_real_,
                               default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) {
    grid.pattern("aRtsy", x, y, id,
                 type = type, fill = fill, alpha = alpha,
                 default.units = default.units, name = name, gp = gp , draw = draw, vp = vp)
}

#' @rdname grid.pattern_aRtsy
#' @export
names_aRtsy <- function() {
    assert_suggested("aRtsy", "aRtsy")
    requireNamespace("aRtsy", quietly = TRUE)
    fns <- grep("^canvas", getNamespaceExports("aRtsy"), value = TRUE)
    gsub("^canvas_", "", fns)
}
