# gridpattern <img src="man/figures/logo.png" align="right" width="200px" alt="gridpattern hex sticker">

[![CRAN Status Badge](https://www.r-pkg.org/badges/version/gridpattern)](https://cran.r-project.org/package=gridpattern)
[![R-CMD-check](https://github.com/trevorld/gridpattern/workflows/R-CMD-check/badge.svg)](https://github.com/trevorld/gridpattern/actions)
[![Coverage Status](https://codecov.io/github/trevorld/gridpattern/branch/main/graph/badge.svg)](https://app.codecov.io/github/trevorld/gridpattern?branch=main)

### Table of Contents

* [Overview](#overview)
* [Installation](#installation)
* [Examples](#examples)
* [Using these patterns with the "ggpattern" package](#ggpattern)
* [Using these patterns with the "ggplot2" package](#ggplot2)
* [Using these patterns with the "piecepackr" package](#piecepackr)

## <a name="overview">Overview</a>

`gridpattern` provides [grid.pattern()](http://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern.html) and
[patternGrob()](http://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern.html)
functions to use with R's grid graphics system. They fill in a
user-specified boundary path with a user-specified pattern. These pattern grobs include
enhanced versions of the patterns originally contained within
[Mike FC](https://github.com/coolbutuseless)'s awesome
[ggpattern](https://github.com/trevorld/ggpattern) package
as well as original "pch", "polygon_tiling", "regular_polygon", "rose", "text", "wave", and "weave" patterns.

`{gridpattern}` currently provides `{grid}` grob support for the following patterns:

1.  [ambient](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_ambient.html):
    noise array patterns powered by the [ambient](https://cran.r-project.org/package=ambient) package
2.  [circle](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_circle.html): circle geometry patterns
3.  [crosshatch](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_crosshatch.html): crosshatch geometry patterns
4.  [fill](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_fill.html): simple fill patterns
5.  [gradient](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_gradient.html): gradient array/geometry patterns
6.  [image](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_image.html): image array patterns
7.  [magick](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_magick.html): imagemagick array patterns
8.  [none](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_none.html): draws nothing
9.  [placeholder](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_placeholder.html): placeholder image array patterns
10.  [plasma](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_plasma.html): plasma array patterns
11. [pch](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_pch.html): plotting character geometry patterns
12. [polygon_tiling](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_polygon_tiling.html): polygon tiling patterns
13. [regular_polygon](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_regular_polygon.html): regular polygon geometry patterns
14. [rose](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_rose.html): rose curve array/geometry patterns
15. [stripe](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_stripe.html): stripe geometry patterns
16. [text](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_text.html): text array/geometry patterns
17. [wave](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_wave.html): wave geometry patterns
18. [weave](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_weave.html): weave geometry patterns
19. [custom geometry-based patterns](https://trevorldavis.com/R/gridpattern/dev/articles/developing-patterns.html)
20. [custom array-based patterns](https://trevorldavis.com/R/gridpattern/dev/articles/developing-patterns.html)

## <a name="installation">Installation</a>

To install the development version use the following command in R:



```r
remotes::install_github("trevorld/gridpattern")
```

## <a name="examples">Examples</a>




```r
library("grid")
library("gridpattern")
x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
```


```r
grid.pattern_circle(x_hex, y_hex, density = 0.5, grid = "hex_circle",
                    fill = c("blue", "yellow", "red"))
```

![](man/figures/README-circle-1.png)

```r
grid.pattern_regular_polygon(x_hex, y_hex, shape = c("convex4", "star8", "circle"),
                             colour = "black", fill = c("blue", "yellow", "red"),
                             density = c(0.45, 0.42, 0.4), spacing = 0.08, angle = 0)
```

![](man/figures/README-regular_star-1.png)

```r
grid.pattern_regular_polygon(x_hex, y_hex, shape = "convex6", grid = "hex",
                             color = "transparent", fill = c("white", "grey", "black"),
                             density = 1.0, spacing = 0.1)
```

![](man/figures/README-regular_hex-1.png)

```r
gp <- gpar(fill = c("yellow", "blue", "red"))
grid.pattern_polygon_tiling(x_hex, y_hex, type = "truncated_hexagonal",
                            spacing = 0.15, gp = gp)
```

![](man/figures/README-truncated_hexagonal-1.png)

```r
blue <- grDevices::rgb(0.35, 0.70, 0.90)
yellow <- grDevices::rgb(0.95, 0.90, 0.25)
red <- grDevices::rgb(0.80, 0.40, 0.00)
green <- grDevices::rgb(0.00, 0.60, 0.50)
grid.rect(gp = gpar(fill = yellow, col = NA))
gp <- gpar(fill = red, col = "black")
grid.pattern_stripe(grid = "hex_circle", density = 0.25, spacing = 0.3,
                    angle = 0, gp = gp)
grid.pattern_stripe(grid = "hex_circle", density = 0.25, spacing = 0.3,
                    angle = -60, gp = gp)
grid.pattern_stripe(grid = "hex_circle", density = 0.25, spacing = 0.3,
                    angle = 60, gp = gp)
gp <- gpar(fill = blue, col = "black")
grid.pattern_regular_polygon(shape = "convex12", grid = "hex_circle", rot = 15,
                             density = 0.82, spacing = 0.3, angle = 0, gp = gp)
gp <- gpar(fill = green, col = "black")
scale <- star_scale(12, 30)
grid.pattern_regular_polygon(shape = "star12", grid = "hex_circle", rot = 15,
                             density = 0.82, spacing = 0.3, angle = 0, gp = gp,
                             scale = scale)
```

![](man/figures/README-rhombitrihexagonal-1.png)

```r
gp <- gpar(fill = c("blue", "red", "yellow", "green"), col = "black")
grid.newpage()
grid.pattern_rose(x_hex, y_hex,
                  spacing = 0.18, density = 0.5, angle = 0,
                  frequency = c(2, 6/4, 5/4, 3/7), gp = gp)
```

![](man/figures/README-rose-1.png)

```r
playing_card_symbols <- c("\u2660", "\u2665", "\u2666", "\u2663")
grid.pattern_text(x_hex, y_hex,
                 shape = playing_card_symbols,
                 colour = c("black", "red", "red", "black"),
                 size = 24, spacing = 0.12, angle = 0)
```

![](man/figures/README-text-1.png)

```r
grid.pattern_wave(x_hex, y_hex, colour = "black", type = "sine",
                  fill = c("red", "blue"), density = 0.4,
                  spacing = 0.15, angle = 0,
                  amplitude = 0.05, frequency = 1 / 0.20)
```

![](man/figures/README-wave-1.png)

```r
grid.pattern_weave(x_hex, y_hex, type = "satin",
             colour = "black", fill = "lightblue", fill2 =  "yellow",
             density = 0.3)
```

![](man/figures/README-weave-1.png)

## <a name="ggpattern">Using these patterns with the "ggpattern" package</a>

`ggpattern` automatically supports all patterns provided by ``gridpattern`` since `ggpattern` v0.2.0.
Here is an example of using the "stripe" pattern:


```r
library("ggpattern") # remotes::install_github("trevorld/ggpattern")
library("ggplot2", warn.conflicts = FALSE)
library("gridpattern")
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(aes(fill = trt), colour = 'black',
                     pattern = 'stripe',
                     pattern_fill = list(c("grey30" , "grey70", "white", "grey70")))
```

![](man/figures/README-ggpattern-1.png)

One can also create [custom patterns](https://trevorldavis.com/R/ggpattern/dev/articles/developing-patterns.html) for use with `ggpattern`.
Here is an example of creating a "tiling3" pattern that creates
three-color polygon tilings using the ``fill`` colour, the ``pattern_fill`` colour,
and their "average" color.


```r
tiling3_pattern <- function(params, boundary_df, aspect_ratio, legend = FALSE) {
    args <- as.list(params)
    args <- args[grep("^pattern_", names(args))]

    # hexagonal tiling using "regular_polygon" pattern
    args$pattern <- "polygon_tiling"

    # three-color tiling using `fill`, `pattern_fill` and their "average"
    avg_col <- gridpattern::mean_col(params$fill, params$pattern_fill)
    args$pattern_fill <- c(params$fill, avg_col, args$pattern_fill)

    args$x <- boundary_df$x
    args$y <- boundary_df$y
    args$id <- boundary_df$id
    args$prefix <- ""

    do.call(gridpattern::patternGrob, args)
}

options(ggpattern_geometry_funcs = list(tiling3 = tiling3_pattern))
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
    geom_col_pattern(aes(fill = trt, pattern_type = trt),
                     pattern = 'tiling3', pattern_angle = 45) +
    scale_pattern_type_manual(values = c("hexagonal", "tetrakis_square", "rhombille")) +
    theme(legend.key.size = unit(1.5, 'cm'))
```

![](man/figures/README-hex_ggpattern-1.png)

## <a name="ggplot2">Using these patterns with the "ggplot2" package</a>

Although use of the ["ggpattern" package](#ggpattern) package is still highly recommended one can directly use pattern/gradient fills in base `ggplot2` geoms since v3.5.0.
[`patternFill()`](https://trevorldavis.com/R/gridpattern/dev/reference/patternFill.html) wraps `grid::pattern()` and [patternGrob()](http://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern.html) and is one way to use `gridpattern` patterns directly with `ggplot2`:


```r
library("ggplot2", warn.conflicts = FALSE)
library("gridpattern")
pal <- grDevices::palette()
herringbone <- patternFill("polygon_tiling", type = "herringbone", fill = pal[2])
truncated_hexagonal <- patternFill("polygon_tiling", type = "truncated_hexagonal", fill = pal[3])
pythagorean <- patternFill("polygon_tiling", type = "pythagorean", fill = pal[4])
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
    geom_col(fill = list(herringbone, truncated_hexagonal, pythagorean))
```

![](man/figures/README-ggplot2-1.png)

## <a name="piecepackr">Using these patterns with the "piecepackr" package</a>

[piecepackr](https://github.com/piecepackr/piecepackr) allows the use of [custom grob functions](https://trevorldavis.com/piecepackr/custom-grob-functions.html) to completely customize the appearance of one's game pieces.  `{piecepackr}` comes with a variety of convenience functions such as `pp_shape()` to facilitate creating custom game pieces using custom grob functions.  Here is an example of creating "patterned" checkers filled with uniform polygon tilings by using ``pp_shape()`` objects' `pattern()` method powered by `{gridpattern}`:


```r
library("grid")
library("gridpattern")
library("piecepackr")

tilings <- c("hexagonal", "snub_square", "pythagorean",
             "truncated_square", "triangular", "trihexagonal")
patternedCheckerGrobFn <- function(piece_side, suit, rank, cfg) {
    opt <- cfg$get_piece_opt(piece_side, suit, rank)
    shape <- pp_shape(opt$shape, opt$shape_t, opt$shape_r, opt$back)
    gp_pattern <- gpar(col=opt$suit_color, fill=c(opt$background_color, "white"))
    pattern_grob <- shape$pattern("polygon_tiling", type = tilings[suit],
                                  spacing = 0.3, name = "pattern",
                                  gp = gp_pattern, angle = 0)
    gp_border <- gpar(col=opt$border_color, fill=NA, lex=opt$border_lex)
    border_grob <- shape$shape(gp=gp_border, name = "border")
    grobTree(pattern_grob, border_grob)
}
checkers1 <- as.list(game_systems()$checkers1)
checkers1$grob_fn.bit <- patternedCheckerGrobFn
checkers1 <- pp_cfg(checkers1)

x1 <- c(1:3, 1:2, 1)
x2 <- c(6:8, 7:8, 8)
df <- tibble::tibble(piece_side = c("board_face", rep_len("bit_back", 24L)),
                     suit = c(6L, rep(c(1L, 3L, 4L, 5L), each = 6L)),
                     rank = 8L,
                     x = c(4.5, x1, rev(x1), x2, rev(x2)),
                     y = c(4.5, rep(c(1,1,1, 2,2, 3, 6, 7,7, 8,8,8), 2)))

pmap_piece(df, cfg=checkers1, default.units="in")
```

![](man/figures/README-piecepackr-1.png)
