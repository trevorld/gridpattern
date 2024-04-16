gridpattern v1.2.0 (development)
================================

* `patternFill()` wraps `patternGrob()` to return a
  `grid::pattern()` fill object (#70).

* `update_alpha()` updates fill colour and/or pattern transparency.

  + It is a fork of `ggplot2::fill_alpha()` by @teunbrand.
  + It does not depend on `{ggplot2}` or `{scales}`.
  + It does not throw an error with a length one list of a vector of multiple colours.

* The "geometry" patterns (e.g. "circle", "stripe", etc.) now allow
  the `fill` to be pattern fills (#67).

gridpattern v1.1.1
==================

* "text" pattern example now skipped if ran within certain graphics devices like `pdf()`
  that can't handle the playing cards Unicode glyphs (#68).

gridpattern v1.1.0
==================

New Features
------------

* `reset_image_cache()` resets the image cache used by `grid.pattern_image()` 
   and `grid.pattern_placeholder()` to store images (#63).

Bug fixes and minor improvements
--------------------------------

* The function `guess_has_R4.1_features()` is now available as a "standalone" file.
  You may copy its source under the permissive [MIT No Attribution (MIT-0)](https://spdx.org/licenses/MIT-0.html) 
  license into your own R package
  by either using `usethis::use_standalone("trevorld/gridpattern", "standalone-guess_has_R4.1_features.R")`
  or simply copying `standalone-guess_has_R4.1_features.R` into your `R` directory and 
  adding `grDevices` and `utils` to the `Imports` of your `DESCRIPTION` file.
* If `{vdiffr}` has been updated to at least v1.0.6 (released 2023-08-25) then
  `guess_has_R4.1_features()` should now always correctly guess R4.1 feature support for the
  `svglite::svglite()` and `vdiffr:::svglite()` graphics devices.

gridpattern v1.0.2
==================

Bug fixes and minor improvements
--------------------------------

* An internal function `convert_polygon_sf_to_polygon_df()` will now be a bit more
  quiet (throw WARNINGs less often).

gridpattern v1.0.1
==================

Bug fixes and minor improvements
--------------------------------

* `guess_has_R4.1_features()` can now also take advantage if `dev.capabilities()`
  **explicitly** indicates that a feature is **not** supported in active graphics device.
* To match aesthetic changes in `{ggplot2}` one may now use the parameter `linewidth` to 
   set polygon/path line widths in
  `grid.pattern_circle()`, `grid.pattern_crosshatch()`, `grid.pattern_pch()`, 
  `grid.pattern_polygon_tiling()`, 
  `grid.pattern_regular_polygon()`, `grid.pattern_stripe()`, `grid.pattern_rose()`, 
  `grid.pattern_wave()`, and `grid.pattern_weave()`.
  For backwards compatibility one may continue to use `size` as well.
  Continue to use `size` (or `gp`) to set the fontsize in `grid.pattern_text()`.
  Both `pattern_linewidth` and `pattern_size` will be available
  for use in custom patterns (#57). 

gridpattern v0.5.3
==================

Bug fixes and minor improvements
--------------------------------

* Fixes bug with `grid.pattern_image()` with `type = "tile"` (#47).
  Additionally `grid.pattern_image()` now supports `gravity` argument when `type = "tile"`.
* If the active graphics device has also implemented the new R 4.2 `dev.capabilities()` support then
  `guess_has_R4.1_features()` can now better guess R 4.1 graphic feature support when called within R 4.2.

  + In particular `guess_has_R4.1_features()` can now better guess R 4.1 graphic feature support in the
    `{grDevices}` bitmap devices (i.e. `bmp()`, `jpeg()`, `png()`, `tiff()`) 
    when called within R 4.2 on Windows.
    Previously it was not possible to easily distinguish on Windows if the device was called with 
   `type = "windows"` or `type = "cairo"` and hence we had to conservatively guess no such support
    even if `type = "cairo"` had been specified (and within R 4.1 we must still conservatively do so).
  + Also if in the future any graphic devices add R 4.1 graphic feature support as well as 
    R 4.2 `dev.capabilities()` support then we should now be able to correctly guess such support within R 4.2
    without needing to manually update `guess_has_R4.1_features()`.

* `guess_has_R4.1_features()` now supports an argument `features` which allows one to
  limit the guessing of R4.1 feature support to a subset of `c("clippingPaths", "gradients", "masks", "patterns")`.
  Although all known graphic devices either implements all or none of these features
  this need not hold true in the future.

gridpattern v0.5.1
==================

Breaking Changes
----------------

* `grid.pattern_rose()` and `grid.pattern_text()` now "clip" their boundary using an 
  "alpha mask" rather than a "clipping path".  
  Now use the argument `use_R4.1_masks`, the global option `ggpattern_use_R4.1_masks`, 
  or the global option `ggpattern_use_R4.1_features` to toggle on/off the R 4.1 alpha mask 
  feature instead of using the `use_R4.1_clipping` argument or 
  the global option `ggpattern_use_R4.1_clipping` as before
  (the latter continue to toggle on/off the R 4.1 clipping path feature in `clippingPathGrob()`).

New Features
------------

* `alphaMaskGrob()` is a function that (alpha) masks one grob by using a second grob to specify the (alpha) mask.

  * If `use_R4.1_masks` is `TRUE` we simply use the new R 4.1 (alpha) masks feature.
  * If `use_R4.1_masks` is `FALSE` we generate a `grid::rasterGrob()` approximation.
  * If `use_R4.1_masks` is `NULL` try to guess an appropriate choice.
  * The default for `use_R4.1_masks` can be set by `options("ggpattern_use_R4.1_masks")`.

Bug fixes and minor improvements
--------------------------------

* `guess_has_R4.1_features()` now returns `TRUE` for the `ragg::agg_jpeg()`,
  `ragg::agg_ppm()`, and `ragg::agg_tiff()` devices if `packageVersion("ragg") >= '1.2.0'`.
  It also returns `TRUE` for `svglite::svglite()` if `packageVersion("svglite") >= '2.1.0'`.
* `clippingPathGrob()` will now consistently - as previously documented - use `ragg::agg_png()` 
  as the default `png_device` graphics device if it is available and `use_R4.1_clipping` is `FALSE`.
* `grid.pattern_image()` should no longer throw an inscrutable `Error in magick_image_readpath`...
  error on certain platforms such as Windows (#47).

gridpattern v0.4.0
==================

New Features
------------

* `guess_has_R4.1_features()` is now an exported function.
  It guesses whether the "active" graphic devices supports the
  new [R 4.1 graphics features](https://www.stat.auckland.ac.nz/~paul/Reports/GraphicsEngine/definitions/definitions.html) and returns
  `TRUE` or `FALSE`.

Bug fixes and minor improvements
--------------------------------

* `grid.pattern_polygon_tiling()` now supports the "elongated_triangular" `type` (#48).
  "geometry" patterns now support a "elongated_triangle" `grid` value.
* `guess_has_R4.1_features()` now returns `TRUE` for the `ragg::agg_png()` and
  `ragg::agg_supertransparent()` devices if `packageVersion("ragg") >= '1.2.0'`.
  It now also returns `TRUE` for the `grDevices::bmp(type = "cairo")`, 
  `grDevices::cairo_ps()`, `grDevices::jpeg(type = "cairo")`, 
  and `grDevices::tiff(type = "cairo")` devices if `getRversion() >= '4.1.0'`.

gridpattern v0.3.1
==================

Breaking Changes
----------------

* The package `{magick}` has been downgraded from "Imports" to "Suggests" (#44).
  You'll need to manually install `{magick}` with `install.packages("magick")` 
  in order to use the following "array" patterns:

  - "gradient" (but only if `use_R4.1_gradients` is `FALSE`)
  - "image"
  - "magick"
  - "placeholder"
  - "plasma"

New Features
------------

* The following package option ``ggpattern_res`` can now be set by `options()`.
  It controls default "raster" image pattern resolution (pixels per inch).
  Defaults to 72 (pixels per inch).

* `grid.pattern_polygon_tiling()` now supports the following additional polygon tiling `type`'s (#42):

  - `"rhombille"` implements a rhombille tiling of rhombi (#37)
  - `"tetrakis_square"` implements a tetrakis square tiling of isosceles right triangles (#38)
  - `"2*.2**.2*.2**"` implements a polygon tiling made of rhombi.
  - `"2**.3**.12*"` implements a polygon tiling made of rhombi, triangles, and twelve-pointed stars.
  - `"3.3.3.3**"` implements a polygon tiling made of triangles.
  - `"3.3*.3.3**"` implements a regular (star) polygon tiling made of triangles and three-pointed stars.
  - `"3.3.3.12*.3.3.12*"` implements a regular (star) polygon tiling made of triangles and twelve-pointed starts.
  - `"3.3.8*.3.4.3.8*"` implements a regular (star) polygon tiling of triangles,
    squares, and eight-pointed stars.
  - `"3.3.8*.4**.8*"` implements a regular (star) polygon tiling made of triangles, four-pointed stars,
    and eight-pointed stars.
  - `"3.4.6.3.12*"` implements a regular (star) polygon tiling made of triangles, squares,
    hexagons, and twelve-pointed stars.
  - `"3.4.8.3.8*"` implements a regular (star) polygon tiling of triangles,
    squares, octagons, and eight-pointed stars.
  - `"3.6*.6**"` implements a regular (star) polygon tiling made of triangles and six-pointed stars.
  - `"4.2*.4.2**"` implements a polygon tiling made of squares and rhombi.
  - `"4.4*.4**"` implements a regular (star) polygon tiling made of squares and four-pointed stars.
  - `"4.6.4*.6"` implements a regular (star) polygon tiling made of squares, hexagons, and four-pointed stars.
  - `"4.6*.4.6*.4.6*"` implements a regular (star) polygon of squares and six-pointed stars.
  - `"4.8*.4**.8*"` implements a polygon tiling of squares and eight-pointed stars.
  - `"6.6*.6.6*"` implements a regular (star) polygon of hexagons and six-pointed stars.
  - `"8.4*.8.4*"` implements a regular (star) polygon of octagons and four-pointed stars.
  - `"9.3.9.3*"` implements a regular (star) polygon of triangles, nonagons, and three-pointed stars.
  - `"12.3*.12.3*"` implements a regular (star) polygon tiling made of dodecagons and three-pointed stars.
  - `"12.12.4*"` implements a regular (star) polygon tiling made of dodecagons and four-pointed stars.
  - `"18.18.3*"` implements a regular (star) polygon tiling made of eighteen-sided polygons and three-pointed stars.

* `grid.pattern_regular_polygon()` now supports a `"tetrakis_left"` `shape` and `"tetrakis_right"` `shape` 
  which both draw an isosceles right triangle (one oriented left and one oriented right) as well as a
 `"rhombille_rhombus"` shape which draws a rhombus.
  These are non-regular polygons intended to help produce tetrakis square and rhombille polygon tilings.

Bug fixes and minor improvements
--------------------------------

* `star_angle()` and `star_scale()` now handle the `n_vertices == 2` case 
  (a "two-pointed star" polygon is a rhombus).
* Fixes `grid.pattern_gradient()` when `use_R4.1_gradients=TRUE` to better
  match behavior when `use_R4.1_gradients=FALSE`.
* Now allows alpha values to be specified
  within the `fill` (and `fill2`) colour strings for
  `gridpattern_plasma()` and `gridpattern_gradient()` (when `use_R4.1_gradients=FALSE`).
* In `pattern_square()` for the "horizontal" and "vertical" types the value `1L` is
  now guaranteed to be in the center of the pattern (#46).

gridpattern v0.2.1
==================

Breaking Changes
----------------

* "array" patterns no longer set a minimum 12 pixel image width and/or height.
  In particular, they can now have a zero pixel image width and/or height
  (in which case the array pattern returns a ``grid::nullGrob()``).
* "array" patterns no longer reduces the number of requested pixels when `legend = TRUE`.

New Features
------------

* Supports the following new patterns:

  * "rose" (curve) ``grid.pattern_rose()`` (#43)
  * "text" ``grid.pattern_text()`` (#40)
  * "wave" ``grid.pattern_wave()`` which supports "sine" and "triangle" `type` waves (#16)

* `names_pattern` is a new character vector of supported (builtin) patterns.
* `clippingPathGrob()` is a function that clips one grob according to clipping path set by a second grob.

  * If `use_R4.1_clipping` is `TRUE` we simply use the new R 4.1 clipping path feature
  * If `use_R4.1_clipping` is `FALSE` we generate a `grid::rasterGrob()` approximation
  * If `use_R4.1_clipping` is `NULL` try to guess an appropriate choice.
  * The default for `use_R4.1_clipping` can be set by `options("ggpattern_use_R4.1_clipping")`

* `grid.pattern_gradient()` now supports the argument `use_R4.1_gradients`

  * If `TRUE` use the new R 4.1 gradient feature
  * If `FALSE` use a `rasterGrob` approximation (old behaviour)
  * If `NULL` try to guess an appropriate choice.
  * The default for `use_R4.1_gradients` can be set by `options("ggpattern_use_R4.1_gradients")`

* The following package options can now be set by `options()`:

  * `ggpattern_use_R4.1_clipping` If `TRUE` use the grid clipping path feature introduced in R v4.1.0.
                    If `FALSE` do a `rasterGrob` approximation of the clipped pattern.
                    Currently used by `clippingPathGrob()`, `grid.pattern_rose()`, 
                    `grid.pattern_text()`, and available for custom patterns.
  * `ggpattern_use_R4.1_features` Set the default for all the other
                    `ggpattern_use_R4.1_*` options arguments.
  * `ggpattern_use_R4.1_gradients` If `TRUE` use the grid gradient feature introduced in R v4.1.0.
                    If `FALSE` do a `rasterGrob` approximation of the gradient pattern.
                    Currently used by `grid.pattern_gradient()` and available for custom patterns.
  * `ggpattern_use_R4.1_masks` If `TRUE` use the grid mask feature introduced in R v4.1.0.
                    Currently unused by this package but available for custom patterns.
  * `ggpattern_use_R4.1_patterns` If `TRUE` use the grid pattern feature introduced in R v4.1.0.
                    Currently unused by this package but available for custom patterns.

Bug fixes and minor improvements
--------------------------------

* `grid.pattern()` now throws an error if there is a non-unique pattern name
  (set by `options("ggpattern_geometry_funcs")` and/or `options("ggpattern_array_funcs")`).
* Fixes an error when *catching* a {magick} image reading error
  for some array patterns.

gridpattern v0.1.2
==================

* ``grid.pattern()`` draws a specified "pattern" to the active graphics device
  while ``patternGrob()`` returns a "pattern" (grid) grob.

* Supports the following original patterns:

  * "pch" ``grid.pattern_pch()``
  * "polygon_tiling" ``grid.pattern_polygon_tiling()`` (#13)
  * "regular_polygon" ``grid.pattern_regular_polygon()`` (#20)
  * "weave" ``grid.pattern_weave()`` (#12)

* Supports the following patterns from [ggpattern](https://github.com/coolbutuseless/ggpattern):

  * "ambient" ``grid.pattern_ambient()``  
  * "circle" ``grid.pattern_circle()`` (#5)
  * "crosshatch"  ``grid.pattern_crosshatch()``(#4)
  * "gradient"  ``grid.pattern_gradient()``(#8)
  * "image"  ``grid.pattern_image()``(#9)
  * "magick" ``grid.pattern_magick()`` (#6)
  * "none" ``grid::null()``
  * "placeholder" ``grid.pattern_placeholder()`` (#10)
  * "plasma" ``grid.pattern_plasma()`` (#7)
  * "stripe" ``grid.pattern_stripe()`` (#3)
  * [Custom ggpattern geometry-based patterns](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html) (#1)
  * [Custom ggpattern array-based patterns](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html) (#2)

  Enhancements made to the ``{ggpattern}`` patterns provided by this package:

    * Resolution of "array" patterns is now settable by new "pattern_res".
      Defaults to 72 pixels per inch but sets a minimum 12 pixel image width and/or height.
    * Default for "pattern_alpha" is now ``NA_real_`` (preserve existing alpha transparency) instead of ``1`` (set to fully opaque).
    * ``grid.pattern_ambient()`` pattern supports all arguments of the six ``{ambient}`` "noise" functions.
    * ``grid.pattern_circle()`` has several enhancements:

      * Allows use of the argument ``xoffset`` and ``yoffset`` (#22)
      * Allows use of arguments ``grid``, ``type``, and ``subtype`` to customize the pattern arrangement.
      * Use of multiple ``fill`` colors now produces a more attractive pattern.
      * Use of ``density`` greater than ``1`` will now sometimes give an attractive result (#17).
      * Uses ``{sf}`` to clip circles to boundary instead of ``{gridGeometry}``

    * ``grid.pattern_crosshatch()`` allows using the argument ``fill2`` to set 
       different fill color(s) for the "over" crosshatch lines (#14).
    * ``xoffset``, ``yoffset``, and ``spacing`` are now interpreted as "snpc" units and
      ``grid.pattern_stripe()`` and ``grid.pattern_crosshatch()`` now allow using the 
      argument ``grid`` to tweak placement of lines so that they now match the placement
      of circles from ``grid.pattern_circle()`` and polygons from ``grid.pattern_regular_polygon()`` (#24).
    * The center point of the geometry "grid" now matches the center of the viewport.

* ``mean_col()`` is a utility function that computes an "average" color (#21)
* ``star_scale()`` and `star_angle()` are utility functions for converting
  between regular star polygon parameterizations (#39).
* ``pattern_hex()`` returns an integer matrix indicating placement of multiple
  colors (or other graphical elements) on a hex grid.  Supports types listed in `hex_names`.
* ``pattern_square()`` returns an integer matrix indicating placement of multiple
  colors (or other graphical elements) on a rectangular grid.  Supports types listed in `square_names`.
* ``pattern_weave()`` returns a logical matrix indicating where the warp
  lines should "up" for a specified weave pattern type and subtype.  Supports
  weaves listed in `weave_names`.  In particular supports "irregular matt" and
  "elongated twill" family of weave patterns including "zig-zag" and
  "herringbone" variations.
