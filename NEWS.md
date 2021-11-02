gridpattern v0.3.0
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

* `grid.pattern_polygon_tiling()` now supports the following additional polygon tiling `type`'s:

  - `"rhombille"` implements a rhombille tiling of rhombi (#37)
  - `"tetrakis_square"` implements a tetrakis square tiling of isosceles right triangles (#38)
  - `"3.3.3.12*30.3.3.12*30"` implements a regular (star) polygon tiling made of triangles and twelve-pointed starts.
  - `"3.3.8*15.3.4.3.8*15"` implements a regular (star) polygon tiling of triangles,
    squares, and eight-pointed stars.
  - `"3.4.8.3.8*15"` implements a regular (star) polygon tiling of triangles,
    squares, octagons, and eight-pointed stars.
  - `"4.6*30.4.6*30.4.6*30"` implements a regular (star) polygon of squares and six-pointed stars.

* `grid.pattern_regular_polygon()` now supports a `"tetrakis_left"` `shape` and `"tetrakis_right"` `shape` 
  which both draw an isosceles right triangle (one oriented left and one oriented right) as well as a
 `"rhombille_rhombus"` shape which draws a rhombus.
  These are non-regular polygons intended to help produce tetrakis square and rhombille polygon tilings.

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
