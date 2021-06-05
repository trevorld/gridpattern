gridpattern v0.1
================

* ``grid.pattern()`` draws a specified "pattern" to the active graphics device.
  while ``patternGrob()`` returns a "pattern" (grid) grob.

* Supports the following original patterns:

  * "regular_polygon" ``grid.pattern_regular_polygon()`` (#20)
  * "weave" ``grid.pattern_regular_weave()`` (#12)

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

    * Resolution of "array" patterns is now user-supplied.
      Defaults to 72 DPI but with a minimum of 12 pixel image width and/or height.
    * Default for "pattern_alpha" is now ``NA_real_`` (preserve existing alpha transparency) instead of ``1`` (set to fully opaque).
    * ``grid.pattern_ambient()`` pattern supports all arguments of the six ``{ambient}`` "noise" functions.
    * ``grid.pattern_circle()`` has several enhancements:

      * Allows use of the argument ``xoffset`` and ``yoffset`` (#22)
      * Allows use of argument ``type`` to setup a "hex" or "hex_circle" arrangement 
        instead of the default "square" arrangement.
      * Use of multiple ``fill`` colors now produces a more attractive pattern.
      * Use of ``density`` greater than ``1`` will now sometimes give an attractive result (#17).
      * Uses ``{sf}`` to clip circles to boundary instead of ``{gridGeometry}``

    * ``grid.pattern_crosshatch()`` allows using the argument ``fill2`` to set 
       different fill color(s) for the "over" crosshatch lines (#14).
    * ``xoffset``, ``yoffset``, and ``spacing`` are now interpreted as "snpc" units and
      ``grid.pattern_stripe()`` and ``grid.pattern_crosshatch()`` now allow using the 
      argument ``type`` to tweak placement of lines so that they now match the placement
      of circles from ``grid.pattern_circle()`` and polygons from ``grid.pattern_regular_polygon()`` (#24).

* ``mean_col()`` is a utility function that computes an "average" color (#21)
* ``weave()`` returns logical matrix a logical matrix indicating where the warp
  lines should "up" for a specified weave pattern type and subtype.  Supports
  weaves listed in `weave_names`.  In particular supports "irregular matt" and
  "elongated twill" family of weave patterns including "zig-zag" and
  "herringbone" variations.
