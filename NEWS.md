gridpattern v0.1
================

* ``grid.pattern()`` draws a specified "pattern" to the active graphics device.
  while ``patternGrob()`` returns a "pattern" (grid) grob.

* Supports  the following patterns from [ggpattern](https://github.com/coolbutuseless/ggpattern):

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

  Enhancements to the ``ggpattern`` patterns provided by this package:

    * Resolution of "array" patterns is now user-supplied.
      Defaults to 72 DPI but with a minimum of 12 pixel image width and/or height.
    * Default for "pattern_alpha" is now ``NA`` (preserve existing alpha transparency) instead of ``1`` (set to fully opaque).
    * ``grid.pattern_ambient()`` pattern supports all arguments of the six [ambient](https://cran.r-project.org/package=ambient) 
      "noise" functions plus a "seed" argument to make them reproducible (for an identical image resolution).
    * ``grid.pattern_crosshatch()`` allows using the argument ``fill2`` to set 
       different fill color(s) for the "over" crosshatch lines (#14).

* Supports the following new patterns:

  * "regular_polygon" ``grid.pattern_regular_polygon()`` (#20)
