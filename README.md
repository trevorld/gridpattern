# gridpattern

[![Build Status](https://travis-ci.org/trevorld/gridpattern.png?branch=main)](https://travis-ci.org/trevorld/gridpattern)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/trevorld/gridpattern?branch=main&svg=true)](https://ci.appveyor.com/project/trevorld/gridpattern)
[![Coverage Status](https://img.shields.io/codecov/c/github/trevorld/gridpattern.svg)](https://codecov.io/github/trevorld/gridpattern?branch=main)
[![Project Status: WIP -- Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

`gridpattern` provides [grid.pattern() and
patternGrob()](http://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern.html)
functions to use with R\'s grid graphics system. They fill in a
specified boundary bath with a pattern. These patterned grobs are
extracted from [Mike FC](https://github.com/coolbutuseless)\'s awesome
[ggpattern](https://github.com/coolbutuseless/ggpattern) package (which
provides patterned ggplot2 \"geom\" functions but [does not provide
exported access to the underlying
grobs](https://github.com/coolbutuseless/ggpattern/issues/11)
themselves).

We currently provide support for the following `ggpattern` patterns:

1.  [ambient](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_ambient.html)
2.  [circle](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_circle.html)
3.  [crosshatch](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_crosshatch.html)
4.  [gradient](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_gradient.html)
5.  [image](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_image.html)
6.  [magick](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_magick.html)
7.  none (equivalent to `grid::null()`)
8.  [placeholder](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_placeholder.html)
9.  [plasma](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_plasma.html)
10. [stripe](https://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern_stripe.html)
11. [Custom geometry-based
    patterns](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html)
12. [Custom array-based
    patterns](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html)
