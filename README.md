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

1.  [circle](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-circle.html)
2.  [crosshatch](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html)
3.  [gradient](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-gradient.html)
4.  [image](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-image.html)
5.  [magick](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-magick.html)
6.  none
7.  [placeholder](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-placeholder.html)
8.  [plasma](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-plasma.html)
9.  [stripe](https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html)
10. [Custom geometry-based
    patterns](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html)
11. [Custom array-based
    patterns](https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html)
