gridpattern
===========

.. image:: https://travis-ci.org/trevorld/gridpattern.png?branch=main
    :target: https://travis-ci.org/trevorld/gridpattern
    :alt: Build Status

.. image:: https://ci.appveyor.com/api/projects/status/github/trevorld/gridpattern?branch=main&svg=true 
    :target: https://ci.appveyor.com/project/trevorld/gridpattern
    :alt: AppVeyor Build Status

.. image:: https://img.shields.io/codecov/c/github/trevorld/gridpattern.svg
    :target: https://codecov.io/github/trevorld/gridpattern?branch=main
    :alt: Coverage Status

.. image:: https://www.repostatus.org/badges/latest/wip.svg
   :alt: Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.
   :target: https://www.repostatus.org/#wip

``gridpattern`` provides `grid.pattern() / `patternGrob() <http://trevorldavis.com/R/gridpattern/dev/reference/grid.pattern.html>`_ functions for use with R's grid graphics system that fill in a boundary bath in a specified pattern.  These patterned grobs are extracted from the awesome `ggpattern <https://github.com/coolbutuseless/ggpattern>`_ package (which provides ggplot2 "geom" functions but `does not provide exported access to the underlying grobs <https://github.com/coolbutuseless/ggpattern/issues/11>`_ themselves).

We currently provide support for the following ``ggpattern`` patterns:

#. `circle <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-circle.html>`_
#. `crosshatch <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-crosshatch.html>`_
#. `gradient <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-gradient.html>`_
#. none
#. `magick <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-magick.html>`_
#. `stripe <https://coolbutuseless.github.io/package/ggpattern/articles/pattern-stripe.html>`_
#. `Custom geometry-based patterns <https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-2.html>`_
#. `Custom array-based patterns <https://coolbutuseless.github.io/package/ggpattern/articles/developing-patterns-3.html>`_
