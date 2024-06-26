% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pattern-geometry-none.R
\name{grid.pattern_none}
\alias{grid.pattern_none}
\title{Grobs without any pattern}
\usage{
grid.pattern_none(
  x = c(0, 0, 1, 1),
  y = c(1, 0, 0, 1),
  id = 1L,
  ...,
  default.units = "npc",
  name = NULL,
  gp = gpar(),
  draw = TRUE,
  vp = NULL
)
}
\arguments{
\item{x}{A numeric vector or unit object specifying x-locations of the pattern boundary.}

\item{y}{A numeric vector or unit object specifying y-locations of the pattern boundary.}

\item{id}{A numeric vector used to separate locations in x, y into multiple boundaries.
All locations within the same \code{id} belong to the same boundary.}

\item{...}{Currently ignored}

\item{default.units}{A string indicating the default units to use if \code{x} or \code{y}
are only given as numeric vectors.}

\item{name}{ A character identifier. }

\item{gp}{An object of class \code{"gpar"}, typically the output
    from a call to the function \code{\link[grid]{gpar}}.  This is basically
    a list of graphical parameter settings.}

\item{draw}{A logical value indicating whether graphics output
    should be produced.}

\item{vp}{A Grid viewport object (or NULL).}
}
\value{
A grid grob object invisibly.  If \code{draw} is \code{TRUE} then also draws to the graphic device as a side effect.
}
\description{
\code{grid.pattern_none()} draws nothing onto the graphic device.
}
\examples{
x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
grid.pattern_none(x_hex, y_hex)
}
\seealso{
\code{\link[grid:grid.null]{grid::grid.null()}}
}
