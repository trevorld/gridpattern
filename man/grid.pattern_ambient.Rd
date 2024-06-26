% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pattern-array-ambient.R
\name{grid.pattern_ambient}
\alias{grid.pattern_ambient}
\title{Ambient patterned grobs}
\usage{
grid.pattern_ambient(
  x = c(0, 0, 1, 1),
  y = c(1, 0, 0, 1),
  id = 1L,
  ...,
  type = "simplex",
  fill = gp$fill \%||\% "grey80",
  fill2 = "#4169E1",
  frequency = 0.01,
  interpolator = "quintic",
  fractal = switch(type, worley = "none", "fbm"),
  octaves = 3,
  lacunarity = 2,
  gain = 0.5,
  pertubation = "none",
  pertubation_amplitude = 1,
  value = "cell",
  distance_ind = c(1, 2),
  jitter = 0.45,
  res = getOption("ggpattern_res", 72),
  alpha = NA_real_,
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

\item{...}{Currently ignored.}

\item{type}{Either cubic, perlin, simplex, value, white, or worley}

\item{fill}{Colour.}

\item{fill2}{Second colour.}

\item{frequency}{Determines the granularity of the features in the noise.}

\item{interpolator}{How should values between sampled points be calculated?
Either \code{'linear'}, \code{'hermite'}, or \code{'quintic'} (default), ranging from lowest
to highest quality.}

\item{fractal}{The fractal type to use. Either \code{'none'}, \code{'fbm'} (default),
\code{'billow'}, or \code{'rigid-multi'}. It is suggested that you experiment with the
different types to get a feel for how they behaves.}

\item{octaves}{The number of noise layers used to create the fractal noise.
Ignored if \code{fractal = 'none'}. Defaults to \code{3}.}

\item{lacunarity}{The frequency multiplier between successive noise layers
when building fractal noise. Ignored if \code{fractal = 'none'}. Defaults to \code{2}.}

\item{gain}{The relative strength between successive noise layers when
building fractal noise. Ignored if \code{fractal = 'none'}. Defaults to \code{0.5}.}

\item{pertubation}{The pertubation to use. Either \code{'none'} (default),
\code{'normal'}, or \code{'fractal'}. Defines the displacement (warping) of the noise,
with \code{'normal'} giving a smooth warping and \code{'fractal'} giving a more eratic
warping.}

\item{pertubation_amplitude}{The maximal pertubation distance from the
origin. Ignored if \code{pertubation = 'none'}. Defaults to \code{1}.}

\item{value}{The noise value to return. Either
\itemize{
\item \code{'value'} (default) A random value associated with the closest point
\item \code{'distance'} The distance to the closest point
\item \code{'distance2'} The distance to the nth closest point (n given by
\code{distance_ind[1]})
\item \code{'distance2add'} Addition of the distance to the nth and mth closest point given in \code{distance_ind}
\item \code{'distance2sub'} Substraction of the distance to the nth and mth closest point given in \code{distance_ind}
\item \code{'distance2mul'} Multiplication of the distance to the nth and mth closest point given in \code{distance_ind}
\item \code{'distance2div'} Division of the distance to the nth and mth closest point given in \code{distance_ind}
}}

\item{distance_ind}{Reference to the nth and mth closest points that should
be used when calculating \code{value}.}

\item{jitter}{The maximum distance a point can move from its start position
during sampling of cell points.}

\item{res}{Assumed resolution (in pixels per graphic device inch) to use when creating array pattern.}

\item{alpha}{Alpha (between 0 and 1) or \code{NA} (default, preserves colors' alpha value).}

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
\code{grid.pattern_ambient()} draws noise patterns onto the graphic device powered by the \code{ambient} package.
}
\examples{
 if (requireNamespace("ambient", quietly = TRUE)) {
   x_hex <- 0.5 + 0.5 * cos(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
   y_hex <- 0.5 + 0.5 * sin(seq(2 * pi / 4, by = 2 * pi / 6, length.out = 6))
   grid.pattern_ambient(x_hex, y_hex, fill = "green", fill2 = "blue")
 }
 if (requireNamespace("ambient")) {
   grid::grid.newpage()
   grid.pattern_ambient(x_hex, y_hex, fill = "green", fill2 = "blue", type = "cubic")
 }
}
\seealso{
For more information about the noise types please see the relevant \code{ambient} documentation:
\code{\link[ambient:noise_cubic]{ambient::noise_cubic()}}, \code{\link[ambient:noise_perlin]{ambient::noise_perlin()}}, \code{\link[ambient:noise_simplex]{ambient::noise_simplex()}},
\code{\link[ambient:noise_value]{ambient::noise_value()}}, \code{\link[ambient:noise_white]{ambient::noise_white()}}, and \code{\link[ambient:noise_worley]{ambient::noise_worley()}}.
\code{\link[=grid.pattern_plasma]{grid.pattern_plasma()}} provides an alternative noise pattern that depends on \code{magick}.
}
