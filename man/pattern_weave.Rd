% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pattern_weave.R
\docType{data}
\name{pattern_weave}
\alias{pattern_weave}
\alias{names_weave}
\title{Weave pattern matrix}
\format{
An object of class \code{character} of length 10.
}
\usage{
pattern_weave(type = "plain", subtype = NULL, nrow = 5L, ncol = 5L)

names_weave
}
\arguments{
\item{type}{Type of weave.  See Details.}

\item{subtype}{Subtype of weave.  See Details.}

\item{nrow}{Number of rows (length of warp).}

\item{ncol}{Number of columns (length of weft).}
}
\value{
A matrix of logical values indicating where the "warp"
is "up" (if \code{TRUE}) or "down" (if \code{FALSE}).
Indices \verb{[1,1]} of the matrix corresponds to the bottom-left of the weave
while indices \verb{[1,ncol]} corresponds to the bottom-right of the weave.
This matrix has a "pattern_weave" subclass which supports a special \code{print()} method.
}
\description{
\code{pattern_weave()} returns a logical matrix indicating where the warp lines should
be "up" for a specified weave pattern type and subtype.
\code{names_weave} is a character vector listing supported weave pattern types.
}
\details{
Here is a list of the various weave \code{type}s supported:

\describe{
\item{basket}{A simple criss-cross pattern using two threads at a time.
Same as the "matt_irregular" weave but with a default \code{subtype} of \code{2L}.}
\item{matt}{A simple criss-cross pattern using 3 (or more) threads at a time.
Same as the "matt_irregular" weave but with a default \code{subtype} of \code{3L}.}
\item{matt_irregular}{A generalization of the "plain" weave.
A character \code{subtype} \code{"U/D(L+R)"} is a standard matt weave specification:
\code{U} indicates number warp up, \code{D} indicates number warp down,
\code{L} indicates number of warp up in repeat, and
\code{R} indicates number of warp down in repeat.
An integer \code{subtype} \code{N} will be interpreted as a \code{"N/N(N+N)"} irregular matt weave.
A character \code{subtype} \code{"U/D"} will be interpreted as a \code{"U/D(U+D)"} irregular matt weave.
Has a default \code{subtype} of \code{"3/2(4+2)"}.}
\item{plain}{A simple criss-cross pattern.
Same as the "matt_irregular" weave but with a default \code{subtype} of \code{1L}.}
\item{rib_warp}{A plain weave variation that emphasizes vertical lines.
An integer \code{subtype} \code{N} will be interpreted as a "matt_irregular" \code{"N/N(1+1)"} weave.
A character \code{subtype} \code{"U/D"} will be interpreted as a "matt_irregular" \code{"U/D(1+1)"} weave.
Default \code{subtype} of \code{2L}.}
\item{satin}{A "regular" satin weave is a special type of the elongated twill weave
with a move number carefully chosen so no twill line is distinguishable.
Same as the "twill_elongated" weave but with a default \code{subtype} of \code{5L}.}
\item{twill}{A simple diagonal pattern.
Same as the "twill_elongated" weave but with a default \code{subtype} of \code{"2/1"}.}
\item{twill_elongated}{A generalization of the "twill" weave.
A character \code{subtype} \code{"U/D(M)"} is a standard twill weave specification:
\code{U} indicates number warp up, \code{D} indicates number warp down,
and \code{M} indicates the "move" number.
A character \code{subtype} \code{"U/D"} will be interpreted as a \code{"U/D(1)"} elongated twill weave.
An integer \code{subtype} \code{N} will provide a \code{"{N-1}/1(1)"} elongated twill weave
if \code{N} is less than 5, 6, or greater than 14 otherwise it will
provide a \code{"{N-1}/1(M)"} weave where \code{M} is the largest
possible regular "satin" move number.
Default \code{subtype} of \code{"4/3(2)"}.}
\item{twill_herringbone}{Adds a (vertical) "herringbone" effect to
the specified "twill_elongated" weave.
Default \code{subtype} of \code{"4/3(2)"}.}
\item{twill_zigzag}{Adds a (vertical) "zig-zag" effect to the specified "twill_elongated" weave.
Default \code{subtype} of \code{"4/3(2)"}.}
}
For both "matt" and "twill" weaves the \code{U/D} part of the subtype can be further extended
to \code{U1/D1*U2/D2}, \code{U1/D1*U2/D2*U3/D3}, etc.
For the "matt" weave the "(L+R)" part of the subtype can be further extended
to \code{(L1+R1+L2+R2)}, \code{(L1+R1+L2+R2+L3+R3)}, etc.
}
\examples{
 # supported weave names
 print(names_weave)

 plain <- pattern_weave("plain", nrow = 7, ncol = 9)
 print(plain)

 matt_irregular <- pattern_weave("matt_irregular", nrow = 9, ncol = 11)
 print(matt_irregular)

 satin <- pattern_weave("satin", nrow = 9, ncol = 11)
 print(satin)

 twill <- pattern_weave("twill", nrow = 9, ncol = 11)
 print(twill)

 twill_zigzag <- pattern_weave("twill_zigzag", nrow = 18, ncol = 11)
 print(twill_zigzag)

}
\seealso{
\code{\link[=grid.pattern_weave]{grid.pattern_weave()}} for drawing weaves onto a graphics device.
See \url{https://textilestudycenter.com/derivatives-of-plain-weave/}
for further information on the "matt" family of weaves,
\url{https://textilelearner.net/twill-weave-features-classification-derivatives-and-uses/}
for further information on the "twill" family of weaves, and
\url{https://texwiz101.blogspot.com/2012/03/features-and-classification-of-satin.html}
for further information on "satin" weaves.
}
\keyword{datasets}
