\name{continuumRemoval}
\alias{continuumRemoval}
\title{Continuum Removal}
\usage{
  continuumRemoval(X,wav,type,interpol,method,parallel=FALSE)
}
\arguments{
  \item{X}{numeric \code{data.frame}, \code{matrix} or
  \code{vector} to process}

  \item{wav}{optional. numeric \code{vector} of band
  positions}

  \item{type}{type of data: 'R' for reflectance (default),
  'A' for absorbance}

  \item{interpol}{interpolation method between points on
  the convex hull: 'linear' (default) or 'spline'}

  \item{method}{normalization method: 'division' (default)
  or 'substraction' (see details section)}

  \item{parallel}{logical value. if \code{TRUE}, apply the
  function in parallel using the parallel backend provided
  by \code{\link{foreach}}}
}
\value{
  a \code{matrix} or \code{vector} with the filtered
  signal(s)
}
\description{
  Compute the continuum removed values of a data
  \code{matrix}, \code{data.frame}, or \code{vector} as
  implemented in ENVI
}
\details{
  The continuum removal technique was introduced by Clark
  and Roush (1984) as a method to highlight energy
  absorption features of minerals. It can be viewed as a
  way to perform albedo normalization. The algorithm find
  points lying on the convex hull (local maxima or
  envelope) of a spectrum, connects the points by linear or
  spline interpolation and normalizes the spectrum by
  dividing (or substracting) the input data by the
  interpolated line.
}
\examples{
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))
# plot of the 10 first abs spectra
matplot(wav,t(NIRsoil$spc[1:10,]),type="l",ylim=c(0,.6),xlab="Wavelength /nm",ylab="Abs")
# type = 'A' is used for absorbance spectra
cr <- continuumRemoval(NIRsoil$spc,wav,type="A")
matlines(wav,t(cr[1:10,]))
}
\author{
  Antoine Stevens
}
\references{
  Clark, R.N., and Roush, T.L., 1984. Reflectance
  Spectroscopy: Quantitative Analysis Techniques for Remote
  Sensing Applications. J. Geophys. Res. 89, 6329-6340.
}
\seealso{
  \code{\link[signal]{sgolayfilt}},
  \code{\link{savitzkyGolay}}, \code{\link{movav}},
  \code{\link{gapDer}}, \code{\link{binning}}
}

