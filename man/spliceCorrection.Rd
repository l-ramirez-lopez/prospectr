\name{spliceCorrection}
\alias{spliceCorrection}
\title{Splice correction of a spectral matrix acquired with an ASD spectrometer}
\usage{
spliceCorrection(X,wav,splice=c(1000,1830),interpol.bands=10)
}
\arguments{
  \item{X}{numeric \code{data.frame}, \code{matrix} or
  \code{vector} to transform}

  \item{wav}{numeric \code{vector} with band positions}

  \item{splice}{numeric \code{vector} of the two positions
  of the splices, default = c(1000,1830) corresponding to
  the splices of the ASD FieldSpec Pro spectrometer.}

  \item{interpol.bands}{number of interpolation bands}
}
\value{
a \code{matrix} with the splice corrected data
}
\description{
Corrects steps in an input spectral matrix by linear
interpolation of the values of the edges of the middle
sensor
}
\details{
Spectra acquired with an ASD FieldSpec Pro
spectroradiometer usually exhibit steps at the splice of
the three built-in sensors, positioned at 1000 and 1830 nm.
}
\author{
Antoine Stevens
}

