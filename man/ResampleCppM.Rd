\name{ResampleCppM}
\alias{ResampleCppM}
\title{Resample to given band position and fwhm}
\usage{
  ResampleCppM(X, wav, new_wav, fwhm)
}
\arguments{
  \item{X}{matrix to resample}

  \item{wav}{a numeric \code{vector} giving the original
  band positions}

  \item{new_wav}{a numeric \code{vector} giving the new
  band positions}

  \item{fwhm}{numeric \code{vector} giving the full width
  half maximums of the new band positions.}
}
\description{
  Resample, written in C++
}
\keyword{internal}

