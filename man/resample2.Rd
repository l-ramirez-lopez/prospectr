\name{resample2}
\alias{resample2}
\title{Resample a high resolution signal to a low resolution signal using full width half maximum (FWHM) values}
\usage{
  resample2(X,wav,new.wav,fwhm)
}
\arguments{
  \item{X}{numeric \code{data.frame}, \code{matrix} or
  \code{vector} to resample}

  \item{wav}{a numeric \code{vector} giving the original
  band positions}

  \item{new.wav}{a numeric \code{vector} giving the new
  band positions}

  \item{fwhm}{numeric \code{vector} giving the full width
  half maximums of the new band positions. If no value is
  specified, it is assumed that the fwhm is equal to the
  sampling interval (i.e. band spacing). If only one value
  is specified, the fwhm is assumed to be constant over the
  spectral range}
}
\value{
  a \code{matrix} or \code{vector} with resampled values
}
\description{
  Resample a data \code{matrix}, \code{data.frame} or
  \code{vector} to match the response of another instrument
  using full width half maximum (FWHM) values
}
\details{
  The function uses gaussian models defined by fwhm values
  to resample the high resolution data to new band
  positions and resolution. It assumes that band spacing
  and fwhm of the input data is constant over the spectral
  range. The interpolated values are set to 0 if input data
  fall outside by 3 standard deviations of the gaussian
  densities defined by fwhm.
}
\examples{
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))
spc <- 1/10^NIRsoil$spc # conversion to reflectance
# Plot 10 first spectra
matplot(wav,t(spc[1:10,]),type="l",xlab="Wavelength /nm",ylab="Reflectance")
# ASTER SWIR bands (nm)
new.wav <- c(1650,2165,2205,2260,2330,2395) # positions
fwhm <- c(100,40,40,50,70,70) #  fwhm's
# Resample NIRsoil to ASTER band positions
aster <- resample2(spc,wav,new.wav,fwhm)
matpoints(as.numeric(colnames(aster)),t(aster[1:10,]),pch=1:5)
}
\author{
  Antoine Stevens
}
\seealso{
  \code{\link{resample}}
}

