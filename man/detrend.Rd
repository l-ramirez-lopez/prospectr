\name{detrend}
\alias{detrend}
\title{Detrend transformation}
\usage{
detrend(X,wav)
}
\arguments{
  \item{X}{numeric \code{data.frame}, \code{matrix} or
  \code{vector} to process}

  \item{wav}{wavelengths/ band centers}
}
\value{
a \code{matrix} or \code{vector} with detrend values
}
\description{
Normalizes each row of an input \code{data.frame} or
\code{matrix} by applying a SNV transformation followed by
fitting a second order linear model and returning the
fitted residuals.
}
\details{
The detrend is a row-wise transformation that allows to
correct for wavelength-dependent scattering effects
(variations in curvilinearity). A second-degree polynomial
is fit through each spectrum: \deqn{x_i = a\lambda^2 +
b\lambda + c + e_i} were \eqn{x_i} is the spectrum,
\eqn{\lambda} is the wavelength vector, a, b, c are
estimated by least square, and \eqn{e_i} are the residuals
of the least square fit. Then, a detrend spectrum
corresponds to: \deqn{x\ast_i = x_i - (a\lambda^2 +
b\lambda + c = e_i)}
}
\examples{
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))
spc <- 1/10^NIRsoil$spc # conversion to reflectance
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1),mar=c(4,4,2,2))
matplot(wav,t(spc[1:10,]),type='l',xlab='',ylab='Reflectance') # plot of the 10 first spectra
mtext('Raw spectra')
det <- detrend(spc,wav)
matplot(wav,t(det[1:10,]),type='l',xlab='Wavelength /nm',ylab='Reflectance')
mtext('Detrend spectra')
par(opar)
}
\author{
Antoine Stevens
}
\references{
Barnes RJ, Dhanoa MS, Lister SJ. 1989. Standard normal
variate transformation and de-trending of near-infrared
diffuse reflectance spectra. Applied spectroscopy, 43(5):
772-777.
}
\seealso{
\code{\link{standardNormalVariate}},
\code{\link{blockScale}}, \code{\link{blockNorm}}
}

