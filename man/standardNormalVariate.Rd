\name{standardNormalVariate}
\alias{standardNormalVariate}
\title{Standard normal variate transformation}
\usage{
  standardNormalVariate(X)
}
\arguments{
  \item{X}{numeric \code{data.frame} or \code{matrix} to
  transform}
}
\value{
  a \code{matrix} of the transformed data
}
\description{
  \code{standardNormalVariate} normalizes each row of an
  input \code{data.frame} or \code{matrix} by substracting
  each row by its mean and dividing by its standard
  deviation
}
\details{
  SNV is simple way for normalizing spectral data that
  intends to correct for light scatter. It operates
  row-wise: \deqn{SNV_i = \frac{x_i - \bar{x_i}}{s_i}}
  where \eqn{x_i} is the signal of a sample \eqn{i},
  \eqn{\bar{x_i}} is its mean and \eqn{s_i} its standard
  deviation
}
\examples{
data(NIRsoil)
spc <- 1/10^NIRsoil$spc # conversion to reflectance
snv <- standardNormalVariate(X = spc)
# 10 first snv spectra
matplot(as.numeric(colnames(snv)),t(snv[1:10,]),type="l",xlab="wavelength /nm",ylab="snv")
\dontrun{
apply(snv,1,sd) # check
}
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
  \code{\link{detrend}}, \code{\link{blockScale}},
  \code{\link{blockNorm}}, \code{\link{msc}}
}

