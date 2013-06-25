\name{savitzkyGolay}
\alias{savitzkyGolay}
\title{Savitzky-Golay transformation}
\usage{
  savitzkyGolay(X,m,p,w,delta.wav)
}
\arguments{
  \item{X}{a numeric \code{data.frame}, \code{matrix} or
  \code{vector} to transform}

  \item{m}{differentiation order}

  \item{p}{polynomial order}

  \item{w}{window size (must be odd)}

  \item{delta.wav}{optional sampling interval}
}
\description{
  Savitzky-Golay smoothing and derivative of a data
  \code{matrix}, \code{data.frame} or \code{vector}.
}
\details{
  The Savitzky-Golay algorithm fits a local polynomial
  regression on the signal. It requires evenly spaced data
  points. Mathematically, it operates simply as a weighted
  sum over a given window: \deqn{ x_j\ast =
  \frac{1}{N}\sum_{h=-k}^{k}{c_hx_{j+h}}} where
  \eqn{x_j\ast} is the new value, \eqn{N} is a normalizing
  coefficient, \eqn{k} is the gap size on each side of
  \eqn{j} and \eqn{c_h} are pre-computed coefficients, that
  depends on the chosen polynomial order and degree.

  The sampling interval specified with the \code{delta.wav}
  argument is used for scaling and get numerically correct
  derivatives. The convolution function is written in
  C++/Rcpp for faster computations.
}
\examples{
data(NIRsoil)
spc <- 1/10^NIRsoil$spc # conversion to reflectance
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,1),mar=c(4,4,2,2))
# plot of the 10 first spectra
matplot(as.numeric(colnames(spc)),t(spc[1:10,]),type="l",xlab="",ylab="Reflectance")
mtext("Raw spectra")
sg <- savitzkyGolay(X = spc,1,3,11,delta.wav=2)
matplot(as.numeric(colnames(sg)),t(sg[1:10,]),type="l",xlab="Wavelength /nm",ylab="1st derivative")
mtext("1st derivative spectra")
par(opar)
}
\author{
  Antoine Stevens
}
\references{
  Savitzky, A., and Golay, M.J.E., 1964. Smoothing and
  differentiation of data by simplified least squares
  procedures. Anal. Chem. 36, 1627-1639.

  Wentzell, P.D., and Brown, C.D., 2000. Signal processing
  in analytical chemistry. Encyclopedia of Analytical
  Chemistry, 9764-9800.
}
\seealso{
  \code{\link[signal]{sgolayfilt}}
}

