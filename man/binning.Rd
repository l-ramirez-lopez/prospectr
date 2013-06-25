\name{binning}
\alias{binning}
\title{Signal binning}
\usage{
  binning(X,bins,bin.size)
}
\arguments{
  \item{X}{numeric \code{data.frame}, \code{matrix} or
  \code{vector} to process}

  \item{bins}{number of bins}

  \item{bin.size}{desired size of the bins}
}
\value{
  a \code{matrix} or \code{vector} with average values per
  bin
}
\description{
  Compute average values of a signal in pre-determined bins
  (col-wise subsets). The bin size can be determined either
  directly or by specifying the number of bins. Sometimes
  called boxcar transformation in signal processing
}
\examples{
data(NIRsoil)
spc <- 1/10^NIRsoil$spc # conversion to reflectance
wav <- as.numeric(colnames(spc))
matplot(wav,t(spc[1:5,]),type="l",xlab="Wavelength /nm",ylab="Reflectance") # 5 first spectra
binned <- binning(spc,bin.size=20)
matpoints(as.numeric(colnames(binned)),t(binned[1:5,]),pch=1:5) # bin means
binned <- binning(spc,bins=20)
dim(binned) # 20 bins
matplot(wav,t(spc[1:5,]),type="l",xlab="Wavelength /nm",ylab="Reflectance") # 5 first spectra
matpoints(as.numeric(colnames(binned)),t(binned[1:5,]),pch=1:5) # bin means
}
\author{
  Antoine Stevens & Leonardo Ramirez-Lopez
}
\seealso{
  \code{\link[signal]{sgolayfilt}},
  \code{\link{savitzkyGolay}}, \code{\link{movav}},
  \code{\link{gapDer}}, \code{\link{continuumRemoval}}
}

