% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blockNorm.R
\name{blockNorm}
\alias{blockNorm}
\title{Sum of squares block weighting}
\usage{
blockNorm(X, targetnorm = 1)
}
\arguments{
\item{X}{a numeric matrix to transform (optionally a data frame that can
be coerced to a numerical matrix).}

\item{targetnorm}{desired sum of squares for a block of variables
(default = 1)}
}
\value{
a list with components \code{Xscaled}, the scaled matrix and \code{f}, the
scaling factor
}
\description{
Sum of squares block weighting: allows to scale blocks of variables,
but keeping the relative weights of the variables inside a block.
}
\details{
The function computes a scaling factor, which, multiplied by the
input matrix,
produces a matrix with a pre--determined sum of squares.
}
\note{
This is a \R port of the \file{MBnorm.m} function of the MB matlab toolbox
by Fran van den Berg.
}
\examples{
X <- matrix(rnorm(100), ncol = 10)
# Block normalize to sum of square equals to 1
res <- blockNorm(X, targetnorm = 1)
sum(res$Xscaled^2) # check
}
\references{
Eriksson, L., Johansson, E., Kettaneh, N., Trygg, J.,
Wikstrom, C., and Wold, S., 2006. Multi- and Megavariate Data Analysis.
MKS Umetrics AB.
}
\seealso{
\code{\link{blockScale}}, \code{\link{standardNormalVariate}},
\code{\link{detrend}}
}
\author{
Antoine Stevens
}
