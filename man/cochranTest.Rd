\name{cochranTest}
\alias{cochranTest}
\title{Cochran \emph{C} Test}
\usage{
  cochranTest(X,id,fun='sum',alpha=0.05)
}
\arguments{
  \item{X}{input \code{data.frame} or \code{matrix}}

  \item{id}{\code{factor} of the replicate identifiers}

  \item{fun}{function to aggregate data: 'sum' (default),
  'mean', 'PC1' or 'PC2'}

  \item{alpha}{\emph{p}-value of the Cochran \emph{C} test}
}
\value{
  a \code{list} with components: \itemize{
  \item{'\code{X}'}{ input \code{matrix} from which
  outlying observations (rows) have been removed}
  \item{'\code{outliers}'}{ numeric \code{vector} giving
  the row indices of the input data that have been flagged
  as outliers} }
}
\description{
  Detects and removes replicate outliers in data series
  based on the Cochran \emph{C} test for homogeneity in
  variance.
}
\details{
  The Cochran \emph{C} test is test whether a single
  estimate of variance is significantly larger than a a
  group of variances. It can be computed as: \deqn{RMSD =
  \sqrt{\frac{1}{n} \sum_{i=1}^n {(y_i - \ddot{y}_i)^2}}}
  where \eqn{y_i} is the value of the side variable of the
  \eqn{i}th sample, \eqn{\ddot{y}_i} is the value of the
  side variable of the nearest neighbor of the \eqn{i}th
  sample and \eqn{n} is the total number of observations

  For multivariate data, the variance \eqn{S_i^2} can be
  computed on aggregated data, using a summary function
  (\code{fun} argument) such as \code{sum}, \code{mean}, or
  first principal components ('PC1' and 'PC2').

  An observation is considered to have an outlying variance
  if the Cochran \emph{C} statistic is higher than an upper
  limit critical value \eqn{C_{UL}} which can be evaluated
  with ('t Lam, 2010):

  \deqn{ C_{UL}(\alpha,n,N) = \left
  [1+\frac{N-1}{F_{c}(\alpha/N,(n-1),(N-1)(n-1))} \right
  ]^{-1}}

  where \eqn{\alpha} is the \emph{p}-value of the test,
  \eqn{n} is the (average) number of replicates and
  \eqn{F_c} is the critical value of the Fisher's \eqn{F}
  ratio.

  The replicates with outlying variance are removed and the
  test can be applied iteratively until no outlying
  variance is detected under the given \emph{p}-value. Such
  iterative procedure is implemented in \code{cochranTest},
  allowing the user to specify whether a set of replicates
  should be removed or not from the dataset by graphical
  inspection of the outlying replicates. The user has then
  the possibility to (i) remove all replicates at once,
  (ii) remove one or more replicates by giving their
  indices or (iii) remove nothing.
}
\note{
  The test assumes a balanced design (i.e. data series have
  the same number of replicates).
}
\author{
  Antoine Stevens
}
\references{
  Centner, V., Massart, D.L., and De Noord, O.E., 1996.
  Detection of inhomogeneities in sets of NIR spectra.
  Analytica Chimica Acta 330, 1-17.

  R.U.E. 't Lam (2010). Scrutiny of variance results for
  outliers: Cochran's test optimized. Analytica Chimica
  Acta 659, 68-84.

  \url{http://en.wikipedia.org/wiki/Cochran's_C_test}
}

