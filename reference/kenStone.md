# Kennard-Stone algorithm for calibration sampling

Select calibration samples from a large multivariate data using the
Kennard-Stone algorithm

## Usage

``` r
kenStone(X, k, metric = "mahal", pc, group,
         .center = TRUE, .scale = FALSE, init = NULL)
```

## Arguments

- X:

  a numeric matrix.

- k:

  number of calibration samples to be selected.

- metric:

  distance metric to be used: 'euclid' (Euclidean distance) or 'mahal'
  (Mahalanobis distance, default).

- pc:

  optional. If not specified, distance are computed in the Euclidean
  space. Alternatively, distance are computed in the principal component
  score space and `pc` is the number of principal components retained.
  If `pc < 1`, the number of principal components kept corresponds to
  the number of components explaining at least (`pc * 100`) percent of
  the total variance.

- group:

  An optional `factor` (or vector that can be coerced to a factor by
  [`as.factor`](https://rdrr.io/r/base/factor.html)) of length equal to
  `nrow(X)`, giving the identifier of related observations (e.g. samples
  of the same batch of measurements, samples of the same origin, or of
  the same soil profile). Note that by using this option in some cases,
  the number of samples retrieved is not exactly the one specified in
  `k` as it will depend on the groups. See details.

- .center:

  logical value indicating whether the input matrix should be centered
  before Principal Component Analysis. Default set to `TRUE`.

- .scale:

  logical value indicating whether the input matrix should be scaled
  before Principal Component Analysis. Default set to `FALSE`.

- init:

  (optional) a vector of integers indicating the indices of the
  observations/rows that are to be used as observations that must be
  included at the first iteration of the search process. Default is
  `NULL`, i.e. no fixed initialization. The function will take by
  default the two most distant observations. If the `group` argument is
  used, then all the observations in the groups covered by the `init`
  observations will be also included in the `init` subset.

## Value

a list with the following components:

- `model`: numeric vector giving the row indices of the input data
  selected for calibration

- `test`: numeric vector giving the row indices of the remaining
  observations

- `pc`: if the `pc` argument is specified, a numeric matrix of the
  scaled pc scores

## Details

The Kennard–Stone algorithm allows to select samples with a uniform
distribution over the predictor space (Kennard and Stone, 1969). It
starts by selecting the pair of points that are the farthest apart. They
are assigned to the calibration set and removed from the list of points.
Then, the procedure assigns remaining points to the calibration set by
computing the distance between each unassigned points \\i_0\\ and
selected points \\i\\ and finding the point for which:

\\d\_{selected} = \max\limits\_{i_0}(\min\limits\_{i}(d\_{i,i\_{0}}))\\

This essentially selects point \\i_0\\ which is the farthest apart from
its closest neighbors \\i\\ in the calibration set. The algorithm uses
the Euclidean distance to select the points. However, the Mahalanobis
distance can also be used. This can be achieved by performing a PCA on
the input data and computing the Euclidean distance on the truncated
score matrix according to the following definition of the Mahalanobis
\\H\\ distance:

\\H\_{ij}^2 = \sum\_{a=1}^A (\hat t\_{ia} - \hat t\_{ja})^{2} / \hat
\lambda_a\\

where \\\hat t\_{ia}\\ is the \\a^{th}\\ principal component score of
point \\i\\, \\\hat t\_{ja}\\ is the corresponding value for point
\\j\\, \\\hat \lambda_a\\ is the eigenvalue of principal component \\a\\
and \\A\\ is the number of principal components included in the
computation.

When the `group` argument is used, the sampling is conducted in such a
way that at each iteration, when a single sample is selected, this
sample along with all the samples that belong to its group, are assigned
to the final calibration set. In this respect, at each iteration, the
algorithm will select one sample (in case that sample is the only one in
that group) or more to the calibration set. This also implies that the
argument `k` passed to the function will not necessary reflect the exact
number of samples selected. For example, if `k = 2` and if the first
sample identified belongs to with group of 5 samples and the second one
belongs to a group with 10 samples, then, the total amount of samples
retrieved by the function will be 15.

## References

Kennard, R.W., and Stone, L.A., 1969. Computer aided design of
experiments. Technometrics 11, 137-148.

## See also

[`duplex`](https://l-ramirez-lopez.github.io/prospectr/reference/duplex.md),
[`shenkWest`](https://l-ramirez-lopez.github.io/prospectr/reference/shenkWest.md),
[`naes`](https://l-ramirez-lopez.github.io/prospectr/reference/naes.md),
[`honigs`](https://l-ramirez-lopez.github.io/prospectr/reference/honigs.md)

## Author

Antoine Stevens & [Leonardo
Ramirez-Lopez](https://orcid.org/0000-0002-5369-5120) with contributions
from Thorsten Behrens and Philipp Baumann

## Examples

``` r
if (FALSE) { # \dontrun{
data(NIRsoil)
sel <- kenStone(NIRsoil$spc, k = 30, pc = .99)
plot(sel$pc[, 1:2], xlab = "PC1", ylab = "PC2")
# points selected for calibration
points(sel$pc[sel$model, 1:2], pch = 19, col = 2)
# Test on artificial data
X <- expand.grid(1:20, 1:20) + rnorm(1e5, 0, .1)
plot(X, xlab = "VAR1", ylab = "VAR2")
sel <- kenStone(X, k = 25, metric = "euclid")
points(X[sel$model, ], pch = 19, col = 2)

# Using the group argument
library(prospectr)

# create groups
set.seed(1)
my_groups <- sample(1:275, nrow(NIRsoil$spc), replace = TRUE) 
my_groups <- as.factor(my_groups)

# check the group size 
table(my_groups)

results_group <- kenStone(X = NIRsoil$spc, k = 2, pc = 3, group = my_groups)

# as the first two samples selected belong to groups
# which have in total more than 2 samples (k).
table(factor(my_groups[results_group$model]))
} # }
```
