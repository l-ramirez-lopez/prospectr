# Hard or soft block scaling

Hard or soft block scaling of a spectral matrix to constant group
variance. In multivariate calibration, block scaling is used to
down-weight variables, when one block of variables dominates other
blocks. With hard block scaling, the variables in a block are scaled so
that the sum of their variances equals 1. When soft block scaling is
used, the variables are scaled such that the sum of variable variances
is equal to the square root of the number of variables in a particular
block.

## Usage

``` r
blockScale(X, type = 'hard', sigma2 = 1)
```

## Arguments

- X:

  a numeric matrix or vector to process (optionally a data frame that
  can be coerced to a numerical matrix).

- type:

  the type of block scaling: 'hard' or 'soft'.

- sigma2:

  the desired total variance of a block (ie sum of the variances of all
  variables, default = 1), applicable when `type = 'hard'`.

## Value

a `list` with `Xscaled`, the scaled matrix and `f`, the scaling factor.

## References

Eriksson, L., Johansson, E., Kettaneh, N., Trygg, J., Wikstrom, C., and
Wold, S., 2006. Multi- and Megavariate Data Analysis. MKS Umetrics AB.

## See also

[`blockNorm`](https://l-ramirez-lopez.github.io/prospectr/reference/blockNorm.md),
[`standardNormalVariate`](https://l-ramirez-lopez.github.io/prospectr/reference/standardNormalVariate.md),
[`detrend`](https://l-ramirez-lopez.github.io/prospectr/reference/detrend.md)

## Author

Antoine Stevens

## Examples

``` r
X <- matrix(rnorm(100), ncol = 10)
# Hard block scaling
res <- blockScale(X)
# sum of column variances == 1
apply(res$Xscaled, 2, var)
#>  [1] 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1
```
