# Sum of squares block weighting

Sum of squares block weighting: allows to scale blocks of variables, but
keeping the relative weights of the variables inside a block.

## Usage

``` r
blockNorm(X, targetnorm = 1)
```

## Arguments

- X:

  a numeric matrix to transform (optionally a data frame that can be
  coerced to a numerical matrix).

- targetnorm:

  desired sum of squares for a block of variables (default = 1)

## Value

a list with components `Xscaled`, the scaled matrix and `f`, the scaling
factor

## Details

The function computes a scaling factor, which, multiplied by the input
matrix, produces a matrix with a pre–determined sum of squares.

## Note

This is a R port of the `MBnorm.m` function of the MB matlab toolbox by
Fran van den Berg.

## References

Eriksson, L., Johansson, E., Kettaneh, N., Trygg, J., Wikstrom, C., and
Wold, S., 2006. Multi- and Megavariate Data Analysis. MKS Umetrics AB.

## See also

[`blockScale`](https://l-ramirez-lopez.github.io/prospectr/reference/blockScale.md),
[`standardNormalVariate`](https://l-ramirez-lopez.github.io/prospectr/reference/standardNormalVariate.md),
[`detrend`](https://l-ramirez-lopez.github.io/prospectr/reference/detrend.md)

## Author

Antoine Stevens

## Examples

``` r
X <- matrix(rnorm(100), ncol = 10)
# Block normalize to sum of square equals to 1
res <- blockNorm(X, targetnorm = 1)
sum(res$Xscaled^2) # check
#> [1] 1
```
