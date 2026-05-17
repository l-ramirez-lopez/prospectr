# Moving average

A simple moving average of a matrix or vector using a convolution
function written in C++/Rcpp for fast computing

## Usage

``` r
movav(X, w)
```

## Arguments

- X:

  a numeric matrix or vector to process (optionally a data frame that
  can be coerced to a numerical matrix).

- w:

  filter length.

## Value

a matrix or vector with the filtered signal(s)

## See also

[`savitzkyGolay`](https://l-ramirez-lopez.github.io/prospectr/reference/savitzkyGolay.md),
[`gapDer`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md),
[`binning`](https://l-ramirez-lopez.github.io/prospectr/reference/binning.md),
[`continuumRemoval`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md)

## Author

Antoine Stevens

## Examples

``` r
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))
# adding some noise
NIRsoil$spc_noise <- NIRsoil$spc + rnorm(length(NIRsoil$spc), 0, 0.001)
matplot(wav,
  t(NIRsoil$spc_noise[1:10, ]),
  type = "l",
  lty = 1,
  xlab = "Wavelength /nm",
  ylab = "Absorbance",
  col = "grey"
)

# window size of 11 bands
NIRsoil$spc_mov <- movav(NIRsoil$spc_noise, w = 15)
# smoothed data
matlines(as.numeric(colnames(NIRsoil$spc_mov)),
  t(NIRsoil$spc_mov[1:10, ]),
  type = "l",
  lty = 1
)
```
