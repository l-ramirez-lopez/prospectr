# Resample spectral data

Resample a data matrix or vector to new coordinates (e.g. band
positions) using spline or linear interpolation. This function is a
simple wrapper around [`approx`](https://rdrr.io/r/stats/approxfun.html)
and [`splinefun`](https://rdrr.io/r/stats/splinefun.html) in base.

## Usage

``` r
resample(X, wav, new.wav, interpol = "spline", ...)
```

## Arguments

- X:

  numeric matrix or vector to resample (optionally a data frame that can
  be coerced to a numerical matrix).

- wav:

  a numeric vector giving the original band positions.

- new.wav:

  a numeric vector giving the new band positions.

- interpol:

  the interpolation method: 'linear' or 'spline' (default).

- ...:

  additional arguments to be passed to the
  [`splinefun`](https://rdrr.io/r/stats/splinefun.html) function when
  `interpol = 'spline'`.

## Value

a matrix or vector with resampled values.

## See also

[`resample2`](https://l-ramirez-lopez.github.io/prospectr/reference/resample2.md)

## Author

Antoine Stevens and [Leonardo
Ramirez-Lopez](https://orcid.org/0000-0002-5369-5120)

## Examples

``` r
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))
# increase spectral resolution by 2
NIRsoil$spc_resampled <- resample(NIRsoil$spc, wav, seq(1100, 2498, 2))
dim(NIRsoil$spc)
#> [1] 825 700
dim(NIRsoil$spc_resampled)
#> [1] 825 700
```
