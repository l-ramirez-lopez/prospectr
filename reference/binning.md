# Signal binning

Compute average values of a signal in pre-determined bins (col-wise
subsets). The bin size can be determined either directly or by
specifying the number of bins. Sometimes called boxcar transformation in
signal processing

## Usage

``` r
binning(X, bins, bin.size)
```

## Arguments

- X:

  a numeric matrix or vector to process (optionally a data frame that
  can be coerced to a numerical matrix).

- bins:

  the number of bins.

- bin.size:

  the desired size of the bins.

## Value

a matrix or vector with average values per bin.

## See also

[`savitzkyGolay`](https://l-ramirez-lopez.github.io/prospectr/reference/savitzkyGolay.md),
[`movav`](https://l-ramirez-lopez.github.io/prospectr/reference/movav.md),
[`gapDer`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md),
[`continuumRemoval`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md)

## Author

Antoine Stevens & [Leonardo
Ramirez-Lopez](https://orcid.org/0000-0002-5369-5120)

## Examples

``` r
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))

# 5 first spectra
matplot(wav, t(NIRsoil$spc[1:5, ]),
  type = "l",
  xlab = "Wavelength /nm",
  ylab = "Absorbance"
)

NIRsoil$spc_binned <- binning(NIRsoil$spc, bin.size = 20)

# bin means
matpoints(as.numeric(colnames(NIRsoil$spc_binned)),
  t(NIRsoil$spc_binned[1:5, ]),
  pch = 1:5
)


NIRsoil$spc_binned <- binning(NIRsoil$spc, bins = 20)
dim(NIRsoil$spc_binned) # 20 bins
#> [1] 825  20

# 5 first spectra
matplot(wav,
  t(NIRsoil$spc[1:5, ]),
  type = "l",
  xlab = "Wavelength /nm",
  ylab = "Absorbance"
)

# bin means
matpoints(as.numeric(colnames(NIRsoil$spc_binned)),
  t(NIRsoil$spc_binned[1:5, ]),
  pch = 1:5
)
```
