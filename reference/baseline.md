# baseline

**maturing lifecycle**

Fits a baseline to each spectrum in a matrix and removes it from the
corresponding input spectrum. A vector can also be passed to this
function.

## Usage

``` r
baseline(X, wav)
```

## Arguments

- X:

  a numeric matrix or vector to process (optionally a data frame that
  can be coerced to a numerical matrix).

- wav:

  optional. A numeric vector of band positions.

## Value

a matrix or vector with the baselined spectra. The resulting matrix is
output with an attribute called `baselines` which contain the spectra of
the fitted baselines.

This function is similar to
[`continuumRemoval`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md)
and it might replace some of its functionality in the future.

## Details

The baseline function find points lying on the convex hull of a
spectrum, connects the points by linear interpolation and subtracts the
interpolated line (baseline) from the corresponding spectrum.

## See also

[`savitzkyGolay`](https://l-ramirez-lopez.github.io/prospectr/reference/savitzkyGolay.md),
[`movav`](https://l-ramirez-lopez.github.io/prospectr/reference/movav.md),
[`gapDer`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md),
[`binning`](https://l-ramirez-lopez.github.io/prospectr/reference/binning.md),
[`continuumRemoval`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md)

## Author

[Leonardo Ramirez-Lopez](https://orcid.org/0000-0002-5369-5120) with
contributions from Mervin Manalili

## Examples

``` r
data(NIRsoil)
wav <- as.numeric(colnames(NIRsoil$spc))
# plot of the 5 first absorbance spectra
matplot(wav,
  t(NIRsoil$spc[1:5, ]),
  type = "l",
  ylim = c(0, .6),
  xlab = "Wavelength /nm",
  ylab = "Absorbance"
)

bs <- baseline(NIRsoil$spc, wav)
matlines(wav, t(bs[1:5, ]))

fitted_baselines <- attr(bs, "baselines")
matlines(wav, t(fitted_baselines[1:5, ]))
title("Original spectra, baselines and baselined spectra")
```
