## Functions for Chemometric Processing and Sample Selection of Spectroscopic Data

![](./reference/figures/logo.png)

*Last update: 2026-05-31*

Version: 0.2.9 – proxy

In science, one man’s noise is another man’s signal

## About

`prospectr` provides tools for signal processing and chemometrics, with
a focus on pre-processing and sample selection of spectral data. It is
increasingly used in spectroscopic applications, as reflected by the
growing number of scientific publications citing the package.

Although similar functions are available in other packages such as
[`signal`](https://CRAN.R-project.org/package=signal), many functions in
`prospectr` are designed to work consistently with `data.frame`,
`matrix`, and `vector` inputs. Several functions are optimised for speed
and rely on C++ code through the
[`Rcpp`](https://CRAN.R-project.org/package=Rcpp) and
[`RcppArmadillo`](https://CRAN.R-project.org/package=RcppArmadillo)
packages.

## Documentation

The package includes three vignettes covering all major functionality:

1.  **An introduction to the `prospectr` package**: Overview,
    installation, and how to cite the package.
2.  **Signal processing**: Pre-processing methods including smoothing,
    derivatives, scatter corrections, baseline removal, centering,
    scaling, resampling, and continuum removal.
3.  **Selecting representative calibration samples**: Algorithms for
    selecting representative calibration and validation subsets from
    spectral data.

## Core functionality

**Signal processing:**

- [`movav()`](https://l-ramirez-lopez.github.io/prospectr/reference/movav.md):
  moving average filter
- [`savitzkyGolay()`](https://l-ramirez-lopez.github.io/prospectr/reference/savitzkyGolay.md):
  Savitzky-Golay smoothing and derivatives
- [`gapDer()`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md):
  gap-segment derivative
- [`baseline()`](https://l-ramirez-lopez.github.io/prospectr/reference/baseline.md):
  baseline removal
- [`continuumRemoval()`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md):
  continuum-removed reflectance or absorbance
- [`detrend()`](https://l-ramirez-lopez.github.io/prospectr/reference/detrend.md):
  SNV-Detrend normalisation
- [`standardNormalVariate()`](https://l-ramirez-lopez.github.io/prospectr/reference/standardNormalVariate.md):
  Standard Normal Variate (SNV) transformation
- [`msc()`](https://l-ramirez-lopez.github.io/prospectr/reference/msc.md):
  Multiplicative Scatter Correction
- [`binning()`](https://l-ramirez-lopez.github.io/prospectr/reference/binning.md):
  average a signal in column bins
- [`resample()`](https://l-ramirez-lopez.github.io/prospectr/reference/resample.md):
  resample a signal to new band positions
- [`resample2()`](https://l-ramirez-lopez.github.io/prospectr/reference/resample2.md):
  resample a signal using FWHM values
- [`blockScale()`](https://l-ramirez-lopez.github.io/prospectr/reference/blockScale.md):
  block scaling
- [`blockNorm()`](https://l-ramirez-lopez.github.io/prospectr/reference/blockNorm.md):
  sum of squares block weighting

**Calibration sampling:**

- [`naes()`](https://l-ramirez-lopez.github.io/prospectr/reference/naes.md):
  k-means sampling
- [`kenStone()`](https://l-ramirez-lopez.github.io/prospectr/reference/kenStone.md):
  Kennard-Stone (CADEX) algorithm
- [`duplex()`](https://l-ramirez-lopez.github.io/prospectr/reference/duplex.md):
  DUPLEX algorithm
- [`shenkWest()`](https://l-ramirez-lopez.github.io/prospectr/reference/shenkWest.md):
  SELECT algorithm
- [`puchwein()`](https://l-ramirez-lopez.github.io/prospectr/reference/puchwein.md):
  Puchwein sampling
- [`honigs()`](https://l-ramirez-lopez.github.io/prospectr/reference/honigs.md):
  sample selection by spectral subtraction

**Other utilities:**

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  read binary files from BUCHI NIRCal software
- [`readASD()`](https://l-ramirez-lopez.github.io/prospectr/reference/readASD.md):
  read binary or ASCII files from ASD instruments
- [`spliceCorrection()`](https://l-ramirez-lopez.github.io/prospectr/reference/spliceCorrection.md):
  correct for detector splice steps in ASD FieldSpec Pro
- [`cochranTest()`](https://l-ramirez-lopez.github.io/prospectr/reference/cochranTest.md):
  detect replicate outliers with the Cochran *C* test

## Installation

Install from CRAN:

``` r

install.packages("prospectr")
```

Or install the development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("l-ramirez-lopez/prospectr")
```

The package requires a C++ compiler. On Windows, install
[Rtools](https://cran.r-project.org/bin/windows/Rtools/). On macOS, you
may need to install `gfortran` and `clang` from [CRAN
tools](https://cran.r-project.org/bin/macosx/tools/).

## Citing the package

``` r

citation(package = "prospectr")
```

## Contributing

Contributions are welcome! Please read our Contributing Guidelines
(available in the GitHub repo) before submitting pull requests.

This project follows a Code of Conduct available in the GitHub repo.

## Bug reports

Report issues at
[GitHub](https://github.com/l-ramirez-lopez/prospectr/issues) or contact
the maintainer (<ramirez.lopez.leo@gmail.com>).

## Related packages

- [`resemble`](https://github.com/l-ramirez-lopez/resemble):
  Memory-based learning and local modelling for spectral chemometrics.
