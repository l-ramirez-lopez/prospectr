

# `prospectr`

## Functions for Chemometric Processing and Sample Selection of Spectroscopic Data

<!-- badges: start -->

![R-CMD-check](https://github.com/l-ramirez-lopez/prospectr/actions/workflows/R-CMD-check.yaml/badge.svg)
[![codecov](https://codecov.io/github/l-ramirez-lopez/prospectr/graph/badge.svg)](https://app.codecov.io/gh/l-ramirez-lopez/prospectr)
[![CRAN
status](https://www.r-pkg.org/badges/version/prospectr?v=2.png)](https://CRAN.R-project.org/package=prospectr)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/prospectr?v=2.png)](https://CRAN.R-project.org/package=prospectr)  
<!-- badges: end -->

<img align="right" src="./man/figures/logo.png" width="15%">

*Last update: 2026-05-18*

Version: 0.2.9 – proxy

<em>
<p align="right">

In science, one man’s noise is another man’s signal
</p>

</em>

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

- `movav()`: moving average filter
- `savitzkyGolay()`: Savitzky-Golay smoothing and derivatives
- `gapDer()`: gap-segment derivative
- `baseline()`: baseline removal
- `continuumRemoval()`: continuum-removed reflectance or absorbance
- `detrend()`: SNV-Detrend normalisation
- `standardNormalVariate()`: Standard Normal Variate (SNV)
  transformation
- `msc()`: Multiplicative Scatter Correction
- `binning()`: average a signal in column bins
- `resample()`: resample a signal to new band positions
- `resample2()`: resample a signal using FWHM values
- `blockScale()`: block scaling
- `blockNorm()`: sum of squares block weighting

**Calibration sampling:**

- `naes()`: k-means sampling
- `kenStone()`: Kennard-Stone (CADEX) algorithm
- `duplex()`: DUPLEX algorithm
- `shenkWest()`: SELECT algorithm
- `puchwein()`: Puchwein sampling
- `honigs()`: sample selection by spectral subtraction

**Other utilities:**

- `read_nircal()`: read binary files from BUCHI NIRCal software
- `readASD()`: read binary or ASCII files from ASD instruments
- `spliceCorrection()`: correct for detector splice steps in ASD
  FieldSpec Pro
- `cochranTest()`: detect replicate outliers with the Cochran *C* test

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

## Bug reports

Report issues at
[GitHub](https://github.com/l-ramirez-lopez/prospectr/issues) or contact
the maintainer (<ramirez.lopez.leo@gmail.com>).

## Related packages

- [`resemble`](https://github.com/l-ramirez-lopez/resemble):
  Memory-based learning and local modelling for spectral chemometrics.
