# prospectr

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/prospectr)](https://CRAN.R-project.org/package=prospectr)
[![Downloads](https://cranlogs.r-pkg.org/badges/prospectr)](https://cranlogs.r-pkg.org/badges/prospectr)

<img align="right" src="./man/figures/logo.png" width="15%">

<!-- badges: end -->
<em>
<p align="left">
Misc. Functions for Processing and Sample Selection of Spectroscopic
Data
</p>

</em> *Antoine Stevens & Leo Ramirez-Lopez*

*Last update: 2025-03-05*

Version: 0.2.8 – galo

`prospectr` is becoming more and more used in spectroscopic
applications, which is evidenced by the number of scientific
publications citing the package. This package is very useful for signal
processing and chemometrics in general as it provides various utilities
for pre–processing and sample selection of spectral data. While similar
functions are available in other packages, like
[`signal`](https://CRAN.R-project.org/package=signal), the functions in
this package works indifferently for `data.frame`, `matrix` and `vector`
inputs. Besides, several functions are optimized for speed and use C++
code through the [`Rcpp`](https://CRAN.R-project.org/package=Rcpp) and
[`RcppArmadillo`](https://CRAN.R-project.org/package=RcppArmadillo)
packages.

## Installing it from GitHub

Install this package from github by:

    remotes::install_github("l-ramirez-lopez/prospectr")

NOTE: in some MAC Os it is still recommended to install `gfortran` and
`clang` from [here](https://cran.r-project.org/bin/macosx/tools/). Even
for R &gt;= 4.0. For more info, check this
[issue](https://github.com/tidyverts/fable/issues/193).

## News

Check the NEWS document for new functionality and general changes in the
package.

## Vignette

A vignette for `prospectr` explaining its core functionality is
available at
<https://CRAN.R-project.org/package=prospectr/vignettes/prospectr.html>.

## Core functionality

A vignette gives an overview of the main functions of the package. Just
type `vignette("prospectr-intro")` in the console to access it.
Currently, the following preprocessing functions are available:

-   `resample()` : resample a signal to new coordinates by linear or
    spline interpolation

-   `resample2()` : resample a signal to new coordinates using FWHM
    values

-   `movav()` : moving average

-   `standardNormalVariate()` : standard normal variate

-   `msc()` : multiplicative scatter correction

-   `detrend()` : detrend normalization

-   `baseline()` : baseline removal/correction

-   `blockScale()` : block scaling

-   `blockNorm()` : sum of squares block weighting

-   `binning()` : average in column–wise subsets

-   `savitzkyGolay()` : Savitzky-Golay filter (smoothing and
    derivatives)

-   `gapDer()` : gap-segment derivative

-   `continuumRemoval()` : continuum-removed absorbance or reflectance
    values

The selection of representative samples/observations for calibration of
spectral models can be achieved with one of the following functions:

-   `naes()` : k-means sampling

-   `kenStone()` : CADEX (Kennard–Stone) algorithm

-   `duplex()` : DUPLEX algorithm

-   `shenkWest()` : SELECT algorithm

-   `puchwein()` : Puchwein sampling

-   `honigs()` : Unique-sample selection by spectral subtraction

Other useful functions are also available:

-   `read_nircal()` : read binary files exported from BUCHI NIRCal
    software

-   `readASD()` : read binary or text files from an ASD instrument
    (Indico Pro format)

-   `spliceCorrection()` : correct spectra for steps at the splice of
    detectors in an ASD FieldSpec Pro

-   `cochranTest()` : detects replicate outliers with the Cochran *C*
    test

## Citing the package

Antoine Stevens and Leornardo Ramirez-Lopez (2025). An introduction to
the prospectr package. R package Vignette R package version 0.2.8. A
BibTeX entry for LaTeX users is:

     @Manual{stevens2022prospectr,
        title = {An introduction to the prospectr package},
        author = {Antoine Stevens and Leornardo Ramirez-Lopez},
        publication = {R package Vignette},
        year = {2025},
        note = {R package version 0.2.8},
      }

## Bug report and development version

You can send an email to the package maintainer
(<ramirez.lopez.leo@gmail.com>) or create an
[issue](https://github.com/l-ramirez-lopez/prospectr/issues) on github.
To install the development version of `prospectr`, simply install
[`devtools`](https://CRAN.R-project.org/package=devtools) from CRAN then
run `install_github("l-ramirez-lopez/prospectr")`.
