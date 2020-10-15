# prospectr

[![Travis-CI Build Status](https://travis-ci.org/l-ramirez-lopez/prospectr.svg?branch=master)](https://travis-ci.org/l-ramirez-lopez/prospectr/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/prospectr)](https://CRAN.R-project.org/package=prospectr)
[![Downloads](https://cranlogs.r-pkg.org/badges/prospectr)](https://cranlogs.r-pkg.org/badges/prospectr)

<img align="right" src="./man/figures/logo.png" width="15%">

<!-- badges: end -->

<em><p align="left"> Misc. Functions for Processing and Sample Selection of Spectroscopic Data </p></em>
_Antoine Stevens & Leo Ramirez-Lopez_

_Last update: 15.10.2020_

`prospectr` is becoming more and more used in spectroscopic applications, which 
is evidenced by the number of scientific publications citing the package. 
This package is very useful for singal processing and chemometrics in general as 
it provides various utilities for pre--processing and sample selection 
of spectral data. While similar functions are available in other packages, like 
[`signal`](https://CRAN.R-project.org/package=signal), the 
functions in this package works indifferently for `data.frame`, `matrix` and 
`vector` inputs. Besides, several functions are optimized for speed and use 
C++ code through the [`Rcpp`](https://CRAN.R-project.org/package=Rcpp) 
and [`RcppArmadillo`](https://CRAN.R-project.org/package=RcppArmadillo)
packages.

Check the NEWS document for new functionality and general changes in the package.

## Core functionality
A vignette gives an overview of the main functions of the package. Just
type `vignette("prospectr-intro")` in the console to access it. Currently, the 
following preprocessing functions are available:

 `resample`              : resample a signal to new coordinates by linear or spline interpolation   
 `resample2`             : resample a signal to new coordinates using FWHM values                 
 `movav`                 : moving average                                                         
 `standardNormalVariate` : standard normal variate      
 `msc`                   : multiplicative scatter correction                                        
 `detrend`               : detrend normalization                                                  
 `blockScale`            : block scaling                                                           
 `blockNorm`             : sum of squares block weighting                                         
 `binning`               : average in column--wise subsets                                        
 `savitzkyGolay`         : Savitzky-Golay filter (smoothing and derivatives)                      
 `gapDer`                : gap-segment derivative                                                 
 `continuumRemoval`      : continuum-removed absorbance or reflectance values                     

The selection of representative samples/observations for calibration of spectral 
models can be achieved with one of the following functions:

 `naes`      : k-means sampling    
 `kenStone`  : CADEX (Kennard--Stone) algorithm                
 `duplex`    : DUPLEX algorithm                                
 `shenkWest` : SELECT algorithm                                
 `puchwein`  : Puchwein sampling                               
 `honigs`    : Unique-sample selection by spectral subtraction 

Other useful functions are also available:

 `read_nircal`      : read binary files exported from BUCHI NIRCal software  
 `readASD`          : read binary or text files from an ASD instrument (Indico Pro format)         
 `spliceCorrection` : correct spectra for steps at the splice of detectors in an ASD FieldSpec Pro  
 `cochranTest`      : detects replicate outliers with the Cochran _C_ test                         

## Citing the package

  Antoine Stevens and Leornardo Ramirez-Lopez (2020). An introduction to the prospectr package. R package
  Vignette R package version 0.2.1.

A BibTeX entry for LaTeX users is:
 
 ```
 @Manual{,
    title = {An introduction to the prospectr package},
    author = {Antoine Stevens and Leornardo Ramirez-Lopez},
    publication = {R package Vignette},
    year = {2020},
    note = {R package version 0.2.1},
  }
  ```

## Bug report and development version

You can send an email to the package maintainer (<ramirez.lopez.leo@gmail.com>) 
or create an [issue](https://github.com/l-ramirez-lopez/prospectr/issues) on github. 
To install the development version of `prospectr`, simply install [`devtools`](https://CRAN.R-project.org/package=devtools) from 
CRAN then run `install_github("l-ramirez-lopez/prospectr")`.
