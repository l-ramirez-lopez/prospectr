[![Travis-CI Build Status](https://travis-ci.org/l-ramirez-lopez/prospectr.svg?branch=master)](https://travis-ci.org/l-ramirez-lopez/prospectr/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/prospectr)](http://cran.r-project.org/web/packages/prospectr)
[![Downloads](https://cranlogs.r-pkg.org/badges/prospectr)](https://cranlogs.r-pkg.org/badges/prospectr)
![alt text](https://raw.githubusercontent.com/antoinestevens/prospectr/gh-pages/logo_prospectr_color.png)


# `prospectr`: Misc. Functions for Processing and Sample Selection of Spectroscopic Data

Visit the package website [here](http://antoinestevens.github.io/prospectr/) !

_Antoine Stevens & Leo Ramirez-Lopez_

_Last update: 21.09.2020_

`prospectr` provides various utilities for pre--processing and sample selection 
of visible and near infrared spectral data with [R](http://cran.r-project.org/). 
While similar functions are available in other packages, like [`signal`](http://cran.r-project.org/web/packages/signal/index.html), the 
functions in this package works indifferently for `data.frame`, `matrix` and 
`vector` inputs. Besides, several functions are optimized for speed and use 
C++ code through the [`Rcpp`](http://cran.r-project.org/web/packages/Rcpp/index.html) 
and [`RcppArmadillo`](http://cran.r-project.org/web/packages/RcppArmadillo/index.html)
packages.

Check the NEWS document for new functionality and general changes in the package.

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

## Bug report and development version

You can send an email to the package maintainer (<ramirez.lopez.leo@gmail.com>) 
or create an [issue](http://github.com/l-ramirez-lopez/prospectr/issues) on github. 
To install the development version of `prospectr`, simply install [`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) from 
CRAN then run `install_github("l-ramirez-lopez/prospectr")`.
