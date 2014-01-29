# prospectr: Processing and sample selection for vis-NIR spectral data

Visit the package website [here](http://antoinestevens.github.io/prospectr/) !

`prospectr` provides various utilities for pre--processing and sample selection of visible and near infrared spectral data with [R](http://cran.r-project.org/). While similar functions are available in other packages, like [`signal`](http://cran.r-project.org/web/packages/signal/index.html), the functions in this package works indifferently for `data.frame`, `matrix` and `vector` inputs. Besides, several functions are optimized for speed and use C++ code through the [`Rcpp`](http://cran.r-project.org/web/packages/Rcpp/index.html) and [`RcppArmadillo`](http://cran.r-project.org/web/packages/RcppArmadillo/index.html) packages.

A vignette gives an overview of the main functionalities of the package. Just type `vignette("prospectr-intro")` in the console to access it. Currently, the following preprocessing functions are available:

 `continuumRemoval`      : continuum-removed absorbance or reflectance values                     
 `savitzkyGolay`         : Savitzky-Golay filter (smoothing and derivatives)                      
 `gapDer`                : gap-segment derivative                                                 
 `movav`                 : moving average                                                         
 `standardNormalVariate` : standard normal variate (snv)                                          
 `detrend`               : detrend normalization                                                  
 `binning`               : average in column--wise subsets                                        
 `resample`              : resample a signal to new coordinates by linear or spline interpolation 
 `resample2`             : resample a signal to new coordinates using FWHM values                 
 `blockScale`            : block scaling                                                           
 `blockNorm`             : sum of squares block weighting                                         

The selection of samples/observations for calibration of vis-NIR data can be achieved with one of the following functions

 `kenStone`  : CADEX (Kennard--Stone) algorithm                
 `duplex`    : DUPLEX algorithm                                
 `naes`      : k-means sampling                                
 `shenkWest` : SELECT algorithm                                
 `puchwein`  : Puchwein sampling                               
 `honigs`    : Unique-sample selection by spectral subtraction 

Other useful functions are also available:

 `readASD`          : read binary or text files from an ASD instrument (Indico Pro format)         
 `spliceCorrection` : correct spectra for steps at the splice of detectors in an ASD FieldSpec Pro  
 `cochranTest`      : detects replicate outliers with the Cochran _C_ test                         

## Bug report and development version

You can send an email to the package maintainer (<antoine.stevens@uclouvain.be>) or create an [issue](http://github.com/antoinestevens/prospectr/issues) on github. To install the development version of `prospectr`, simply install [`devtools`](http://cran.r-project.org/web/packages/devtools/index.html) from CRAN then run `install_github("prospectr","antoinestevens")`.
