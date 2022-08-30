# `prospectr`


`prospectr` 0.2.6
===============

### Improvements and fixes

* `read_nircal()`: non-utf characters in sample IDs are properly handled (before
some IDs were wrongly converted into NAs).Non-UTF8 characters are now converted 
to their corresponding codes.

* Documentation is now compatible with HTML5.

`prospectr` 0.2.5
===============

### Improvements and fixes

* `kenStone()`: an error in a sanity check that prevented the 
function to run when the number of groups to be selected was smaller than the 
total number of groups. This sanity check has been fixed. Thanks to 
Pogs Manalili for reporting this ([#41](https://github.com/l-ramirez-lopez/prospectr/issues/41)). 

* `kenStone()`: this function was failing when when using the Mahalanobis distance 
on a matrix of one column. By default the function transforms the matrix onto 
the Mahalanobis space using either SVD or eigendecomposition. This makes sense 
for matrices with more than two columns. However for a matrix of one column, we 
now assume that such space is equivalent to the variable divided by the its 
standard deviation. Thanks to Sergio Roldan (https://github.com/sdroldan) for 
reporting this ([#40](https://github.com/l-ramirez-lopez/prospectr/issues/40])).


* `spliceCorrection()`: correcting more than one point at once  (argument splice)
in the spectra was returning an error. This was a bug introduced in 
version 0.2.4. Thanks to Jose Lucas Safanelli (https://github.com/zecojls) 
for reporting this ([#39](https://github.com/l-ramirez-lopez/prospectr/issues/39)). 


`prospectr` 0.2.4
===============

### Improvements and fixes

* `baseline()`: in some cases the function did not properly capture the baseline
confounding maximum values of a peaks as part of the baseline. This has been 
addressed in this version by ensuring the envelope used in the computation of the 
convex hull (used to extract the baseline) is properly defined. At the edges, 
this envelope has always values higher than any peak of the spectrum. 

* `read_nircal()`: the function was wrongly identifying actual .nir files as 
invalid. This preventing the function from reading any nir file. This has been 
fixed. 


`prospectr` 0.2.3
===============

### New features

* The `kenStone()` function now includes a new feature that allows to initialize the
calibration sample search with a user-defined subset of observations (which are 
to be included in the final calibration subset). Thanks to Thorsten Behrens and
Philipp Baumann for suggesting this feature. 

* The gap-segment derivatives (`gapDer()`) function now allows for high order 
derivatives. In previous versions the function only allowed up to derivatives of 
4th order, in this version the the function accepts as derivative order 
any integer larger than 1. 

### Improvements and fixes
* `binning() `a bug in the creation of the binning groups has been fixed. This bug 
is in fact inherited from a problem in the `findInterval()` function. The breaks 
(given in the vec arument) might get corrupted when they contain many decimal 
places. These breaks (in vec) are used to define the final bins. The problem in 
the binning function was that when a frequency 
variable (e.g. wavelength) was exactly on the left of the bin
range the variable was assigned to the next bin. In some cases this lead to 
small discrepancies in the in the computation of the mean of the bins. 

* ``spliceCorrection()`` now accepts one or two values as input for the splice
argument. Previously it only accepted a vector of length two. For example, now it 
corrrects for splice steps of spectra that originates from spectrometers 
with two detectors (i.e. it corrects for the potential abrupt transition 
betwteen the two detectors). 

* An extra sanity check has been added to the ``read_nircal()`` function. The 
function evaluaes whether it is indeed a file properly produced by the BUCHI 
nircal software. 

* There was a bug in the filter for the 3rd order derivative in gap segment 
derivative (`gapDer()`) function. One of the factors in the filter had a wrong 
negative sign. 

* There was a bug in the `read_nircal()` function. It emerged only in some files and 
prevented the function from reading the Description field properly 

### Changes
* In the `msc()` function the argument `reference_spc` has been renamed to 
`ref_spectrum` to emphasize that its input must be is a vector and not a 
matrix of spectra. 

`prospectr` 0.2.2
===============

### New features

* a new function for baseline correction was added. See `baseline()`  

### Improvements and fixes

* The package was stripping some symbols for Rcpp functions in Makevars in order 
to reduce the installation size of the package. Now these lines have been 
commented to comply with CRAN policies

* `standardNormalVariate()` returns now a matrix (previously a data.frame)


`prospectr` 0.2.1
===============
* New license
* The detrend function now allows to remove trends corresponding to different 
polynomial orders. Check the new 'p' argument. 
* New preprocessing function: multiplicative scatter correction (msc)
* Now `resample()` also has a new argument (`...`) to pass additional arguments to the 
`splinefun()` function (of stats) used within `resample()` when the argument 
`interpol = 'spline'` (the default since version 0.2.0)
* Bug fix: in `read_nircal()`, the description file was not being properly read
* Now performing automatic unit tests with the package testthat

`prospectr` 0.2.0
===============
* New maintainer [Leo Ramirez-Lopez]
* Updated vignette
* A new function (`read_nircal()`) for reading buchi NIRcal files has been added
* Documentation reviewed
* The description of the continuum removal algorithm is was adjusted. Previously 
was indicated that the implemented algorithm was based on the search for a 
local minimum of the absorbance spectra the algorithm, however in fact it looks 
for the convex hull. Thanks to Peter Tillmann for noticing this.

`prospectr` 0.1.4
===============
* Fix bug in `kenStone()` and `duplex()` when the group parameter is used

`prospectr` 0.1.3
===============
* Fix in DESCRITPION and NAMESPACE as required by Rcpp 0.11

`prospectr` 0.1.2
===============
* bug fix for `readASD()` when ASD file version = 7.0
* add sanity check in `spliceCorrection()`

`prospectr` 0.1.1
===============
* bug fix for `shenkWest()` when `rm.outlier = TRUE` 
* bug fix for `gapDer()` when input is a data.frame

`prospectr` 0.1
===============
* Initial release of the package
