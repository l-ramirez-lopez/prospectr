# prospectr
# version 0.2.8 - galo

# submission message:
Dear CRAN maintainers,
I am submitting my package "prospectr" to CRAN. This version fixes problems with the documentation which were reported in the CRAN checks.
Prior to this submission, this tarball has been checked with in the winbuilder service. 
Reverse dependencies have also been checked. 
Best regards,
Leonardo


## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```

# R win builder checks for release of `prospectr 0.2.8` (`galo`) 50.03.2025 
passed all the checks without notes.

No rhub tests
-----------------

# prospectr
# version 0.2.7 - cakes

# submission message:
Dear CRAN maintainers,
I am submitting my package "prospectr" to CRAN. This version fixes problems with the documentation which were reported in the CRAN checks.
Prior to this submission, this tarball has been checked with in the winbuilder service. Apart from that it has been also submitted to extensive tests in rhub.
For this second submission the package passed all the tests in the above platforms. 
Reverse dependencies have also been checked. 
Best regards,
Leonardo


## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```

# R win builder checks for release of `prospectr 0.2.7` (`ccakes`) 15.02.2024 
passed all the checks without notes.

# Rhub checks for release of `prospectr 0.2.7` (`ccakes`) 15.02.2024
The checks were conducted in the following platforms through rhub:

```
rhub::check(paste0(gsub("/prospectr$", "/", getwd()), "prospectr_0.2.7.tar.gz"), 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```
- "fedora-gcc-devel"  NOTE
* checking installed package size ... NOTE
  installed size is  6.5Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   3.1Mb

- "windows-x86_64-devel" OK

- "macos-highsierra-release-cran" (not tested)

- "windows-x86_64-release" OK

- "ubuntu-gcc-release" OK

- "solaris-x86-patched-ods" (not tested)



# version 0.2.6 - chicago

# submission message:
Dear CRAN maintainers,
I am submitting my package "prospectr" to CRAN. This version accounts for 
problems found in Rd files auto-generated with roxygen2 7.1.2 (not compatible 
with HTML5). The new Rd files are now compatible with HTML5 (as Rd files 
are generated with roxygen2_7.2.0 ). 
Prior to this submission, this tarball has been checked with in the winbuilder service. Apart from that it has been also submitted to extensive tests in rhub.
A first submission of this version failed (for "r-devel-linux-x86_64-debian-gcc"), 
therefore following platforms were tested for a second submission using Rhub: 
- Debian Linux, R-devel, GCC ASAN/UBSAN
- Debian Linux, R-devel, GCC, no long double
- Debian Linux, R-devel, clang, ISO-8859-15 locale
- Debian Linux, R-devel, GCC
For this second submission the package passed all the tests in the above platforms. 
Reverse dependencies have also been checked. 
Best regards,
Leonardo


## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```

# R win builder checks for release of `prospectr 0.2.6` (`chicago`) 30.08.2022 
passed all the checks without notes.

# Rhub checks for release of `prospectr 0.2.6` (`chicago`) 30.08.2022
The checks were conducted in the following platforms through rhub:

```
rhub::check(paste0(gsub("/prospectr$", "/", getwd()), "prospectr_0.2.6.tar.gz"), 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```
- "linux-x86_64-rocker-gcc-san"

- "fedora-gcc-devel"  NOTE
  installed size is  6.6Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   3.9Mb

- "windows-x86_64-devel" OK

- "macos-highsierra-release-cran" OK

- "windows-x86_64-release" OK


- "ubuntu-gcc-release" NOTE
  installed size is  6.8Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   4.0Mb


- "solaris-x86-patched-ods" checking package dependencies (2.7s)
   Package suggested but not available: ‘testthat’
   
   The suggested packages are required for a complete check.
   Checking can be attempted without them by setting the environment
   variable _R_CHECK_FORCE_SUGGESTS_ to a false value.
   
   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
   manual.



# version 0.2.5 - antilla

# submission message:
Dear CRAN maintainers,
I am submitting my package "prospectr" to CRAN. This version accounts for some bugs. 
Prior to this submission, this tarball has been checked with in the winbuilder service. Apart from that it has been also submitted to extensive tests in rhub.
A first submission of this version failed (for "r-devel-linux-x86_64-debian-gcc"), 
therefore following platforms were tested for a second submission using Rhub: 
- Debian Linux, R-devel, GCC ASAN/UBSAN
- Debian Linux, R-devel, GCC, no long double
- Debian Linux, R-devel, clang, ISO-8859-15 locale
- Debian Linux, R-devel, GCC
For this second submission the package passed all the tests in the above platforms. 
Reverse dependencies have also been checked. 
Best regards,
Leonardo


## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```

# R win builder checks for release of `prospectr 0.2.5` (`antilla`) 19.07.2022 
passed all the checks without notes.

# Rhub checks for release of `prospectr 0.2.5` (`antilla`) 18.07.2022
The checks were conducted in the following platforms through rhub:

```
rhub::check(paste0(gsub("/prospectr$", "/", getwd()), "prospectr_0.2.5.tar.gz"), 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```
- "linux-x86_64-rocker-gcc-san"

- "fedora-gcc-devel"  NOTE
  installed size is  6.6Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   3.9Mb

- "windows-x86_64-devel" OK

- "macos-highsierra-release-cran" OK

- "windows-x86_64-release" OK


- "ubuntu-gcc-release" NOTE
  installed size is  6.8Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   4.0Mb


- "solaris-x86-patched-ods" checking package dependencies (2.7s)
   Package suggested but not available: ‘testthat’
   
   The suggested packages are required for a complete check.
   Checking can be attempted without them by setting the environment
   variable _R_CHECK_FORCE_SUGGESTS_ to a false value.
   
   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
   manual.



# version 0.2.4

# submission message:
Dear CRAN maintainers,
I am submitting my package "prospectr" to CRAN. This version accounts a bug in one function. 
Prior to this submission, this tarball has been checked with in the winbuilder service. Apart from that it has been also submitted to extensive tests in rhub.
Reverse dependencies have also been checked. 
Best regards,
Leonardo

## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```



# R win builder checks for release of `prospectr 0.2.4` (`mandarina`) 03.04.2022 passed all the checks without notes

# Rhub checks for release of `prospectr 0.2.4` (`mandarina`) 03.04.2022
The checks were conducted in the following platforms through rhub:

```
rhub::check(paste0(gsub("/prospectr$", "/", getwd()), "prospectr_0.2.4.tar.gz"), 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```
Since there was an error with the first submission (with the flavor 
"r-devel-linux-x86_64-debian-gcc"), the following platforms were added: 
linux-x86_64-rocker-gcc-san; debian-gcc-devel-nold; debian-gcc-devel


- linux-x86_64-rocker-gcc-san OK

- debian-gcc-devel-nold OK

- debian-gcc-devel OK


- "fedora-gcc-devel" 
* checking installed package size ... NOTE
  installed size is  6.6Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   3.9Mb
    
- "windows-x86_64-devel" OK

- "macos-highsierra-release-cran" OK

- "windows-x86_64-release" 
* checking installed package size ... NOTE
  installed size is  5.2Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   2.4Mb

- "ubuntu-gcc-release" 
* checking installed package size ... NOTE
  installed size is  7.0Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   4.2Mb

- "solaris-x86-patched-ods" OK

# A first submission to CRAN was rejected. The problem: 

Flavor: r-devel-linux-x86_64-debian-gcc
Check: package subdirectories, Result: NOTE
  Problems with news in 'NEWS':
    Cannot process chunk/lines:
      function to run when the number of groups to be selected was smaller than the
    Cannot process chunk/lines:
      total number of groups. This sanity check has been fixed. Thanks to
    Cannot process chunk/lines:
      on a matrix of one column. By default the funcion transforms the matrix onto
    Cannot process chunk/lines:
      the Mahalanobis space using either SVD or eigendecomposition. This makes sense
    Cannot process chunk/lines:
      for matrices with more than two columns. However for a matrix of one column, we
    Cannot process chunk/lines:
      now assume that such space is equivalent to the variable divided by the its
    Cannot process chunk/lines:
      standard deviation. Thanks to Sergio Roldán (https://github.com/sdroldan) for
    Cannot process chunk/lines:
      in the spectra was returning an error. This was a bug introduced in
    Cannot process chunk/lines:
      for reporting this.
    Cannot process chunk/lines:
      confounding maximum values of peaks as part of the baseline. This has been
    Cannot process chunk/lines:
      addressed in this version by ensuring the envelope used in the computation of the
    Cannot process chunk/lines:
      convex hull (used to extract the baseline) is properly defined. At the edges,
    Cannot process chunk/lines:
      invalid. This preventing the function from reading any nir file. This has been
    Cannot process chunk/lines:
      fixed.
    Cannot process chunk/lines:
      calibration sample search with a user-defined subset of observations (which are
    Cannot process chunk/lines:
      to be included in the final calibration subset). Thanks to Thorsten Behrens and
    Cannot process chunk/lines:
      derivatives. In previous versions the function only allowed up to derivatives of
      4th order, in this version the the function accepts as derivative order
    Cannot process chunk/lines:
      any integer larger than 1.
      ## Improvements and fixes
      * `binning() `a bug in the creation of the binning groups has been fixed. This bug
    Cannot process chunk/lines:
      is in fact inherited from a problem in the `findInterval()` function. The breaks
      (given in the vec arument) might get corrupted when they contain many decimal
    Cannot process chunk/lines:
      places. These breaks (in vec) are used to define the final bins. The problem in
    Cannot process chunk/lines:
      the binning function was that when a frequency
    Cannot process chunk/lines:
      variable (e.g. wavelength) was exactly on the left of the bin
    Cannot process chunk/lines:
      range the variable was assigned to the next bin. In some cases this lead to
    Cannot process chunk/lines:
      argument. Previously it only accepted a vector of length two. For example, now it
    Cannot process chunk/lines:
      corrrects for splice steps of spectra that originates from spectrometers
    Cannot process chunk/lines:
      with two detectors (i.e. it corrects for the potential abrupt transition
    Cannot process chunk/lines:
      function evaluaes whether it is indeed a file properly produced by the BUCHI
    Cannot process chunk/lines:
      derivative (`gapDer`) function. One of the factors in the filter had a wrong
    Cannot process chunk/lines:
      prevented the function from reading the Description field properly
      ## Changes
      * In the `msc()` function the argument `reference_spc` has been renamed to
      `ref_spectrum` to emphasize that its input must be is a vector and not a
    Cannot process chunk/lines:
      matrix of spectra.
    Cannot process chunk/lines:
      to reduce the installation size of the package. Now these lines have been
      


# version 0.2.3

# submission message:
Dear CRAN maintainers,
I am submitting my package "prospectr" to CRAN. This version accounts for some bugs. 
Prior to this submission, this tarball has been checked with in the winbuilder service. Apart from that it has been also submitted to extensive tests in rhub.
Reverse dependencies have also been checked. 
Best regards,
Leonardo

## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```
The aim of the release was to fix minor bugs

# R win builder checks for release of `prospectr 0.2.3` (`positive`) 18.02.2022 passed all the checks without notes

# Rhub checks for release of `prospectr 0.2.3` (`positive`) 18.02.2022
The checks were conducted in the following platforms through rhub:

```
rhub::check(paste0(gsub("/prospectr$", "/", getwd()), "prospectr_0.2.3.tar.gz"), 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```
- "fedora-gcc-devel" NOTE 
* checking installed package size ... NOTE
  installed size is  6.7Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   4.0Mb
    
- "windows-x86_64-devel" OK

- "macos-highsierra-release-cran" OK

- "windows-x86_64-release" OK 

- "linux-x86_64-rocker-gcc-san" ## Not checked Rhub returned a PREPERROR

- "ubuntu-gcc-release" NOTE
* checking installed package size ... NOTE
  installed size is  7.0Mb
  sub-directories of 1Mb or more:
    data   1.9Mb
    libs   4.2Mb

- "solaris-x86-patched-ods"
 
# version 0.2.2

## Package was built using: 
```
devtools::build(
  pkg = ".",
  path = NULL,
  binary = FALSE,
  vignettes = TRUE,
  manual = TRUE,
  args = NULL,
  quiet = FALSE
)
```

The package was stripping some symbols for Rcpp functions in Makevars in order 
to reduce the installation size of the package. Now these lines have been 
commented to comply with CRAN policies:
#strippedLib: $(SHLIB)
#		if test -e "/usr/bin/strip" & test -e "/bin/uname" & [[ `uname` == "Linux" ]]; then /usr/bin/strip --strip-debug $(SHLIB); fi
#.phony: strippedLib


# Rhub checks for release of `prospectr 0.2.2` (`flawil`) 26.11.2021
The checks were conducted in the following platforms through rhub:

```
rhub::check("/home/rl_leonardo/github/prospectr_0.2.2.tar.gz", 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```

- "windows-x86_64-devel" OK

- "macos-highsierra-release-cran" OK

- "linux-x86_64-rocker-gcc-san" ## Not checked Rhub throwed a PREPERROR

- "ubuntu-gcc-release"
* checking installed package size ... NOTE
installed size is  7.5Mb
sub-directories of 1Mb or more:
  data   1.9Mb
libs   4.2Mb
"debian-clang-devel" OK

- "fedora-gcc-devel"
checking installed package size ... NOTE
installed size is  7.3Mb
sub-directories of 1Mb or more:
  data   1.9Mb
libs   4.0Mb

- "solaris-x86-patched-ods" OK

# version 0.2.1

# Rhub checks for release of `prospectr 0.2.1` (`seville`)


24.10.2020

The checks were conducted in the following platforms through rhub:

- "debian-clang-devel"

- "debian-gcc-devel"

- "fedora-gcc-devel"

- "debian-gcc-devel-nold"

- "debian-gcc-patched"

- "debian-gcc-release"

- "linux-x86_64-rocker-gcc-san" 

- "macos-highsierra-release-cran" 

- "solaris-x86-patched-ods" 

- "ubuntu-gcc-release"

- "windows-x86_64-devel"

For example, for checks with "fedora-gcc-devel"", the following code was used::
```
rhub::check("C:/Users/raml/Documents/GitHub/prospectr_0.2.1.tar.gz", 
            platform = c("fedora-gcc-devel"), 
            email = "ramirez.lopez.leo@gmail.com")
```

## Size of the package
To reduce the size of the package, Makevars was modified was added.

## Vignette
The package uses html_vignette. For a pdf vignette, the checks for 
"fedora-gcc-devel"and "fedora-clang-devel" at rhub, were throwing an error 
indicating that the "framed.sty" package was not available?


## NOTE for compiled code
An strange not was thrown when the check was done locally on windows with R `4.0.3`
It is apparently a problem in R core and not related to the package nor Rcpp. 
The issue was reported here:
https://stackoverflow.com/questions/64402688/information-on-o-files-for-x64-is-not-available-note-on-r-package-checks-using/64419033#64419033


