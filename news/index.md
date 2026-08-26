# Changelog

## `prospectr 0.2.11 (postdetrendy)`

CRAN release: 2026-08-25

#### Bug fixes

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  fixed a failure when reading files whose sample IDs contain non-ASCII
  characters. NIRCal stores text in a Windows single-byte codepage
  (CP1252/latin1), but those bytes were passed to
  [`enc2utf8()`](https://rdrr.io/r/base/Encoding.html), which honours
  the declared encoding of the input and therefore performs no
  conversion at all on strings read with
  [`readBin()`](https://rdrr.io/r/base/readBin.html). In a UTF-8 locale
  the bytes were then left invalid and
  [`nchar()`](https://rdrr.io/r/base/nchar.html) aborted with
  `invalid multibyte string`. The record-number prefix is now stripped
  byte-wise and the text converted explicitly, so IDs such as
  `Adubo Líquido_Tanque 05 - 18/02/2022` are read correctly, including
  their internal slashes
  ([\#94](https://github.com/l-ramirez-lopez/prospectr/issues/94)).

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  fixed a failure when the names of the response variables contain
  regular expression metacharacters. The names are used to locate the
  sample blocks in the raw file and were interpolated into the search
  pattern unescaped, so names such as `Ca++` or `Mg++` aborted the read
  with `invalid use of repetition operators`. Names containing `(`, `)`
  or `+` (for example `P (Resina)` or `H+Al`) were affected more
  quietly: the pattern remained valid but no longer matched the file,
  and the corresponding blocks were never found. Metacharacters are now
  escaped before the search
  ([\#94](https://github.com/l-ramirez-lopez/prospectr/issues/94)).

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  fixed corruption of accented response variable names. These were
  converted with `iconv(from = "ASCII", sub = "byte")`, which replaces
  each non-ASCII byte with its hex escape as literal text, so a variable
  named `Matéria Orgânica` was returned as `Mat<e9>ria Org<e2>nica`.
  Conversion now uses the encoding the files are actually written in
  ([\#94](https://github.com/l-ramirez-lopez/prospectr/issues/94)).

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  fixed the renaming of duplicated response variable names. The index
  was compared against the full vector of duplicated names rather than
  the one being processed, so files carrying two or more distinct
  duplicated names were labelled incorrectly
  ([\#94](https://github.com/l-ramirez-lopez/prospectr/issues/94)).

#### Internal

- Continuous integration moved to GitHub Actions (`R-CMD-check`,
  `pkgdown`, `rhub` and test coverage workflows).

## `prospectr 0.2.10 (zurich)`

CRAN release: 2026-06-23

#### Features:

- [`detrend()`](https://l-ramirez-lopez.github.io/prospectr/reference/detrend.md):
  New `method` argument to allow polynomial detrending without a prior
  SNV transformation. The default behaviour is unchanged and remains
  consistent with Barnes et al. (1989). Set `method = "poly"` to apply
  pure polynomial detrending independently of SNV, for example as a
  separate step in a pre-processing pipeline.

## `prospectr 0.2.9 (proxy)`

CRAN release: 2026-05-31

#### Bug fixes

- [`continuumRemoval()`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md):
  fixed `NA` values produced for spectra with very low reflectance in
  the first bands and high NIR reflectance. The fixed boundary offset of
  1 wavelength unit used in the internal convex hull computation was too
  large for spectra with fine spectral resolution or wavelengths
  expressed in units other than nanometres. The offset is now derived
  from the actual spectral resolution at each edge, making the behaviour
  unit-agnostic (reported by [@jbferet](https://github.com/jbferet),
  [\#80](https://github.com/l-ramirez-lopez/prospectr/issues/80)).

- [`continuumRemoval()`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md):
  fixed `NaN` produced at the first wavelength when its reflectance
  value is exactly zero. The continuum-removed value is now set to 1 (no
  absorption feature) at bands where both the spectrum and the continuum
  are zero
  ([\#80](https://github.com/l-ramirez-lopez/prospectr/issues/80)).

- [`continuumRemoval()`](https://l-ramirez-lopez.github.io/prospectr/reference/continuumRemoval.md):
  corrected a long-standing typo in the `method` argument:
  `"substraction"` has been replaced by `"subtraction"`. A deprecation
  warning is issued if the old spelling is passed explicitly.

- [`standardNormalVariate()`](https://l-ramirez-lopez.github.io/prospectr/reference/standardNormalVariate.md):
  now it can handle a single spectrum passed as a vector.

#### New features

- [`detrend()`](https://l-ramirez-lopez.github.io/prospectr/reference/detrend.md):
  added `snv` argument (default `TRUE`) to allow polynomial detrending
  without a prior SNV transformation. The default behaviour is unchanged
  and remains consistent with Barnes et al. (1989). Set `snv = FALSE` to
  apply pure polynomial detrending independently of SNV, for example as
  a separate step in a pre-processing pipeline.

- The vignette has been reorganised into three separate vignettes with
  extended examples: (1) an introduction to the package, (2) signal
  processing, and (3) calibration sampling.

#### Documentation

- [`spliceCorrection()`](https://l-ramirez-lopez.github.io/prospectr/reference/spliceCorrection.md):
  clarified that `wav` must be a numeric vector of length equal to
  `ncol(X)`, not a two-element range vector
  ([\#69](https://github.com/l-ramirez-lopez/prospectr/issues/69)).

- [`duplex()`](https://l-ramirez-lopez.github.io/prospectr/reference/duplex.md):
  a warning is now issued when `k` exceeds `floor(nrow(X) / 2)`, the
  maximum number of samples selectable per set. Previously, `k` was
  silently capped to this limit with no indication to the user (reported
  by [@georgejr45](https://github.com/georgejr45)).

## `prospectr 0.2.8 (galo)`

CRAN release: 2025-03-05

#### Improvements and fixes

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  The function was still crashing because of some special characters in
  the IDs. This has been fixed
  ([\#65](https://github.com/l-ramirez-lopez/prospectr/issues/65)).

- `gap_der()`: It can now accept `m = 0`.

## `prospectr 0.2.7 (cakes)`

CRAN release: 2024-02-16

#### Improvements and fixes

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  The function was crashing because of some special characters in the
  IDs. This has been fixed
  ([\#65](https://github.com/l-ramirez-lopez/prospectr/issues/65)).

- [`kenStone()`](https://l-ramirez-lopez.github.io/prospectr/reference/kenStone.md):
  An issue has been fixed for the `k` argument when `group` was used.
  The function prevented to pass a value to the `k` argument larger than
  the number of groups passed to the `group` argument
  ([\#51](https://github.com/l-ramirez-lopez/prospectr/issues/51)).
  Thanks to Michael Simmler from Agroscope for pointing at this issue.

- [`duplex()`](https://l-ramirez-lopez.github.io/prospectr/reference/duplex.md):
  Avoid error when the number of samples to select is exactly half of
  the size of the input data set
  ([\#48](https://github.com/l-ramirez-lopez/prospectr/pull/48)). Thanks
  to Philipp Baumann for his pull request solving this issue. Some
  modifications to his original solution were made which also address
  the concerns raised by Matthew Dirks (skylogic004)
  ([\#56](https://github.com/l-ramirez-lopez/prospectr/pull/56)). Thank
  you!

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  fixes a bug when reading some special type of comments of each sample
  in the file.

## `prospectr` 0.2.6

CRAN release: 2022-08-31

#### Improvements and fixes

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  non-utf characters in sample IDs are properly handled (before some IDs
  were wrongly converted into NAs).Non-UTF8 characters are now converted
  to their corresponding codes.

- Documentation is now compatible with HTML5.

## `prospectr` 0.2.5

CRAN release: 2022-07-19

#### Improvements and fixes

- [`kenStone()`](https://l-ramirez-lopez.github.io/prospectr/reference/kenStone.md):
  an error in a sanity check that prevented the function to run when the
  number of groups to be selected was smaller than the total number of
  groups. This sanity check has been fixed. Thanks to Pogs Manalili for
  reporting this
  ([\#41](https://github.com/l-ramirez-lopez/prospectr/issues/41)).

- [`kenStone()`](https://l-ramirez-lopez.github.io/prospectr/reference/kenStone.md):
  this function was failing when when using the Mahalanobis distance on
  a matrix of one column. By default the function transforms the matrix
  onto the Mahalanobis space using either SVD or eigendecomposition.
  This makes sense for matrices with more than two columns. However for
  a matrix of one column, we now assume that such space is equivalent to
  the variable divided by the its standard deviation. Thanks to Sergio
  Roldan (<https://github.com/sdroldan>) for reporting this
  ([\#40](https://github.com/l-ramirez-lopez/prospectr/issues/40)).

- [`spliceCorrection()`](https://l-ramirez-lopez.github.io/prospectr/reference/spliceCorrection.md):
  correcting more than one point at once (argument splice) in the
  spectra was returning an error. This was a bug introduced in version
  0.2.4. Thanks to Jose Lucas Safanelli (<https://github.com/zecojls>)
  for reporting this
  ([\#39](https://github.com/l-ramirez-lopez/prospectr/issues/39)).

## `prospectr` 0.2.4

CRAN release: 2022-04-03

#### Improvements and fixes

- [`baseline()`](https://l-ramirez-lopez.github.io/prospectr/reference/baseline.md):
  in some cases the function did not properly capture the baseline
  confounding maximum values of peaks as part of the baseline. This has
  been addressed in this version by ensuring the envelope used in the
  computation of the convex hull (used to extract the baseline) is
  properly defined. At the edges, this envelope has always values higher
  than any peak of the spectrum.

- [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md):
  the function was wrongly identifying actual .nir files as invalid.
  This preventing the function from reading any nir file. This has been
  fixed.

## `prospectr` 0.2.3

CRAN release: 2022-02-18

#### New features

- The
  [`kenStone()`](https://l-ramirez-lopez.github.io/prospectr/reference/kenStone.md)
  function now includes a new feature that allows to initialize the
  calibration sample search with a user-defined subset of observations
  (which are to be included in the final calibration subset). Thanks to
  Thorsten Behrens and Philipp Baumann for suggesting this feature.

- The gap-segment derivatives
  ([`gapDer()`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md))
  function now allows for high order derivatives. In previous versions
  the function only allowed up to derivatives of 4th order, in this
  version the the function accepts as derivative order any integer
  larger than 1.

#### Improvements and fixes

- [`binning()`](https://l-ramirez-lopez.github.io/prospectr/reference/binning.md)a
  bug in the creation of the binning groups has been fixed. This bug is
  in fact inherited from a problem in the
  [`findInterval()`](https://rdrr.io/r/base/findInterval.html) function.
  The breaks (given in the vec argument) might get corrupted when they
  contain many decimal places. These breaks (in vec) are used to define
  the final bins. The problem in the binning function was that when a
  frequency variable (e.g. wavelength) was exactly on the left of the
  bin range the variable was assigned to the next bin. In some cases
  this lead to small discrepancies in the in the computation of the mean
  of the bins.

- [`spliceCorrection()`](https://l-ramirez-lopez.github.io/prospectr/reference/spliceCorrection.md)
  now accepts one or two values as input for the splice argument.
  Previously it only accepted a vector of length two. For example, now
  it corrects for splice steps of spectra that originates from
  spectrometers with two detectors (i.e. it corrects for the potential
  abrupt transition between the two detectors).

- An extra sanity check has been added to the
  [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md)
  function. The function evaluates whether it is indeed a file properly
  produced by the BUCHI nircal software.

- There was a bug in the filter for the 3rd order derivative in gap
  segment derivative
  ([`gapDer()`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md))
  function. One of the factors in the filter had a wrong negative sign.

- There was a bug in the
  [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md)
  function. It emerged only in some files and prevented the function
  from reading the Description field properly

#### Changes

- In the
  [`msc()`](https://l-ramirez-lopez.github.io/prospectr/reference/msc.md)
  function the argument `reference_spc` has been renamed to
  `ref_spectrum` to emphasize that its input must be is a vector and not
  a matrix of spectra.

## `prospectr` 0.2.2

CRAN release: 2021-11-27

#### New features

- a new function for baseline correction was added. See
  [`baseline()`](https://l-ramirez-lopez.github.io/prospectr/reference/baseline.md)

#### Improvements and fixes

- The package was stripping some symbols for Rcpp functions in Makevars
  in order to reduce the installation size of the package. Now these
  lines have been commented to comply with CRAN policies

- [`standardNormalVariate()`](https://l-ramirez-lopez.github.io/prospectr/reference/standardNormalVariate.md)
  returns now a matrix (previously a data.frame)

## `prospectr` 0.2.1

CRAN release: 2020-10-23

- New license
- The detrend function now allows to remove trends corresponding to
  different polynomial orders. Check the new ‘p’ argument.
- New preprocessing function: multiplicative scatter correction (msc)
- Now
  [`resample()`](https://l-ramirez-lopez.github.io/prospectr/reference/resample.md)
  also has a new argument (`...`) to pass additional arguments to the
  [`splinefun()`](https://rdrr.io/r/stats/splinefun.html) function (of
  stats) used within
  [`resample()`](https://l-ramirez-lopez.github.io/prospectr/reference/resample.md)
  when the argument `interpol = 'spline'` (the default since version
  0.2.0)
- Bug fix: in
  [`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md),
  the description file was not being properly read
- Now performing automatic unit tests with the package testthat

## `prospectr` 0.2.0

CRAN release: 2020-03-14

- New maintainer \[Leo Ramirez-Lopez\]
- Updated vignette
- A new function
  ([`read_nircal()`](https://l-ramirez-lopez.github.io/prospectr/reference/read_nircal.md))
  for reading buchi NIRcal files has been added
- Documentation reviewed
- The description of the continuum removal algorithm was adjusted.
  Previously, it was indicated that the implemented algorithm was based
  on the search for a local minimum of the absorbance spectra; however,
  in fact it looks for the convex hull. Thanks to Peter Tillmann for
  noticing this.

## `prospectr` 0.1.4

- Fix bug in
  [`kenStone()`](https://l-ramirez-lopez.github.io/prospectr/reference/kenStone.md)
  and
  [`duplex()`](https://l-ramirez-lopez.github.io/prospectr/reference/duplex.md)
  when the group parameter is used

## `prospectr` 0.1.3

CRAN release: 2014-02-14

- Fix in DESCRITPION and NAMESPACE as required by Rcpp 0.11

## `prospectr` 0.1.2

- bug fix for
  [`readASD()`](https://l-ramirez-lopez.github.io/prospectr/reference/readASD.md)
  when ASD file version = 7.0
- add sanity check in
  [`spliceCorrection()`](https://l-ramirez-lopez.github.io/prospectr/reference/spliceCorrection.md)

## `prospectr` 0.1.1

CRAN release: 2013-12-09

- bug fix for
  [`shenkWest()`](https://l-ramirez-lopez.github.io/prospectr/reference/shenkWest.md)
  when `rm.outlier = TRUE`
- bug fix for
  [`gapDer()`](https://l-ramirez-lopez.github.io/prospectr/reference/gapDer.md)
  when input is a data.frame

## `prospectr` 0.1

CRAN release: 2013-08-14

- Initial release of the package
