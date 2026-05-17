# Splice correction of a spectral matrix acquired with an ASD spectrometer

Corrects steps in an input spectral matrix by linear interpolation of
the values of the edges of the middle sensor

## Usage

``` r
spliceCorrection(X, wav, splice = c(1000, 1830), interpol.bands = 10)
```

## Arguments

- X:

  a numeric matrix or vector to transform (optionally a data frame that
  can be coerced to a numerical matrix).

- wav:

  a numeric vector with band positions.

- splice:

  a numeric vector of length 1 or 2 with the positions of the splice(s).
  Default: `c(1000, 1830)` (as for the ASD FieldSpec Pro spectrometer of
  Malvern Panalytical). See details.

- interpol.bands:

  the number of interpolation bands.

## Value

a matrix with the splice corrected data.

## Details

This function uses by default the positions for the ASD FieldSpec Pro
spectroradiometer (Malvern Panalytical) which usually exhibit steps at
the splice of the three built-in detectors, positioned at 1000 nm (end
of VNIR detector) and 1830 nm (end of SWIR1 detector). The data
corresponding to the spectral region after the first step is used as
reference for correcting the first region and the laste region (if 2
steps are supplied). Other typical examples of splice artifacts caused
by concatenating data captured by different detectors inside the
spectrometer:

- XDS (FOSS): 1100 nm

- ProxiMate (BUCHI Labortechnik): 900 nm

## Author

Antoine Stevens and [Leonardo
Ramirez-Lopez](https://orcid.org/0000-0002-5369-5120)
