# Resample to given band position and fwhm

Resample, written in C++

## Usage

``` r
resample_fwhm(X, wav, new_wav, fwhm)
```

## Arguments

- X:

  matrix to resample

- wav:

  a numeric `vector` giving the original band positions

- new_wav:

  a numeric `vector` giving the new band positions

- fwhm:

  numeric `vector` giving the full width half maximums of the new band
  positions.
