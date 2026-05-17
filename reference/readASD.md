# Read ASD FieldSpec Pro binary and ASCII files

**maturing lifecycle**

Read single or multiple binary and ASCII files acquired with an ASD
FieldSpec Pro
([ASDi](https://www.malvernpanalytical.com/en/products/product-range/asd-range),
Boulder, CO) spectroradiometer

## Usage

``` r
readASD(fnames, in_format, out_format)
```

## Arguments

- fnames:

  a character vector of the name(s) (with absolute path) of the file(s)
  to read.

- in_format:

  the format of the input file: `'binary'` or `'txt'`.

- out_format:

  the format of the output: 'matrix' (default) or 'list' (see below).

## Value

if `out_format` = `'matrix'`, reflectance values of the input file(s) in
a single matrix.

if `out_format` = `'list'`, a `list` of the input file(s) data
consisting of a list with components:

- `Name`: name of the file imported

- `datetime`: date and time of acquisition in `POSIXct` format

- `header`: list with information from the header file

- `radiance`: if applicable, a numeric vector of radiance values

- `reference`: if applicable, a numeric vector of radiance values of the
  white reference

- `reflectance`: numeric vector of reflectance values

- `wavelength`: numeric vector of the band positions

## Note

There is a R port of the `importasd.m` function from the
‘FSFPostProcessing’ Matlab toolbox by Iain Robinson (University of
Edinburgh), which is based on some Java code provided by Andreas Hunei
(University of Zurich).

It seems that ASD file format has changed quite a lot with file
versions. The function will possibly not work as expected for all
versions. Please report any bugs to the package maintainer.

## References

Robinson, I., and A. MacArthur. 2011. The Field Spectroscopy Facility
Post Processing Toolbox User Guide. Post processing spectral data in
MATLAB, University of Edinburgh, Edinburgh, UK.

## Author

Antoine Stevens (R port), Iain Robinson (matlab function) & Leonardo
Ramirez-Lopez (R port)
