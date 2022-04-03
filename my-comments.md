# prospectr

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


