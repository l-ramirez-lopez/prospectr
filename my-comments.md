# prospectr
 
# Rhub checks for release of `resemble 0.2.1` (`seville`)

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


