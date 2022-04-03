---
title: "cran-comments.md"
author: "Leo Ramirez Lopez"
date: "13 3 2020"
---


## version 0.2.3
## Test environments
18.02.2022

Check: installed package size
Result: NOTE
     installed size is 6.3Mb
     sub-directories of 1Mb or more:
     data 2.1Mb
     libs 3.4Mb
Flavors: r-release-macos-arm64, r-release-macos-x86_64, r-oldrel-macos-x86_64


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

- Running under: Windows Server x64 (build 17763) (appveyor) R 3.6.3 

- ubuntu 16.04.6 (on travis-ci), R version 4.0.2

- CRAN win-builders

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 


## version 0.2.1
## Test environments
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

- Running under: Windows Server x64 (build 17763) (appveyor) R 3.6.3 

- ubuntu 16.04.6 (on travis-ci), R version 4.0.2

- CRAN win-builders

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 



## version 0.2.0
## Test environments
* local x86_64-w64-mingw32 (64-bit) install, R version 3.6.1/R devel 2020-03-12 r77936
* Running under: Windows Server x64 (build 17763) (appveyor) R 3.6.3 
* ubuntu 16.04.6 (on travis-ci), R version 3.6.2
* win-builder 

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE (in all test environments):

* checking installed package size ... NOTE
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    data   4.0Mb
    libs   1.8Mb

## Recommenations CRAN submission
For the future:  Add some reference about the method in
the Description field in the form Authors (year) <doi:.....>?