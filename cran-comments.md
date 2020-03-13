---
title: "cran-comments.md"
author: "Leo Ramirez Lopez"
date: "13 3 2020"
---


## Test environments
* local win install, R version 3.6.1/R devel 2020-03-12 r7793
* ubuntu 16.04.6 (on travis-ci), R version 3.6.2
* win-builder (appveyor) R 3.6.3 

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  6.0Mb
  sub-directories of 1Mb or more:
    data   4.0Mb
    libs   1.8Mb


## Downstream dependencies
I have also run R CMD check on downstream dependencies of httr 
(https://github.com/wch/checkresults/blob/master/httr/r-release). 
All packages that I could install passed except:

* Ecoengine: this appears to be a failure related to config on 
  that machine. I couldn't reproduce it locally, and it doesn't 
  seem to be related to changes in httr (the same problem exists 
  with httr 0.4).