#' @useDynLib prospectr
#' @import lifecycle
#' @import Rcpp
#' @import foreach
#' @import iterators
#' @import mathjaxr
## usethis namespace: start
#' @importFrom lifecycle deprecate_soft
## usethis namespace: end
#' @importFrom grDevices chull
#' @importFrom graphics legend matplot
#' @importFrom stats approx cov kmeans lm prcomp qf sd splinefun var
#' @importFrom utils read.table txtProgressBar setTxtProgressBar
#'
#' @description
#'
#' \ifelse{html}{\out{<a href='https://www.tidyverse.org/lifecycle/#stable'><img src='figures/lifecycle-stable.svg' alt='Stable lifecycle'></a>}}{\strong{Stable}}
#'
#' Misc functions for spectral data
#' \if{html}{\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}
#'
#' This package implements a number of functions useful for
#' pre-processing spectral data well as for selecting representative samples/spectra.
#' The functions included here are particularly useful in Near-Infrared and Infrared
#' Spectroscopy applications.
#'
#' @details
#' 
#' #' This is the version 
#' `r paste(pkg_info()[1:2], collapse = " \U002D\U002D ")` of the package. 
#' The main functionality is listed here. 
#'
#' Currently, the following preprocessing functions are available:
#'
#' \itemize{
#'   \item{\code{\link{resample}}}
#'   \item{\code{\link{resample2}}}
#'   \item{\code{\link{movav}}}
#'   \item{\code{\link{standardNormalVariate}}}
#'   \item{\code{\link{msc}}}
#'   \item{\code{\link{detrend}}}
#'   \item{\code{\link{baseline}}}
#'   \item{\code{\link{blockScale}}}
#'   \item{\code{\link{blockNorm}}}
#'   \item{\code{\link{binning}}}
#'   \item{\code{\link{savitzkyGolay}}}
#'   \item{\code{\link{gapDer}}}
#'   \item{\code{\link{continuumRemoval}}}
#'  }
#'
#' For the selection of representative samples/observations for calibrating
#' spectral models the following functions ca be used:
#'
#' \itemize{
#'   \item{\code{\link{naes}}}
#'   \item{\code{\link{honigs}}}
#'   \item{\code{\link{shenkWest}}}
#'   \item{\code{\link{kenStone}}}
#'   \item{\code{\link{duplex}}}
#'   \item{\code{\link{puchwein}}}
#'  }
#'
#'  Other useful functions are also available:
#'
#' \itemize{
#'   \item{\code{\link{read_nircal}}}
#'   \item{\code{\link{readASD}}}
#'   \item{\code{\link{spliceCorrection}}}
#'   \item{\code{\link{cochranTest}}}
#'  }
#' @name prospectr-package
#' @aliases prospectr-package prospectr
#' @title Overview of the functions in the prospectr package
#' @seealso
#' Useful links:
#' \itemize{
#' \item \url{https://github.com/l-ramirez-lopez/prospectr}
#' \item Report bugs at \url{https://github.com/l-ramirez-lopez/prospectr/issues}
#' }
#' @author
#'
#' \strong{Maintainer}: Leonardo Ramirez-Lopez \email{ramirez.lopez.leo@gmail.com}
#'
#' Authors:
#' \itemize{
#' \item Antoine Stevens (\href{https://orcid.org/0000-0002-1588-7519}{ORCID})
#'
#' \item Leonardo Ramirez-Lopez (\href{https://orcid.org/0000-0002-5369-5120}{ORCID})
#' }
######################################################################
# prospectr
# Copyrigth (C) 2024 Leonardo Ramirez-Lopez
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
######################################################################
"_PACKAGE"
NULL
