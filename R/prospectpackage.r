#'@description 
#' This package implements a number of \R functions for preprocessing, and sample selection calibration sampling 
#' of visible and near infrared diffuse (\acronym{vis-NIR}) reflectance data
#' 
#' Currently, the following preprocessing functions are available:
#' 
#' \itemize{ 
#'   \item{\code{\link{continuumRemoval}}} 
#'   \item{\code{\link{savitzkyGolay}}} 
#'   \item{\code{\link{detrend}}} 
#'   \item{\code{\link{gapDer}}} 
#'   \item{\code{\link{movav}}} 
#'   \item{\code{\link{standardNormalVariate}}}
#'   \item{\code{\link{binning}}}   
#'   \item{\code{\link{resample}}} 
#'   \item{\code{\link{resample2}}} 
#'   \item{\code{\link{blockScale}}}  
#'   \item{\code{\link{blockNorm}}}
#'  } 
#' 
#' The selection of samples/observations for calibration of \acronym{vis-NIR} data can be achieved with one of the following functions:
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
#'  \itemize{
#'   \item{\code{\link{readASD}}} 
#'   \item{\code{\link{spliceCorrection}}}
#'   \item{\code{\link{cochranTest}}}
#'  } 
#'   
#'@docType package
#'@name prospect
#'@title prospect package
#'@import Rcpp RcppArmadillo foreach iterators 
#'@useDynLib prospect
#'@author Antoine Stevens & Leonardo Ramirez-Lopez
#'
NULL