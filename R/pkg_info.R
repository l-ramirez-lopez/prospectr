#' @title Get the package version info
#' @description returns package info.
#' @param pkg the package name i.e "prospectr"
#' @keywords internal
pkg_info <- function(pkg = "prospectr") {
  fld <- c("Version", "Config/VersionName", "URL")
  pinfo <- read.dcf(system.file("DESCRIPTION", package = pkg), fields = fld)
  pinfo
}