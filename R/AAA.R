# .PROSPECTR_CACHE <- new.env(FALSE, parent = globalenv())

.onAttach <- function(lib, pkg) {
  # assign("gpclib", FALSE, envir=.prospectr_CACHE)
  prospectr.v <- read.dcf(
    file = system.file("DESCRIPTION", package = pkg),
    fields = "Version"
  )
  packageStartupMessage(paste(pkg, "version", prospectr.v, "-- 'positive'"))
  mss2 <- paste0(
    "check the github repository at: ",
    "http://github.com/l-ramirez-lopez/prospectr"
  )
  packageStartupMessage(mss2)
}

# .onUnload <- function(libpath) {
#     rm(.prospectr_CACHE)
# }
