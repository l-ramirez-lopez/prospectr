# .PROSPECTR_CACHE <- new.env(FALSE, parent = globalenv())

.onAttach <- function(lib, pkg) {
    #assign("gpclib", FALSE, envir=.prospectr_CACHE)
    prospectr.v <- read.dcf(file = system.file("DESCRIPTION", package=pkg), fields="Version")
    packageStartupMessage(paste(pkg, "version", prospectr.v, "-- 'waving'"))
    packageStartupMessage("check the github repository at http://github.com/l-ramirez-lopez/prospectr")
}

# .onUnload <- function(libpath) {
#     rm(.prospectr_CACHE)
# }
