# .PROSPECTR_CACHE <- new.env(FALSE, parent = globalenv())

.onAttach <- function(lib, pkg) {
  # assign("gpclib", FALSE, envir=.prospectr_CACHE)
  prospectr_v <- read.dcf(
    file = system.file("DESCRIPTION", package = pkg),
    fields = "Version"
  )
  mss <- paste0(
    "\033[34m",
    pkg, " version ",
    prospectr_v,
    " -- \033[39m'chicago'"
  )

  mss2 <- paste0(
    "\033[34mcheck the github repository at: ",
    "https://github.com/l-ramirez-lopez/prospectr/\033[39m"
  )

  packageStartupMessage(mss)
  packageStartupMessage(mss2)
}

# .onUnload <- function(libpath) {
#     rm(.prospectr_CACHE)
# }
