# .PROSPECTR_CACHE <- new.env(FALSE, parent = globalenv())

.onAttach <- function(lib, pkg) {
  # assign("gpclib", FALSE, envir=.prospectr_CACHE)
  # prospectr_v <- read.dcf(
  #   file = system.file("DESCRIPTION", package = pkg),
  #   fields = "Version"
  # )
  prospectr_v <- pkg_info()

  mss <- paste0(
    "\033[34m",
    pkg, " version ",
    paste(resemble_v[1:2], collapse = " -- "),
    "\033[39m"
  )

  mss2 <- paste0(
    "\033[34mcheck the github repository at: ",
    prospectr_v[, "URL"],
    "\033[39m"
  )

  packageStartupMessage(mss)
  packageStartupMessage(mss2)
}

# .onUnload <- function(libpath) {
#     rm(.prospectr_CACHE)
# }
