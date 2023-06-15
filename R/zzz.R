op <- options()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"elevatr v1+ NOTE: Version 1.0+ of 'elevatr' use 'sf' and 'terra'.  
The 'sp' and 'raster' packages are no longer supported.")
}

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
