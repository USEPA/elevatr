op <- options()

.onLoad <- function(libname, pkgname){
  options("rgdal_show_exportToProj4_warnings"="thin")
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"elevatr v0.4.5 NOTE: This is the last version of 'elevatr' that will use the 
'sp' and 'raster' packages. The next release will switch to 'sf' and 'terra'.")
}

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
