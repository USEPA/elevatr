op <- options()

.onLoad <- function(libname, pkgname){
  options("rgdal_show_exportToProj4_warnings"="thin")
  invisible()
}

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
