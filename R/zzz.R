op <- options()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"elevatr v1+ NOTE: Version 1.0+ of 'elevatr' use 'sf' and 'terra'.  
Support for the 'sp' and 'raster' packages is being deprecated; however, 
get_elev_raster continues to return a RasterLayer.  This will be dropped in
future versions, so please plan accordingly.")
}

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
