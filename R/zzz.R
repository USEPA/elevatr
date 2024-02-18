op <- options()

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
"elevatr v0.99.0 NOTE: Version 0.99.0 of 'elevatr' uses 'sf' and 'terra'.  Use 
of the 'sp', 'raster', and underlying 'rgdal' packages by 'elevatr' is being 
deprecated; however, get_elev_raster continues to return a RasterLayer.  This 
will be dropped in future versions, so please plan accordingly.")
}

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
