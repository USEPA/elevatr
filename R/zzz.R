op <- options()

.onLoad <- function(libname, pkgname){
  options("rgdal_show_exportToProj4_warnings"="thin")
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Version 0.4.5 of elevatr is the last version that will \n
                        use the 'sp' and 'raster' packages. The next release \n
                        will swith to 'sf' and 'terra'.")
}

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
