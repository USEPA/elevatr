op <- options()

.onUnload <- function(libname, pkgname){
  options(op)
  invisible()
}
