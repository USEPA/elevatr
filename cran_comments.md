## Comments
This submission fixes a bug in the get_elev_raster function.  The response type changed on the API side and internal checks were failing.  This is corrected.  Also, three packages were listed as Imports, but not called.  That has been fixed and all NOTES listed in the CRAN checks should be addressed.  

## Test Environments
- r-hub, Ubuntu Linux 16.04 LTS, R-devel, GCC
- r-hub, Fedora Linux, R-devel, GCC
- r-hub, Windows Server 2008 R2 SP1, R-release, 32/64 bit

## R CMD check results
- No ERRORS or WARNINGS

## Downstream dependencies
There are currently no downstream dependencies