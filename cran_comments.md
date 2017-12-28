## Comments
This submission fixes a bug in the get_elev_raster function.  The response type changed on the API side and internal checks were failing.  This is corrected.  Also, three packages were listed as Imports, but not called.  That has been fixed and all NOTES listed in the CRAN checks should be addressed.  

## Test Environments
- r-hub, Ubuntu Linux 16.04 LTS, R-devel, GCC
- Appveyor, Windows Server 2012 R2 x64 (build 9600),  R version 3.4.3 Patched (2017-12-27 r73967)
- Local, Windows 10, R version 3.4.0 (2017-04-21)

## R CMD check results
- No ERRORS or WARNINGS

## Downstream dependencies
There are currently no downstream dependencies