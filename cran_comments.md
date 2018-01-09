## Comments
This submission fixes a bug in the get_elev_raster function.  The response type changed on the API side and internal checks were failing.  This is corrected.  Also, three packages were listed as Imports, but not called.  That has been fixed and all NOTES listed in the CRAN checks should be addressed.  Lastly, a deprecation notice for the mapzen api (which is shutting down soon) was added.

## Test Environments
- Travis-CI, Ubuntu 14.04.5 LTS, R 3.4.2 (2017-01-27)
- r-hub, x86_64-redhat-linux-gnu (64-bit), 3.4.2 (2017-09-28) 
- Appveyor, Windows Server 2012 R2 x64 (build 9600),  R version 3.4.3 Patched (2018-01-07 r74099)
- Local, Windows 10, R version 3.4.0 (2017-04-21)

## R CMD check results
- No ERRORS or WARNINGS

## Downstream dependencies
There are currently no downstream dependencies