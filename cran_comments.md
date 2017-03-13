## Comments
This submission fixes a bug in the get_elev_raster function.  The API key for mapzen had a typo.  This also required some changes to testing the package.  The package is now tested locally and via travis-ci.  The tests are skipped on CRAN.  

## Test Environments
- Ubuntu 12.04, travis-ci, R Under development (unstable) (2017-03-13 r72338)
- Windows Server 2012 R2 x64 (build 9600), Appveyor, R version 3.3.3 Patched (2017-03-06 r72327)
- Red Hat 6.8, local, R version 3.3.2 (2016-10-31)

## R CMD check results
- No ERRORS or WARNINGS

## Downstream dependencies
There are currently no downstream dependencies