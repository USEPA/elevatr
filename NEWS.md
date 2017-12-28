elevatr 0.1.4 (201X-0X-XX)
==========================
## Bug Fix
- Primary change with this released is fixing a bug with the return file type on the AWS and mapzen APIs.  "tif" was changed to "tiff" and the check was stopping processing of the raster images.  Details are on <https://github.com/jhollist/elevatr/issues/17>. Thanks to the following individuals for catching this: @yipcma, @TomBor, @jslingsby.  And thanks to @vividbot for <https://github.com/jhollist/elevatr/pull/18> which provided a fix.  

elevatr 0.1.2 (2017-03-13)
==========================

## Bug Fix
- There was a typo in building the mapzen api key.  Was masked prior as a keyless access was allowed.  It no longer is and get_elev_raster was failing.  That has been fixed
- Tests also failing due to keyless access.  Encripted key now pushed for use on travis.  Tests not run on CRAN
- Thanks to @hrbrmstr for pointing me in the right direction on fixing the testing with an api key.
- Also thanks to @noamross and @ropensci for maintaing <https://discuss.ropensci.org> where I found <https://discuss.ropensci.org/t/test-api-wrapping-r-packages-with-oauth-tokens/157>.  And thanks to @jennybc for wrapping all this up and provide great guidance on testing and vignettes that require a key.  That info is here: <https://rawgit.com/jennybc/googlesheets/master/vignettes/managing-auth-tokens.html#encrypting-tokens-for-hosted-continuous-integration>


elevatr 0.1.1 (2017-01-27)
==========================

## Minor Changes
- inst/doc was inadvertently included in package.  This verisons removes that and includes only vignettes.


elevatr 0.1.0 (2017-01-25)
==========================

## Initial CRAN Release
- This is the initial CRAN release. Provides access to point elevation data from USGS and from Mapzen.  Provides access to raster DEM from Mapzen Terrain Tiles and AWS Terrain Tiles.