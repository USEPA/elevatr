elevatr 0.3.5.9999 (2021-02-01)
==========================
- Empty rasters were failing as elevatr was using nrow(locations) to get number 
  of features.  It still does that as the default behavior, but if nrow(locations).  
  Thanks for the catch, Gengping Zhu!
  returns a null, it uses length(locations) instead.
- Skipped testing on R version 3.6.2 as it was throwing an error on CRAN.  All 
  other platforms passed.
- Added access to OpenTopography Global Bathymetry SRTM15+ V2.1 with 
  src = "srtm15plus" 
- Documentation fix on get_elev_raster, now correctly reports that the function 
  returns a raster, not points.  Thanks @AndyBunn!
- sfc objects getting missing in coercion in loc_check.  Not anymore!
- serial loop for get_epqs was taking a long time (API returns are slow), so use
  furrr::future_map_dbl to paralellize the gets.  Defaults to 1 minus available 
  cores.
- Added argument to get_eqps to control serial vs parallel API calls.  Defaults 
  to serial for 35 or fewer points, but can be set to TRUE for force serial.
- epqs occasionally times out, but subsequent hits usually work fine.  Added a second
  hit when that happens and if that second one doesn't work then it assigns 
  elevation to NA and throws a warning, instead of erroring
- Was suppressing messages (and thus progress bar) on get_elev_pt src = "aws".
  Turned that off so progress of building the DEM is tracked.
- Added overwrite argument to get_elev_point() to check for existence of elevation
  and elev_units columns.  If either exist and overwrite not TRUE then errors.
- Updated progress bars to use the progressr package.
- proj_expand was using buffers to expand.  Not great for geographic projections.  Now it adds the expansion to the max and subtracts from the min to expand the bbox by the expand value.  For raster retrievals with a single point the resultant raster will be significant smaller than previous (approximate 1km by 1km).  Multiple points should see no difference.

elevatr 0.3.4 (2021-01-21)
==========================
# Errors
- updated tests to deal with build errors
- Parsing crs through st_crs()
- Fixed vignettes


elevatr 0.3.3 (2021-01-08)
==========================

# Bug Fixes
- Rasters were not getting handled correctly by size estimation.  Thanks to @tteo for the catch (https://github.com/jhollist/elevatr/issues/37)
- Single point requests to Open Topography were failing.  Expands now to capture small area around single point
- Switched to using wkt instead of proj4.  I think this will help with the more recent versions of PROJ...
- Added rgdal_show_exportToProj4_warning=thin to options on load.  
- Fixed https://github.com/jhollist/elevatr/issues/38.  Thanks to @cjcarlson, @ACheysson, and @jsta for helping track this one down.

elevatr 0.3.1 (2020-11-09)
==========================

# Added Functionality
- Added user agents to httr requests
- Added new OpenTopography API access to three global datasets, SRTM GL1 and 3, and the  ALOS World 3D 30m.  These are accessed via the src argument
- Added new argument to convert any negative values to NA.  

# Bug Fixes
- Zoom levels 1 and 0 were throwing errors becuase tile selction was overzealous and was selecting tiles that existed.  Conditionals to check fo this.  Also zoom 0 returns as "image/tif", not "image/tiff" that all other levls return.  More robust checking on return type.
- Tiles for points that fall exactly on the equator were returning NA on `get_elev_point()`.  On tile selection in `get_tilexy()` a conditional was added to check for lat == 0 and projeciton being and acceptable proj4 alias of Lat/Long.  If that is met a very small (~ 1meter) expansion to the bounding box is done.  Thanks @willgearty for the bug report <https://github.com/jhollist/elevatr/issues/25>. 
- USGS epqs return -1000000 for areas without an elevation.  `elevatr` now converts those values to NA.  Thanks to George Moroz for the catch! <https://github.com/jhollist/elevatr/issues/24>
- Updated stale sp objects.  Thank you Roger Bivand for making the update very easy!
- Updated links to mapzen documentation.  Thanks to Alec Robitaille for the fix.
- Sped up merging and projecting via PR from Mike Johnson.  Thanks for the contribution, Mike.

elevatr 0.2.0 (2018-11-28)
==========================
# API Changes
- Major change for this is dropping Mapzen support since Mapzen shutdown in January of 2018.  Replacement services for terrain tiles exist at Nextzen, however; the published geotiff endpoints were not working.  I opted to not include the Nextzen endpoints at this time.  Rolled back to terrain tiles from AWS only.

# Added Functionality
- Added point elevations from AWS.  Extract point elevations from a DEM obtained via `get_elev_raster()`.  Will likely be faster for cases with many points in a relatively small geographic area.
- Added a clip argument to `get_elev_raster()`.  Default behavior of returning the full tiles is the same as in prior versions.  The argument expands this by allowing users to clip the resultant DEM either by the bounding box of the input locations via `clip = "bbox"` or by the locations themselves via `clip = "locations"`.  Partly inspired by https://github.com/jhollist/elevatr/issues/13.  Thanks to Michael Sumner (@mdsumner) for the inspiration.     
- Support for input simple features of the class `sf` has been added.  This is supported by coercion of the input `sf` class to a `SpatialXDataFrame`.  An `sf` object is also returned when used as the input locations for `get_elev_point`

# Minor Changes
- Updated Vignette to reflect new focus on AWS and USGS
- Updated Tests
- Updated README
- Added message to inform user of vertical units and CRS.


elevatr 0.1.4 (2017-12-28)
==========================
## Bug Fixes
- Primary change with this released is fixing a bug with the return file type on the AWS and mapzen APIs.  "tif" was changed to "tiff" and the check was stopping processing of the raster images.  Details are on <https://github.com/jhollist/elevatr/issues/17>. Thanks to the following individuals for catching this: @yipcma, @TomBor, @jslingsby.  And thanks to @vividbot for <https://github.com/jhollist/elevatr/pull/18> which provided a fix.  
- Thanks to @pascalfust for this issue: <https://github.com/USEPA/elevatr/issues/2>.  Kicked me into gear to send fix to CRAN.
- Fixed NOTE on CRAN: Packages in Imports, not imported.
    - Removed prettyunits
    - moved rgdal to suggests
    - Changed where ratelimitr getting called (was not in a function so couldn't be exported/called.
- Fixed travis build errors caused by change in elevation API that now requires a key.
- Added deprecation message to get_elev_point and get_elev_raster, due to pending shutdown of Mapzen :(

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