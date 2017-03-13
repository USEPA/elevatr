elevatr 0.1.2 (2017-03-13)
==========================

## Bug Fix
- There was a typo in building the mapzen api key.  Was masked prior as a keyless access was allowed.  It no longer is and get_elev_raster was failing.  That has been fixed
- Tests also failing due to keyless access.  Encripted key now pushed for use on travis.  Tests not run on CRAN


elevatr 0.1.1 (2017-01-27)
==========================

## Minor Changes
- inst/doc was inadvertently included in package.  This verisons removes that and includes only vignettes.


elevatr 0.1.0 (2017-01-25)
==========================

## Initial CRAN Release
- This is the initial CRAN release. Provides access to point elevation data from USGS and from Mapzen.  Provides access to raster DEM from Mapzen Terrain Tiles and AWS Terrain Tiles.