
[![Travis](https://travis-ci.org/jhollist/elevatr.svg?branch=master)](https://travis-ci.org/jhollist/elevatr)
[![Appveyor](https://ci.appveyor.com/api/projects/status/github/jhollist/elevatr?svg=true)](https://ci.appveyor.com/project/jhollist/elevatr)
[![](http://www.r-pkg.org/badges/version/elevatr)](http://www.r-pkg.org/pkg/elevatr)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/elevatr)](http://www.r-pkg.org/pkg/elevatr)
[![Coverage Status](https://coveralls.io/repos/github/jhollist/elevatr/badge.svg?branch=master)](https://coveralls.io/github/jhollist/elevatr?branch=master)

# elevatr
An R package for accessing elevation data from various sources

The `elevatr` package currently provides access to elevation data from from Mapzen ([Mapzen Tile Service](https://mapzen.com/documentation/terrain-tiles/) for raster digital elevation models.  For point elevation data, either the [Mapzen Elevation Service](https://mapzen.com/documentation/elevation/elevation-service/) or the [USGS Elevation Point Query Service](http://ned.usgs.gov/epqs/)) may be used. Additional elevation data sources may be added.

Current plan for this package includes just two functions to access elevation web services:

- `get_elev_point()`:  Get point elevations using the Mapzen Elevation Service or the USGS Elevation Point Query Service (for the US Only) .  This will accept a data frame of x (long) and y (lat) or a SpatialPoints/SpatialPointsDataFame as input.  A SpatialPointsDataFrame is returned.
- `get_elev_raster()`: Get elevation data as a raster (e.g. a Digital Elevation Model) from the Mapzen Terrain GeoTIFF Service.  Other sources may be added later.  This will accept a data frame of of x (long) and y (lat) or any `sp` or `raster` object as input and will return a `raster` object of the elevation tiles that cover the bounding box of the input spatial data. 

## Installation

This package is currently in development and should not be considered stable.  The functions and API may change drastically and rapidly and it may not work at any given moment...  That being said, install with `devtools`


```r
library(devtools)
install_github("jhollist/elevatr")
```

## Attribution
Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 content courtesy of the U.S. Geological Survey and ETOPO1 content courtesy of U.S. National Oceanic and Atmospheric Administration.
