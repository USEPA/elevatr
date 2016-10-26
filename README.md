# elevatr
An R package for accessing elevation data from [Mapzen Tile Service](https://mapzen.com/documentation/terrain-tiles/) and the [Mapzen Elevation Service](https://mapzen.com/documentation/elevation/elevation-service/)

Two services available from Mapzen.com provide access to elevation data as a raster digital elevation model or as height at a single point.  This package provides access to those services and returns elevation data either as a data frame (from the Mapzen Elevation Service) or as a raster object (from the Mapzen Terrain Service).

## Installation

This package is currently in development and should not be considered stable.  The functions and API may change drastically and rapidly and it may not work at any given moment...  That being said, install with `devtools`


```r
library(devtools)
install_github("jhollist/elevatr")
```

## Attribution
Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 content courtesy of the U.S. Geological Survey and ETOPO1 content courtesy of U.S. National Oceanic and Atmospheric Administration.
