
[![R build
status](https://github.com/jhollist/elevatr/workflows/R-CMD-check/badge.svg)](https://github.com/jhollist/elevatr/actions)
[![](https://www.r-pkg.org/badges/version/elevatr)](https://www.r-pkg.org/pkg/elevatr)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/elevatr)](https://www.r-pkg.org/pkg/elevatr)
[![Codecov test
coverage](https://codecov.io/gh/jhollist/elevatr/branch/main/graph/badge.svg)](https://codecov.io/gh/jhollist/elevatr?branch=main)
[![DOI](https://zenodo.org/badge/65325400.svg)](https://zenodo.org/badge/latestdoi/65325400)

# elevatr

An R package for accessing elevation data from various sources

The `elevatr` package currently provides access to elevation data from
AWS Open Data [Terrain
Tiles](https://registry.opendata.aws/terrain-tiles/) and the Open
Topography [Global datasets
API](https://opentopography.org/developers#API) for raster digital
elevation models. For point elevation data,the [USGS Elevation Point
Query Service](https://nationalmap.gov/epqs/)) may be used or the point
elevations may be derived from the AWS Tiles. Additional elevation data
sources may be added as they come available.

Currently this package includes just two primary functions to access
elevation web services:

-   `get_elev_point()`: Get point elevations using the USGS Elevation
    Point Query Service (for the US Only) or using the AWS Terrian Tiles
    (global). This will accept a data frame of x (long) and y (lat), a
    SpatialPoints/SpatialPointsDataFame, or a Simple Features object as
    input. A SpatialPointsDataFrame or Simple Features object is
    returned, depending on the class of the input locations.
-   `get_elev_raster()`: Get elevation data as a raster (e.g. a Digital
    Elevation Model) from the AWS Open Data Terrain Tiles. Other sources
    may be added later. This will accept a data frame of of x (long) and
    y (lat) or any `sp` or `raster` object as input and will return a
    `raster` object of the elevation tiles that cover the bounding box
    of the input spatial data.

## Installation

Version 0.4.1 of this package is currently available from CRAN and may
be installed by:

``` r
install.packages("elevatr")
```

The development version (this repo) may installed with `devtools`:

``` r
library(devtools)
install_github("jhollist/elevatr")
```

## Contributions and Use Cases

As `elevatr` is getting more widely used, there have been some great
contributions, use cases, and additional packages that have come from
the user community. If you have one you’d like to share, let me know and
I will gladly add it. Here are some of the ones that I have seen. Thank
you all!

-   [Michaela Mulhearn’s `rayshader` and `elevatr`
    cheatsheet](contributions/mulhearn_rayshader_elevatr_cheatsheet.pdf)
-   [researchremora on twitter has created some amazing elevation
    maps](https://mobile.twitter.com/researchremora/status/1415119197441564672)
-   [Hugh Graham’s `rayvista` package uses `rayshader`, `maptiles`, and
    `elevatr` to create some cool
    visualizations](https://github.com/h-a-graham/rayvista)

## Attribution

Mapzen terrain tiles (which supply the AWS source) contain 3DEP, SRTM,
and GMTED2010 content courtesy of the U.S. Geological Survey and ETOPO1
content courtesy of U.S. National Oceanic and Atmospheric
Administration. The Open Topography API provide access to the SRTM and
the ALOS World 3D datasets. See <https://opentopography.org/> for
details.

## Repositories

The source code for this repository is maintained at
<https://github.com/jhollist/elevatr> which is also mirrored at
<https://github.com/usepa/elevatr>

## EPA Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project
code is provided on an “as is” basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity , confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.
