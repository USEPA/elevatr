
[![R build
status](https://github.com/jhollist/elevatr/workflows/R-CMD-check/badge.svg)](https://github.com/jhollist/elevatr/actions)
[![](https://www.r-pkg.org/badges/version/elevatr)](https://www.r-pkg.org/pkg/elevatr)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/elevatr)](https://www.r-pkg.org/pkg/elevatr)
[![Codecov test
coverage](https://codecov.io/gh/jhollist/elevatr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jhollist/elevatr?branch=main)
[![DOI](https://zenodo.org/badge/65325400.svg)](https://zenodo.org/badge/latestdoi/65325400)

# Key information about version 0.99.0 and upcoming versions of `elevatr`

Several major changes have been made to `elevatr` in response to the
retirement of legacy spatial packages (see
<https://r-spatial.org/r/2023/05/15/evolution4.html> for details).
Version 0.99.0 has switched to using `sf` and `terra` for all data
handling; however, in this version a `raster RasterLayer` is still
returned from `get_elev_raster()`. Additional changes are planned for
version 1+, most notably the return for `get_elev_raster()` will be a
`terra SpatRaster`. Please plan accordingly for your analyses and/or
packages account for this change.

# elevatr

An R package for accessing elevation data from various sources

The `elevatr` package currently provides access to elevation data from
AWS Open Data [Terrain
Tiles](https://registry.opendata.aws/terrain-tiles/) and the Open
Topography [Global datasets
API](https://opentopography.org/developers#API) for raster digital
elevation models. For point elevation data,the [USGS Elevation Point
Query Service](https://apps.nationalmap.gov/epqs/)) may be used or the
point elevations may be derived from the AWS Tiles. Additional elevation
data sources may be added as they come available.

Currently this package includes just two primary functions to access
elevation web services:

- `get_elev_point()`: Get point elevations using the USGS Elevation
  Point Query Service (for the US Only) or using the AWS Terrain Tiles
  (global). This will accept a data frame of x (long) and y (lat), a
  Simple Features object, or `terra` SpatRaster as input. A Simple
  Features object is returned of the point locations and elevations.
- `get_elev_raster()`: Get elevation data as a raster (e.g. a Digital
  Elevation Model) from the AWS Open Data Terrain Tiles or Open
  Topography Global datasets. Other sources may be added later. This
  will accept a data frame of of x (long) and y (lat) or any `sf` or
  `terra` SpatRaster object as input and will return a terra
  `SpatRaster` object of the elevation. The extent of the SpatRaster is
  the full tiles that cover the bounding box of the input spatial data,
  but may be clipped to the shape or bounding box of the input
  locations.

## Installation

Version 0.99.0 of this package is currently available from CRAN and may
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

- [Michaela Mulhearn’s `rayshader` and `elevatr`
  cheatsheet](https://github.com/jhollist/elevatr/blob/main/contributions/mulhearn_rayshader_elevatr_cheatsheet.pdf)
- researchremora on twitter has created some amazing elevation maps
- [Hugh Graham’s `rayvista` package uses `rayshader`, `maptiles`, and
  `elevatr` to create some cool
  visualizations](https://github.com/h-a-graham/rayvista)
- [Spencer Schien has built some fantastic shaded relief visulaizations
  and provided the code to recreate
  them](https://github.com/Pecners/rayshader_portraits)

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
