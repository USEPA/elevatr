# elevatr
An R package for accessing elevation data

There are many sources for elevation data and these are offered up via a variety of services.  The `elevatr` package will provide access to some of these.  Two interfaces will be provided to access the data. First is via a data.frame of locations which will return a data.frame of locations and the elevation and second will be via a `raster` `Extent` object which will return a `RasterLayer` object of the elevation for that area.  Data sets currently included in `elevatr` include:

|Dataset|Access via data.frame|Access via Extent|
|-------|---------------------|-----------------|
|Shttule Radar Topography Mission version 3 | [GeoNames](http://www.geonames.org/export/web-services.html#srtm3) using the [`geonames` package](https://github.com/ropensci/geonames) |[OpenTopography](http://www.opentopography.org/developers#SRTM)|
|USGS National Elevation Dataset (NED)| [USGS Elevation Point Query Service](http://ned.usgs.gov/epqs/) | Not available at this time. | 


