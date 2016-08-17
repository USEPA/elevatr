# elevatr
An R package for accessing elevation data

This package will provide an interface to the 
[USGS Elevation Point Query Service](http://ned.usgs.gov/epqs/) 
for US only elevation and for global data the SRTM REST service from 
[OpenTopography](http://www.opentopography.org/developers#SRTM).  Other 
elevation services may be added at a later date.  The interface expects a data
frame as entry and will return a data frame.  Input data frame should consist of
two columns, Longitude and Latitude.  Output data frame will contain a third 
columns with the elevation for that location.