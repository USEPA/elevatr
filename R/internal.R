#' function to convert lat long to xyz tile
latlong_to_tilexy <- function(lat_deg, lon_deg, zoom){
  #Code from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Coordinates_to_tile_numbers_2
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return( c(xtile, ytile))
}

get_tilexy <- function(bbx,z){
  minmin <- bbx[,1] 
  minmax <- c(bbx[1,1],bbx[2,2])
  maxmin <- c(bbx[1,2],bbx[2,1])
  maxmax <- bbx[,2]
  latlong_to_tilexy()
}