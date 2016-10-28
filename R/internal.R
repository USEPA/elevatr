#' function to convert lat long to xyz tile
latlong_to_tilexy <- function(lat_deg, lon_deg, zoom){
  #Code from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Coordinates_to_tile_numbers_2
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return(c(xtile, ytile))
}

#' function to get a data.frame of all xyz tiles to download
get_tilexy <- function(bbx,z){
  min_tile <- latlong_to_tilexy(bbx[2,1],bbx[1,1],z)
  max_tile <- latlong_to_tilexy(bbx[2,2],bbx[1,2],z)
  return(expand.grid(min_tile[1]:max_tile[1],min_tile[2]:max_tile[2]))
}

#' function to check input type and projection.  All input types convert to a
#' SpatialPointsDataFrame for point elevation and bbx for raster.
loc_check <- function(locations, prj = NULL){
  if(class(locations)=="data.frame"){ 
    if(is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    locations<-sp::SpatialPointsDataFrame(sp::coordinates(locations),
                             proj4string = CRS(prj),
                             data = data.frame(elevation = 
                                                 vector("numeric",
                                                        nrow(locations))))
  } else if(class(locations) == "SpatialPoints"){
    if(is.na(sp::proj4string(locations))& is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    if(is.na(sp::proj4string(locations))){
      proj4string(locations)<-prj
    }
    locations<-sp::SpatialPointsDataFrame(locations,
                                          data = data.frame(elevation = 
                                                              vector("numeric",
                                                                     nrow(coordinates(locations)))))
  } else if(class(locations) == "SpatialPointsDataFrame"){
    if(is.na(sp::proj4string(locations)) & is.null(prj)) {
      stop("Please supply a valid proj.4 string.")
    }
    if(is.na(sp::proj4string(locations))){
      proj4string(locations)<-prj
    }
    locations@data <- data.frame(locations@data,
                                 elevation = vector("numeric",nrow(locations))) 
  } else if(attributes(class(locations)) %in% c("raster","sp")){
    if(is.na(sp::proj4string(locations)) & is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    if(is.na(sp::proj4string(locations))){
      proj4string(locations)<-prj
    }
  }
locations
} 
  

#' function to get api key based on source
get_api_key<-function(src){
  if(src == "mapzen"){
    return(getOption("mapzen_key"))
  }
}