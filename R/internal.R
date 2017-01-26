#' function to convert lat long to xyz tile with decimals
#' rounding to tile occurs in \code{get_tilexy}
#' @keywords internal
latlong_to_tilexy <- function(lon_deg, lat_deg, zoom){
  #Code from http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Coordinates_to_tile_numbers_2
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- (lon_deg + 180.0) / 360.0 * n
  ytile <- (1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n
  return(c(xtile, ytile))
}

#' function to get a data.frame of all xyz tiles to download
#' @keywords internal
get_tilexy <- function(bbx,z){
  min_tile <- floor(latlong_to_tilexy(bbx[1,1],bbx[2,1],z))
  max_tile <- floor(latlong_to_tilexy(bbx[1,2],bbx[2,2],z))
  x_all <- seq(from = min_tile[1], to = max_tile[1])
  y_all <- seq(from = min_tile[2], to = max_tile[2])
  return(expand.grid(x_all,y_all))
}

#' function to check input type and projection.  All input types convert to a
#' SpatialPointsDataFrame for point elevation and bbx for raster.
#' @keywords internal
loc_check <- function(locations, prj = NULL){
  if(class(locations)=="data.frame"){ 
    if(is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    if(ncol(locations) > 2){
      df <- data.frame(locations[,3:ncol(locations)],
                       vector("numeric",nrow(locations)))
      names(df) <- c(names(locations)[3:ncol(locations)],
                     "elevation")
    } else {
      df <- data.frame(vector("numeric",nrow(locations)))
      names(df) <- "elevation"
    }
    locations<-sp::SpatialPointsDataFrame(sp::coordinates(locations[,1:2]),
                             proj4string = sp::CRS(prj),
                             data = df)
  } else if(class(locations) == "SpatialPoints"){
    if(is.na(sp::proj4string(locations))& is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    if(is.na(sp::proj4string(locations))){
      sp::proj4string(locations)<-prj
    }
    locations<-sp::SpatialPointsDataFrame(locations,
                                          data = data.frame(elevation = 
                                                              vector("numeric",
                                                                     nrow(sp::coordinates(locations)))))
  } else if(class(locations) == "SpatialPointsDataFrame"){
    if(is.na(sp::proj4string(locations)) & is.null(prj)) {
      stop("Please supply a valid proj.4 string.")
    }
    if(is.na(sp::proj4string(locations))){
      sp::proj4string(locations)<-prj
    }
    locations@data <- data.frame(locations@data,
                                 elevation = vector("numeric",nrow(locations))) 
  } else if(attributes(class(locations)) %in% c("raster","sp")){
    if(is.na(sp::proj4string(locations)) & is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    #browser()
    if(is.na(sp::proj4string(locations))){
      if(attributes(class(locations)) == "raster"){
        if(sum(!is.na(raster::getValues(locations))) == 0){
          stop("No distinct points, all values NA.")
        } else {
          locations <- raster::rasterToPoints(locations,spatial = TRUE)
          sp::proj4string(locations)<-prj
        }
      } 
    }
  }
locations
} 
  

#' function to get API key based on source
#' @keywords internal
get_api_key<-function(src){
  if(src == "mapzen"){
    key <- Sys.getenv("mapzen_key")
    if(nchar(key) == 0) {
      key <- NULL
    }
    return(key)
  }
}

#' function to project bounding box and if needed expand it
#' @keywords internal
proj_expand <- function(bbx,prj,expand){
  if(!is.null(expand)){
    bbx <- bbx
    bbx[,1] <- bbx[,1] - expand
    bbx[,2] <- bbx[,2] + expand
  }
  bbx <- sp::bbox(sp::spTransform(sp::SpatialPoints(t(sp::coordinates(bbx)),
                                                    bbox=bbx, proj4string = sp::CRS(prj)),
                     sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
  bbx
}

#' based on https://mapzen.com/documentation/overview/#rate-limits
#' limits for keyless access are 1/second and 6/minute
#' see issue #4
#' @keywords internal
mapzen_elev_GET_nokey <- ratelimitr::limit_rate(
  httr::GET, 
  ratelimitr::rate(n = 1, period = 2), ratelimitr::rate(n = 5, period = 100))

#' based on https://mapzen.com/documentation/overview/#rate-limits
#' limits for valid keyholders are 2/second and 20K/day
#' this function will only enforce the 2/second limit
#' see issue #4
#' @keywords internal
mapzen_elev_GET_withkey <- ratelimitr::limit_rate(
  httr::GET,
  ratelimitr::rate(n = 2, period = 1)
)
