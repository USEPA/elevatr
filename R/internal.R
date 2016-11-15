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
  max_tile <- ceiling(latlong_to_tilexy(bbx[1,2],bbx[2,2],z))
  return(expand.grid(min_tile[1]:max_tile[1],min_tile[2]:max_tile[2]))
}

#' function to check input type and projection.  All input types convert to a
#' SpatialPointsDataFrame for point elevation and bbx for raster.
#' @keywords internal
loc_check <- function(locations, prj = NULL){
  if(class(locations)=="data.frame"){ 
    if(is.null(prj)){
      stop("Please supply a valid proj.4 string.")
    }
    locations<-sp::SpatialPointsDataFrame(sp::coordinates(locations),
                             proj4string = sp::CRS(prj),
                             data = data.frame(elevation = 
                                                 vector("numeric",
                                                        nrow(locations))))
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
    if(is.na(sp::proj4string(locations))){
      sp::proj4string(locations)<-prj
    }
  }
locations
} 
  

#' function to get api key based on source
#' @keywords internal
get_api_key<-function(src){
  if(src == "mapzen"){
    return(getOption("mapzen_key"))
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
  bbx <- sp::bbox(sp::spTransform(sp::SpatialPoints(t(sp::coordinates(bbx)),bbox=bbx,
                                   proj4string = sp::CRS(prj)),
                     sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
  bbx
}

#' function to break up larger requests into smaller ones and not go afoul of
#' mapzen API limits
#' @keywords internal

#resp <- httr::GET(url)
#if (httr::http_type(resp) != "application/json") {
#  stop("API did not return json", call. = FALSE)
#} 
#resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
#                           simplifyVector = FALSE)
#locations$elevation <- unlist(resp$height)