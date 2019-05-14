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
  min_tile <- latlong_to_tilexy(bbx[1,1],bbx[2,1],z)
  max_tile <- latlong_to_tilexy(bbx[1,2],bbx[2,2],z)
  x_all <- seq(from = floor(min_tile[1]), to = ceiling(max_tile[1]))
  y_all <- seq(from = ceiling(min_tile[2]), to = floor(max_tile[2]))
  return(expand.grid(x_all,y_all))
}

#' function to check input type and projection.  All input types convert to a
#' SpatialPointsDataFrame for point elevation and bbx for raster.
#' @keywords internal
loc_check <- function(locations, prj = NULL){
  
  #Convert sf locations to SP
  if("sf" %in% class(locations)){
    locations <- sf::as_Spatial(locations)
  }
  #browser()
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



#' function to project bounding box and if needed expand it
#' @keywords internal
proj_expand <- function(locations,prj,expand){
  if(!is.null(expand)){
    
    bbx_sp <- bbox_to_sp(sp::bbox(locations), sp::CRS(prj))
    bbx_sp_expand <- rgeos::gBuffer(bbx_sp, width = expand, capStyle = "SQUARE")
    bbx <- sp::bbox(sp::spTransform(bbx_sp_expand, sp::CRS(ll_geo)))
  } else{
    bbx <- sp::bbox(sp::spTransform(locations, sp::CRS(ll_geo)))
  }
  
  bbx
}

#' function to clip the DEM
#' @keywords internal
clip_it <- function(rast, loc, expand, clip){
  
  loc_rast_prj <- sp::spTransform(loc, sp::CRS(sp::proj4string(rast)))
  if(clip == "locations" & !grepl("Points", class(loc_rast_prj))){
    dem <- raster::mask(raster::crop(rast,loc_rast_prj), loc_rast_prj)
  } else if(clip == "bbox" | grepl("Points", class(loc_rast_prj))){
    
    bbx <- proj_expand(loc_rast_prj, sp::proj4string(rast), expand)
    bbx_sp <- sp::spTransform(bbox_to_sp(bbx), 
                              sp::CRS(sp::proj4string(rast)))
    dem <- raster::mask(raster::crop(rast,bbx_sp), bbx_sp)
  }
  dem
}

#' Edited from https://github.com/jhollist/quickmapr/blob/master/R/internals.R
#' sp bbox to poly
#' @param bbx an sp object
#' @param prj an sp CRS object
#' @keywords internal
bbox_to_sp <- function(bbox, prj = sp::CRS(ll_geo)) {
  x <- c(bbox[1, 1], bbox[1, 1], bbox[1, 2], bbox[1, 2], bbox[1, 1])
  y <- c(bbox[2, 1], bbox[2, 2], bbox[2, 2], bbox[2, 1], bbox[2, 1])
  p <- sp::Polygon(cbind(x, y))
  ps <- sp::Polygons(list(p), "p1")
  sp_bbx <- sp::SpatialPolygons(list(ps), 1L , proj4string = prj)
  sp_bbx
}
