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
  if(z == 1){
    x_all <- x_all[x_all<2]
    y_all <- y_all[y_all<2]
  } else if(z == 0){
    x_all <- x_all[x_all<1]
    y_all <- y_all[y_all<1]
  }
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
 
  lll <- grepl("longlat",prj) |
    grepl("lonlat",prj) |
    grepl("latlong",prj) |
    grepl("latlon",prj)
  
  if(any(locations@bbox[2,] == 0) & lll & is.null(expand)){
    # Edge case for lat exactly at the equator - was returning NA
    # Expansion of bbox is approximately one meter
    expand <- 0.00001
  } 
  
  bbx <- bbox_to_sp(sp::bbox(locations), prj = prj)
  
  if(!is.null(expand)){
    bbx <- methods::as(sf::st_buffer(sf::st_as_sf(bbx), expand), "Spatial")
  }
  
  bbx <- sp::bbox(sp::spTransform(bbx, sp::CRS(ll_geo)))
  bbx
  
}

#' function to clip the DEM
#' @keywords internal
clip_it <- function(rast, loc, expand, clip){
  loc_wm <- sp::spTransform(loc, raster::crs(rast))
  if(clip == "locations" & !grepl("Points", class(loc_wm))){
    dem <- raster::mask(raster::crop(rast,loc_wm), loc_wm)
  } else if(clip == "bbox" | grepl("Points", class(loc_wm))){
    bbx <- proj_expand(loc_wm, as.character(raster::crs(rast)), expand)
    bbx_sp <- sp::spTransform(bbox_to_sp(bbx), raster::crs(rast))
    dem <- raster::mask(raster::crop(rast,bbx_sp), bbx_sp)
  }
  dem
}

#' Edited from https://github.com/jhollist/quickmapr/blob/master/R/internals.R
#' Assumes geographic projection
#' sp bbox to poly
#' @param bbx an sp bbox object
#' @param prj defaults to "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' @keywords internal
bbox_to_sp <- function(bbox, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {
  x <- c(bbox[1, 1], bbox[1, 1], bbox[1, 2], bbox[1, 2], bbox[1, 1])
  y <- c(bbox[2, 1], bbox[2, 2], bbox[2, 2], bbox[2, 1], bbox[2, 1])
  p <- sp::Polygon(cbind(x, y))
  ps <- sp::Polygons(list(p), "p1")
  sp_bbx <- sp::SpatialPolygons(list(ps), 1L, proj4string = sp::CRS(prj))
  sp_bbx
}

#' Estimate download size of DEMs
#' @param locations the locations
#' @param src the src
#' @z zoom level if source is aws
#' @keywords internal
estimate_raster_size <- function(locations, src, z = NULL){
 
  locations <- sp::spTransform(locations, sp::CRS("+init=EPSG:4326"))
  # Estimated cell size from zoom level source
  # https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#sources-native-resolution
  z_res <- data.frame(z = 0:14, res_dd = c(0.54905236, 0.27452618, 0.15455633, 
                                           0.07145545, 0.03719130, 0.01901903, 
                                           0.00962056, 0.00483847, 0.00241219, 
                                           0.00120434, 0.00060173, 0.00030075, 
                                           0.00015035, 0.00007517, 0.00003758))

  bits <- switch(src,
                 aws = 32,
                 gl3 = 32,
                 gl1 = 32,
                 alos = 32)
  if(src == "aws"){
    res <- z_res[z_res$z == z,]$res_dd
  } else{
    res <- switch(src,
                  gl3 = 0.0008333,
                  gl1 = 0.0002778,
                  alos = 0.0002778) 
  }
  num_rows <- (sp::bbox(locations)[1, "max"] - sp::bbox(locations)[1, "min"])/res
  num_cols <- (sp::bbox(locations)[2, "max"] - sp::bbox(locations)[2, "min"])/res
  
  num_megabytes <- (num_rows * num_cols * bits)/8388608
  num_megabytes
}
