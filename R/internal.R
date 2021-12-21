#' function to convert lat long to xyz tile with decimals
#' rounding to tile occurs in \code{get_tilexy}
#' @keywords internal
latlong_to_tilexy <- function(lon_deg, lat_deg, zoom){
  # Code assumes lon is 180 to 180, so converts to that
  lon_deg <- ifelse(lon_deg > 180, lon_deg - 360, lon_deg)
  #Code from https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Coordinates_to_tile_numbers_2
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
  x_all <- seq(from = floor(min_tile[1]), to = floor(max_tile[1]))
  y_all <- seq(from = floor(min_tile[2]), to = floor(max_tile[2]))
  
  if(z == 1){
    x_all <- x_all[x_all<2]
    y_all <- y_all[y_all<2]
  } else if(z == 0){
    x_all <- x_all[x_all<1]
    y_all <- y_all[y_all<1]
  }
  
  
  return(expand.grid(x_all,y_all))
}

#' function to get a data.frame of all xyz tiles to download
#' @keywords internal
get_tilexy_coords <- function(locations,z){
  coords <- sp::coordinates(locations)
  
  tiles <- latlong_to_tilexy(coords[,1],coords[,2],z)
  tiles <- matrix(tiles, nrow = nrow(coords), ncol = 2)
  tiles <- floor(tiles)
  tiles <- unique(tiles)
  
  tiles
}



#' function to check input type and projection.  All input types convert to a
#' SpatialPointsDataFrame for point elevation and bbx for raster.
#' @keywords internal
loc_check <- function(locations, prj = NULL){
  
  if(is.null(nrow(locations))){
    nfeature <- length(locations) 
  } else {
    nfeature <- nrow(locations)
  }
  
  if(any(class(locations)=="data.frame")){ 
    if(is.null(prj) & !any(class(locations) %in% c("sf", "sfc", "sfg"))){
      stop("Please supply a valid sf crs via locations or prj.")
    }
    
    locations <- sf::st_as_sf(x = locations, coords = c("x", "y"), crs = prj)
    locations$elevation <- rep(0, nfeature)
    
  } else if(attributes(class(locations)) %in% c("raster")){
    
    raster_crs <- raster::crs(locations)
    
    if((is.null(raster_crs) | is.na(raster_crs)) & is.null(prj)){
      stop("Please supply a valid crs via locations or prj.")
    }
    
    if(is.null(raster_crs) | is.na(raster_crs)){
        if(attributes(class(locations)) == "raster"){
          if(sum(!is.na(raster::getValues(locations))) == 0){
            stop("No distinct points, all values NA.")
          } else {
            
            locations <- unique(data.frame(raster::rasterToPoints(locations)))
            locations$elevation <- vector("numeric", nrow(locations))
            locations<-sf::st_as_sf(x = locations, coords = c("x", "y"), 
                                    crs = st_crs(prj))
          }
        }
    } else if(attributes(class(locations)) %in% c("raster")){
      
      locations <- unique(data.frame(raster::rasterToPoints(locations)))
      locations$elevation <- vector("numeric", nrow(locations))
      locations <- sf::st_as_sf(x = locations, coords = c("x", "y"),
                                crs = raster_crs)
    }
  }
  
  #check for long>180
  lll <- any(grepl("GEOGCRS",sf::st_crs(prj)) |
               grepl("GEODCRS", sf::st_crs(prj)) |
               grepl("GEODETICCRS", sf::st_crs(prj)) |
               grepl("GEOGRAPHICCRS", sf::st_crs(prj)) |
               grepl("longlat", sf::st_crs(prj)) |
               grepl("latlong", sf::st_crs(prj)) |
               grepl("4326", sf::st_crs(prj)))
  if(lll){
    if(any(sf::st_coordinates(locations)[,1]>180)){
      stop("The elevatr package requires longitude in a range from -180 to 180.")
    } 
  }
  
locations
} 



#' function to project bounding box and if needed expand it
#' @keywords internal
proj_expand <- function(locations,prj,expand){
  
  lll <- any(grepl("GEOGCRS",sf::st_crs(prj)) |
               grepl("GEODCRS", sf::st_crs(prj)) |
               grepl("GEODETICCRS", sf::st_crs(prj)) |
               grepl("GEOGRAPHICCRS", sf::st_crs(prj)) |
               grepl("longlat", sf::st_crs(prj)) |
               grepl("latlong", sf::st_crs(prj)) |
               grepl("4326", sf::st_crs(prj)))
  
  if(is.null(nrow(locations))){
    nfeature <- length(locations) 
  } else {
    nfeature <- nrow(locations)
  }
  
  if(any(sp::bbox(locations)[2,] == 0) & lll & is.null(expand)){
    # Edge case for lat exactly at the equator - was returning NA
    expand <- 0.01
  } else if(nfeature == 1 & lll & is.null(expand)){
    # Edge case for single point and lat long
    expand <- 0.01
  } else if(nfeature == 1 & is.null(expand)){
    # Edge case for single point and projected
    # set to 1000 meters
    unit <- st_crs(sf::st_as_sf(locations), parameters = TRUE)$ud_unit
    expand <- units::set_units(units::set_units(1000, "m"), unit, 
                               mode = "standard")
    expand <- as.numeric(expand)
  }
 
  #
  
  if(!is.null(expand)){
    #bbx <- methods::as(sf::st_buffer(sf::st_as_sf(bbx), expand), "Spatial")
    bbx <- sp::bbox(locations) + c(-expand, -expand, expand, expand)
  } else {
    bbx <- sp::bbox(locations)
  }
  bbx <- bbox_to_sp(bbx, prj = prj)
  bbx <- sp::bbox(sp::spTransform(bbx, sp::CRS(ll_geo)))
  bbx
  
  #sf expand - save for later
  #loc_sf <- sf::st_as_sf(locations)
  #loc_bbx <- sf::st_bbox(loc_sf)
  #bbx_sf <- loc_bbx + c(-expand,-expand,expand,expand)
  #names(bbx_sf) <- c("xmin", "ymin", "xmax", "ymax")
  #attr(bbx_sf, "class") <- "bbox"
  #bbx_sf
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
#' @param prj defaults to "EPSG:4326"
#' @keywords internal
#' @importFrom sp wkt
bbox_to_sp <- function(bbox, prj = "EPSG:4326") {
  x <- c(bbox[1, 1], bbox[1, 1], bbox[1, 2], bbox[1, 2], bbox[1, 1])
  y <- c(bbox[2, 1], bbox[2, 2], bbox[2, 2], bbox[2, 1], bbox[2, 1])
  p <- sp::Polygon(cbind(x, y))
  ps <- sp::Polygons(list(p), "p1")
  if(grepl("+proj", prj)){
    sp_bbx <- sp::SpatialPolygons(list(ps), 1L, 
                                  proj4string = sp::CRS(prj))
  } else {
    sp_bbx <- sp::SpatialPolygons(list(ps), 1L, 
                                  proj4string = sp::CRS(SRS_string = prj))
  }
  sp_bbx
}

#' Estimate download size of DEMs
#' @param locations the locations
#' @param prj prj string as set earlier by get_elev_point or get_elev_raster
#' @param src the src
#' @param z zoom level if source is aws
#' @keywords internal
#' @importFrom sp wkt
estimate_raster_size <- function(locations, prj, src, z = NULL){
  
  locations <- bbox_to_sp(sp::bbox(locations), 
                          prj = prj)

  locations <- sp::spTransform(locations, sp::CRS(SRS_string = "EPSG:4326"))
  # Estimated cell size (at equator) from zoom level source
  # https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#sources-native-resolution
  # Each degree at equator = 111319.9 meters
  # Convert ground res to dd
  # zoom level 0 = 156543 meters 156543/111319.9
  # old resolution (no idea how I calculated these...)
  # c(0.54905236, 0.27452618, 0.15455633, 0.07145545, 0.03719130, 0.01901903, 
  # 0.00962056, 0.00483847, 0.00241219, 0.00120434, 0.00060173, 0.00030075, 
  #  0.00015035, 0.00007517, 0.00003758)
  m_at_equator <- c(156543.0, 78271.5, 39135.8, 19567.9, 9783.9, 4892.0, 2446.0, 
                    1223.0, 611.5, 305.7, 152.9, 76.4, 38.2, 19.1, 9.6, 4.8, 
                    2.4)
  z_res <- data.frame(z = 0:16, res_dd = m_at_equator/111319.9)

  bits <- switch(src,
                 aws = 32,
                 gl3 = 32,
                 gl1 = 32,
                 alos = 32,
                 srtm15plus = 32)
  if(src == "aws"){
    res <- z_res[z_res$z == z,]$res_dd
  } else{
    res <- switch(src,
                  gl3 = 0.0008333,
                  gl1 = 0.0002778,
                  alos = 0.0002778,
                  srtm15plus = 0.004165) 
  }
  num_rows <- (sp::bbox(locations)[1, "max"] - sp::bbox(locations)[1, "min"])/res
  num_cols <- (sp::bbox(locations)[2, "max"] - sp::bbox(locations)[2, "min"])/res
  
  num_megabytes <- (num_rows * num_cols * bits)/8388608
  num_megabytes
}
