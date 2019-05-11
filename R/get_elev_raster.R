#' Get Raster Elevation
#' 
#' Several web services provide access to raster elevation. Currently, this 
#' function provides access to the Mapzen Terrain Service The function 
#' accepts a \code{data.frame} of x (long) and y (lat), an 
#' \code{sp}, or \code{raster} object as input.  A \code{raster} object is 
#' returned.
#' 
#' @param locations Either a \code{data.frame} of x (long) and y (lat), an 
#'                  \code{sp}, or \code{raster} object as input. 
#' @param z  The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'           of the resultant raster is determined by the zoom and latitude.  For 
#'           details on zoom and resolution see the documentation from Mapzen at 
#'           \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution                 
#' @param prj A PROJ.4 string defining the projection of the locations argument. 
#'            If a \code{sp} or \code{raster} object is provided, the PROJ.4 
#'            string will be taken from that.  This argument is required for a 
#'            \code{data.frame} of locations."
#' @param src A character indicating which API to use.  Currently supports "aws" 
#'            and "gl3" from the Open Topograhy API. "aws" is the default.
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile and 
#'               additional area around the feature is desired. Default is NULL.
#' @param clip A character value used to determine clipping of returned DEM.  
#'             The default value is "tile" which returns the full tiles.  Other 
#'             options are "bbox" which returns the DEM clipped to the bounding 
#'             box of the original locations (or expanded bounding box if used), 
#'             or "locations" if the spatials data (e.g. polygons) in the input 
#'             locations should be used to clip the DEM.  Locations are not used 
#'             to clip input point datasets.  Instead the bounding box is used.
#' @param verbose Toggles on and off the note about units and coordinate 
#'                reference system.
#' @param ... Extra arguments to pass to \code{httr::GET} via a named vector, 
#'            \code{config}.   See
#'            \code{\link{get_aws_terrain}} for more details. 
#' @return Function returns a \code{SpatialPointsDataFrame} in the projection 
#'         specified by the \code{prj} argument.
#' @details Currently, the \code{get_elev_raster} utilizes only the 
#'          Amazon Web Services 
#'          (\url{https://registry.opendata.aws/terrain-tiles/}) terrain 
#'          tiles.  Versions of \code{elevatr} 0.1.4 or earlier had options for 
#'          the Mapzen terrain tiles.  Mapzen data is no longer available.  
#'          Support for the replacment Nextzen tiles is not currently available
#'          
#'          The terrain data is provided via x, y, and z tiles (see 
#'          \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames} for 
#'          details.) The x and y are determined from the bounding box of the 
#'          object submitted for \code{locations} argument, and the z argument 
#'          must be specified by the user.   
#' @export
#' @examples 
#' \dontrun{
#' loc_df <- data.frame(x = runif(6,min=sp::bbox(lake)[1,1], 
#'                                max=sp::bbox(lake)[1,2]),
#'                      y = runif(6,min=sp::bbox(lake)[2,1], 
#'                                max=sp::bbox(lake)[2,2]))
#' x <- get_elev_raster(locations = loc_df, prj = sp::proj4string(lake), z=10)
#' 
#' data(lake)
#' x <- get_elev_raster(lake, z = 12)
#' x <- get_elev_raster(lake, src = "gl3", expand = 0.5)
#' }
#' 
get_elev_raster <- function(locations, z, prj = NULL,src = c("aws", "gl3"),
                           expand = NULL, clip = c("tile", "bbox", "locations"), 
                           verbose = TRUE, ...){
  src <- match.arg(src)
  clip <- match.arg(clip) 
  # Check location type and if sp, set prj.  If no prj (for either) then error
  locations <- loc_check(locations,prj)
  prj <- sp::proj4string(locations)
  
  # Pass of locations to APIs to get data as raster
  if(src == "aws") {
    raster_elev <- get_aws_terrain(locations, z, prj = prj, 
                                   expand = expand, ...)
  } else if(src == "gl3"){
    raster_elev <- get_gl3(locations, prj = prj, ...)
  }
  # Re-project from webmerc back to original and return
  if(clip != "tile"){
    message(paste("Clipping DEM to", clip))
    raster_elev <- clip_it(raster_elev, locations, expand, clip)
  }
  message(paste("Reprojecting DEM to original projection"))
  raster_elev <- raster::projectRaster(raster_elev, crs = sp::CRS(prj))
  if(verbose){
    message(paste("Note: Elevation units are in meters.\nNote: The coordinate reference system is:\n", prj))
  }
  raster_elev
}



#' Get a digital elevation model from the AWS Terrain Tiles
#' 
#' This function uses the AWS Terrain Tile service to retrieve an elevation
#' raster from the geotiff service.  It accepts a \code{sp::bbox} object as 
#' input and returns a single raster object covering that extent.   
#' 
#' @source Attribution: Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 
#'         content courtesy of the U.S. Geological Survey and ETOPO1 content 
#'         courtesy of U.S. National Oceanic and Atmospheric Administration. 
#'         \url{https://mapzen.com/documentation/terrain-tiles/} 
#' 
#' @param bbx a \code{sp::bbox} object that is used to select x,y,z tiles.
#' @param z The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'          of the resultant raster is determined by the zoom and latitude.  For 
#'          details on zoom and resolution see the documentation from Mapzen at 
#'          \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution}
#' @param prj PROJ.4 string for input bbox 
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile and 
#'               additional area around the feature is desired. Default is NULL.
#' @param ... Extra configuration parameters to be passed to httr::GET.  Common 
#'            usage is to adjust timeout.  This is done as 
#'            \code{config=timeout(x)} where \code{x} is a numeric value in 
#'            seconds.  Multiple configuration functions may be passed as a 
#'            vector.              
#' @export
#' @keywords internal
get_aws_terrain <- function(locations, z, prj, expand=NULL, ...){
  # Expand (if needed) and re-project bbx to dd
  bbx <- proj_expand(sp::bbox(locations),prj,expand)
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff/"
  tiles <- get_tilexy(bbx,z)
  dem_list<-vector("list",length = nrow(tiles))
  pb <- progress::progress_bar$new(
    format = "Downloading DEMs [:bar] :percent eta: :eta",
    total = nrow(tiles), clear = FALSE, width= 60)
  for(i in seq_along(tiles[,1])){
    pb$tick()
    Sys.sleep(1/100)
    tmpfile <- tempfile()
    url <- paste0(base_url,z,"/",tiles[i,1],"/",tiles[i,2],".tif")
    resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=TRUE), ...)
    if (httr::http_type(resp) != "image/tiff") {
      stop("API did not return tif", call. = FALSE)
    } 
    dem_list[[i]] <- raster::raster(tmpfile)
    raster::projection(dem_list[[i]]) <- web_merc
  }
  origins<-t(data.frame(lapply(dem_list,raster::origin)))
  min_origin<-c(min(origins[,1]),min(origins[,2]))
  change_origins <- function(x,y){
    raster::origin(x)<-y
    x
  }
  dem_list <- lapply(dem_list, function(x,y) change_origins(x,min_origin))
  
  if(length(dem_list) == 1){ #Not sure this case will ever be satsified...
    return(dem_list[[1]])
  } else if (length(dem_list) > 1){
    message("Merging DEMs")
    return(do.call(raster::merge, dem_list))
  } 
}

#' Get a digital elevation model from the Open Topography SRTM Version 3
#' 
#' This function uses the Open Topography SRTM Version 3 files.   
#' 
#' @source Attribution: Details here
#' 
#' @param locations
#' @param prj PROJ.4 string for input bbox 
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the SRTM data. 
#' @param ... Extra configuration parameters to be passed to httr::GET.  Common 
#'            usage is to adjust timeout.  This is done as 
#'            \code{config=timeout(x)} where \code{x} is a numeric value in 
#'            seconds.  Multiple configuration functions may be passed as a 
#'            vector.              
#' @export
#' @keywords internal
get_gl3 <- function(locations, z, prj, expand=NULL, ...){
  # Expand (if needed) and re-project bbx to dd
  # MAKE SURE BBX IS WGS84!
  bbx <- data.frame(proj_expand(sp::bbox(locations),prj,expand))
  tmpfile <- tempfile()
  base_url <- "http://opentopo.sdsc.edu/otr/getdem?demtype=SRTMGL3"
  url <- paste0(base_url,
                "&west=",bbx[1,]$min,
                "&south=",bbx[2,]$min,
                "&east=",bbx[1,]$max,
                "&north=",bbx[2,]$max,
                "&outputFormat=GTiff")
  resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=TRUE), ...)
  if (httr::http_type(resp) != "application/octet-stream") {
    stop("API did not return octet-stream as expected", call. = FALSE)
  } 
  dem <- raster::raster(tmpfile)
  dem
}
