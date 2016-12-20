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
#'           \url{https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution}                 
#' @param prj A PROJ.4 string defining the projection of the locations argument. 
#'            If a \code{sp} or \code{raster} object is provided, the PROJ.4 
#'            string will be taken from that.  This argument is required for a 
#'            \code{data.frame} of locations."
#' @param src A character indicating which API to use, currently either 
#'               "mapzen" (default), or "aws" is used. Both use the same source
#'               tiles.  The Amazon Web Services tiles are best if rate limits
#'               are causing failure of the Mapzen tiles or if you are accessing
#'               the data via and AWS instance.
#' @param api_key A valid API key.  
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile and 
#'               additional area around the feature is desired. Default is NULL.
#' @param ... Extra arguments to pass to \code{httr::GET} via a named vector, 
#'            \code{config}.   See \code{\link{get_mapzen_terrain}} and 
#'            \code{\link{get_aws_terrain}} for more details. 
#' @return Function returns a \code{SpatialPointsDataFrame} in the projection 
#'         specified by the \code{prj} argument.
#' @details Currently, the \code{get_elev_raster} utilizes two separate APIs, 
#'          the Mapzen Terrain Tile Service 
#'          (\url{https://mapzen.com/documentation/terrain-tiles/}) or the 
#'          Amazon Web Services 
#'          (\url{https://aws.amazon.com/public-datasets/terrain/}).  Both 
#'          services utilize the same underlying data and provide global 
#'          coverage, but they have different use cases.  The Mapzen service is
#'          cached and thus should provide speedier downloads.  It will work 
#'          without an API key but an API key is suggested. 
#'          
#'          Both services are provided via x, y, and z tiles (see 
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
#' x <- get_elev_raster(locations = loc_df, prj = sp::proj4string(lake), z=10, 
#'                      api_key = NULL)
#' 
#' data(lake)
#' x <- get_elev_raster(lake, z = 3, src = "mapzen")
#' x <- get_elev_raster(lake, z = 12, src = "aws")
#' }
#' 
get_elev_raster <- function(locations, z, prj = NULL,src = c("mapzen", "aws"),
                           api_key = get_api_key(src), expand = NULL, ...){
  src <- match.arg(src)
  # Check location type and if sp, set prj.  If no prj (for either) then error
  locations <- loc_check(locations,prj)
  prj <- sp::proj4string(locations)
  
  # Pass of locations to apis to get data as raster
  if(src == "mapzen"){
    raster_elev <- get_mapzen_terrain(sp::bbox(locations), z, prj = prj, api_key = api_key, 
                                      expand = expand, ...)
  } else if(src == "aws") {
    raster_elev <- get_aws_terrain(sp::bbox(locations), z, prj = prj, expand = expand, ...)
  }
  # Re-project from webmerc back to original and return
  raster_elev <- raster::projectRaster(raster_elev, crs = sp::CRS(prj))
  raster_elev
}

#' Get a digital elevation model from the Mapzen Terrain Tiles
#' 
#' This function uses the Mapzen Terrain Tile service to retrieve an elevation
#' raster from the geotiff service.  It accepts a \code{sp::bbox} object as 
#' input and returns a single raster object covering that extent.  You should have
#' an api_key from Mapzen.
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
#'          \url{https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution}
#' @param prj Proj.4 string for input bbox 
#' @param api_key An API Key from Mapzen, create at 
#'                \url{https://mapzen.com/developer} Required. Set in your 
#'                \code{.Renviron} file with the variable "mapzen_key"
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
get_mapzen_terrain <- function(bbx, z, prj, api_key = NULL ,expand=NULL, ...){

  # Expand (if needed) and re-project bbx to dd
  bbx <- proj_expand(bbx,prj,expand)
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  base_url <- "https://tile.mapzen.com/mapzen/terrain/v1/geotiff/"
  tiles <- get_tilexy(bbx,z)
  dem_list<-vector("list",length = nrow(tiles))
  for(i in seq_along(tiles[,1])){
    tmpfile <- tempfile()
    if(is.null(api_key)){
      url <- paste0(base_url,z,"/",tiles[i,1],"/",tiles[i,2],".tif")
    } else {
      url <- paste0(base_url,z,"/",tiles[i,1],"/",tiles[i,2],".tif?api=key",api_key)
    }
    resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=T), ...)
    if (httr::http_type(resp) != "image/tif") {
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
  if(length(dem_list) == 1){
    return(dem_list[[1]])
  } else if (length(dem_list) > 1){
    return(do.call(raster::merge, dem_list))
  } 
}

#' Get a digital elevation model from the AWS Terrain Tiles
#' 
#' This function uses the AWS Terrain Tile service to retrieve an elevation
#' raster from the geotiff service.  It accepts a \code{sp::bbox} object as 
#' input and returns a single raster object covering that extent.  The data is
#' the same as that availble via the Mapzen tiles but does not require a key.  
#' It is best used if rate limits are causing failures on the Mapzen service or 
#' if you are needing to access the terrain tiles via an AWS instance.  These 
#' tiles are not cached so accessing them via a local/non-AWS machine will be 
#' slower than the Mapzen tiles.
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
#'          \url{https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution}
#' @param prj Proj.4 string for input bbox 
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
get_aws_terrain <- function(bbx, z, prj,expand=NULL, ...){
  # Expand (if needed) and re-project bbx to dd
  bbx <- proj_expand(bbx,prj,expand)
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff/"
  tiles <- get_tilexy(bbx,z)
  dem_list<-vector("list",length = nrow(tiles))
  for(i in seq_along(tiles[,1])){
    tmpfile <- tempfile()
    url <- paste0(base_url,z,"/",tiles[i,1],"/",tiles[i,2],".tif")
    resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=T), ...)
    if (httr::http_type(resp) != "image/tif") {
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
  if(length(dem_list) == 1){
    return(dem_list[[1]])
  } else if (length(dem_list) > 1){
    return(do.call(raster::merge, dem_list))
  } 
}
