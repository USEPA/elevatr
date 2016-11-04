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
#' @param prj A PROJ.4 string defining the projection of the locations argument. 
#'            If a \code{sp} or \code{raster} object is provided, the PROJ.4 
#'            string will be taken from that.  This argument is required for a 
#'            \code{data.frame} of locations.
#' @param src A character indicating which API to use, currently only 
#'               "mapzen" is used.
#' @param api_key A valid API key.  
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile and 
#'               additional area around the feature is desired. Default is NULL.
#' @param ... Extra parameters to pass to API specific fucntions
#' @return Function returns a \code{SpatialPointsDataFrame} in the projection 
#'         specified by the \code{prj} argument.
#' @export
#' @examples 
#' data(lake)
#' loc_df <- data.frame(x = runif(6,min=bbox(lake)[1,1], max=bbox(lake)[1,2]),
#'                      y = runif(6,min=bbox(lake)[2,1], max=bbox(lake)[2,2]))
#' x <- get_elev_raster(locations = loc_df, prj = sp::proj4string(lake), z=10)
#' x <- get_elev_raster(lake, z = 12)
#' 
get_elev_raster <- function(locations,prj = NULL,src = c("mapzen"),
                           api_key = get_api_key(src), expand = NULL, ...){
  src <- match.arg(src)
  # Check location type and if sp, set prj.  If no prj (for either) then error
  locations <- loc_check(locations,prj)
  prj <- sp::proj4string(locations)
  # Pass of locations to apis to get data as raster
  if(src == "mapzen"){
    raster_elev <- get_mapzen_terrain(sp::bbox(locations),prj,api_key = api_key, 
                                      expand, ...)
  }
  # Re-project from webmerc back to original and return
  raster_elev <- raster::projectRaster(raster_elev, crs = sp::CRS(prj))
  raster_elev
}

#' Get a digital elevation model from the Mapzen Terrain Tiles
#' 
#' This function uses the Mapzen Terrain Tile service to retrieve an elevation
#' raster from the geotiff service.  It accepts a \code{sp::bbox} object as 
#' input and returns a single raster object covering that extent.  You must have
#' an api_key from Mapzen.
#' 
#' @source Attribution: Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 
#'         content courtesy of the U.S. Geological Survey and ETOPO1 content 
#'         courtesy of U.S. National Oceanic and Atmospheric Administration. 
#'         \url{https://mapzen.com/documentation/terrain-tiles/} 
#' 
#' @param bbx a \code{sp::bbox} object that is used to select x,y,z tiles.
#' @param z The zoom level to return.  The zoom ranges from 1 to 15.  Resolution
#'          of the resultant raster is determined by the zoom and latitude.  For 
#'          details on zoom and resolution see the documentation from Mapzen at 
#'          \url{https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution}
#' @param prj Proj.4 string for input bbox 
#' @param api_key An API Key from Mapzen, create at 
#'                \url{https://mapzen.com/developer} Required. Set in your 
#'                \code{.Rprofile} file with the option \code{mapzen_key}
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile and 
#'               additional area around the feature is desired. Default is NULL.                
#' @export
#' @keywords internal
#' @examples 
#' data(lake)
#' wgs84_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' lake_dd <- sp::spTransform(lake,sp::CRS(wgs84_dd))
#' get_mapzen_terrain(sp::bbox(lake_dd),prj = wgs84_dd, z=13)
get_mapzen_terrain <- function(bbx, z=9, prj, api_key = getOption("mapzen_key")
                               ,expand=NULL){
  # Expand (if needed) and re-project bbx to dd
  bbx <- proj_expand(bbx,prj,expand)
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  base_url <- "https://tile.mapzen.com/mapzen/terrain/v1/geotiff/"
  tiles <- get_tilexy(bbx,z)
  dem_list<-vector("list",length = nrow(tiles))
  for(i in seq_along(tiles[,1])){
    tmpfile <- tempfile()
    url <- paste0(base_url,z,"/",tiles[i,1],"/",tiles[i,2],".tif?api=key",api_key)
    httr::GET(url,httr::write_disk(tmpfile,overwrite=T))
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
  } else {
    stop("Whoa, something is not right")
  }
}
