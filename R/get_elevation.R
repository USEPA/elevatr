#' Get elevation data
#' 
#' Primary function for accessing elevation data from a variety of online 
#' sources
#' 
#' @param location  A data.frame of the location(s) for which you wish to return 
#'                  elevation. The first colum is Longitude and the second 
#'                  column is Latitude.  
#' @param source  character string indicating 'epqs' for USGS elevation point 
#'                query service or 'srtm' for Open Topography SRTM service
#' @param units Character string of either meters or feet.  Only works for 'epqs'
#' @param res Character string of either 30 or 90. Only works for 'srtm' and 
#'            fetches data for the 30 meter SRTM (i.e., SRTM GL1 (Global 30m)) or
#'            the 90 meter SRTM (i.e., SRTM GL3 (Global 90m))
#' @export
#' @examples 
#' xdf<-data.frame(runif(10,-75,-72),runif(10,40,45))
#' get_elevation(xdf)
#' get_elevation(xdf,"srtm")
get_elevation <- function(location, source = c("epqs","srtm"), 
                          units = c("Meters","Feet"),
                          res = c("30","90")){
  source <- match.arg(source)
  units <- match.arg(units)
  res <- match.arg(res)
  if(source == "epqs"){
    return(get_epqs(location,units))
  }
  if(source == "srtm"){
    return(get_srtm(location,res))
  }
}

#' Get EPQS
#' @keywords internal
#' @param location location from get_elevation
#' @param units units from get_elevation
get_epqs <- function(location,units){
  df <- data.frame(matrix(ncol = 3, nrow = nrow(location)))
  base_url <- "http://ned.usgs.gov/epqs/pqs.php?"
  units <- paste0("&units=",units)
  #Add Progress bar
  for(i in seq_along(location[,1])){
    x <- location[i,1]
    y <- location[i,2]
    loc <- paste0("x=",x, "&y=", y)
    url <- paste0(base_url,loc,units,"&output=json")
    resp <- httr::GET(url)
    if (httr::http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    } 
    resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
                               simplifyVector = FALSE,
                                )
    df[i,] <- c(x,y,resp[[1]][[1]]$Elevation)
  }
  names(df) <- c("long","lat","elev")
  df
}

#' Get SRTM from Open Topography
#' @keywords internal
#' @param location location from get_elevation
#' @param res res from get_elevation
get_srtm <- function(location,res){
   browser()
   df <- data.frame(matrix(ncol = 3, nrow = nrow(location)))
   base_url <- "http://opentopo.sdsc.edu/otr/getdem?"
   if(res == 90){dem <- "demtype=SRTMGL3"}
   if(res == 30){dem <- "demtype=SRTMGL1"}
   #NEED FUNCTION TO SET LOCATION FOR a single SRTM download
   west <- location[i,1]
   east <- west + 10
   north <- location[i,2]
   south <- north - 10
   loc <- paste0("&west=",west,"&south=",south,"&east=",east,"&north=",north)
   url <- paste0(base_url,dem,loc)
   tmp_gtif <- tempfile(fileext=".tif")
   resp <- httr::POST(url,httr::write_disk(tmp_gtif,overwrite=T))
   #NEED TO FIGURE OUT RESPONSE TYPE (octet-stream?)
   if (httr::http_type(resp) != "application/octet-stream") {
     stop("API did not return json", call. = FALSE)
   } 
   for(i in seq_along(location[,1])){
 
     #NEED FUNCTION TO GET SINGLE POINT ELEVATION FROM RASTER
     df[i,] <- c(x,y,resp[[1]][[1]]$Elevation)
   }
   df
}

#' Get a digital elevation model from the Mapzen Terrain Tiles
#' 
#' This function uses the Mapzen Terrain Tile service to retrieve an elevation
#' raster.  It accepts a \code{sp::bbox} object as input and returns a single
#' raster object covering that extent.  You must have an api_key from Mapzen.
#' 
#' @param bbx a \code{sp::bbox} object that defines the area to return
#' @param z The zoom level to return.  The zoom ranges from 1 to 15.  Resolution
#'          of the resultant raster is determined by the zoom and latitude.  For 
#'          details on zoom and resolution see the documentation from Mapzen at 
#'          \url{https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution}
#' @param api_key An API Key from Mapzen, create at 
#'                \url{https://mapzen.com/developer} Required. Set in your 
#'                \code{.Rprofile} file with the option \code{mapzen_key}
#' @param file_format Several file formats are provided by Mapzen: terrarium, 
#'                    normal, geotiff, and skadi.  Currently only the geotiff
#'                    format is supported
#' @export
#' @examples 
#' library(quickmapr)
#' library(sp)
#' data(lake)
#' wgs84_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' lake_dd <- spTransform(lake,CRS(wgs84_dd))
#' get_mapzen_terrain(bbox(lake_dd),z=13)
get_mapzen_terrain <- function(bbx, z=10, api_key = getOption("mapzen_key"),
                               file_format = c("geotiff"),
                               ...){
  
  web_merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
  wgs84_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  base_url <- "https://tile.mapzen.com/mapzen/terrain/v1/geotiff/"
  tiles <- get_tilexy(bbx,z-1)
  dem_list<-vector("list",length = nrow(tiles))
  tmpfile <- tempfile() 
  for(i in seq_along(tiles[,1])){
    url <- paste0(base_url,z-1,"/",tiles[i,1],"/",tiles[i,2],".tif?api=key",api_key)
    httr::GET(url,httr::write_disk(tmpfile,overwrite=T))
    dem_list[[i]] <- raster::raster(tmpfile)
    raster::projection(dem_list[[i]]) <- web_merc
    dem_list[[i]] <- raster::projectRaster(dem_list[[i]],crs=CRS(wgs84_dd))
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

get_mapzen_elev <- function(location){
  #elevation.mapzen.com/height?json={"shape":[{"lat":40.712431,"lon":-76.504916},{"lat":40.712275,"lon":-76.605259}]}&api_key=mapzen-RVEVhbW
}
