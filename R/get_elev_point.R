#' Get Point Elevation
#' 
#' This function provides access to point elevations using either the USGS 
#' Elevation Point Query Service (US Only) or by extracting point elevations 
#' from the AWS Terrain Tiles.  The function accepts a \code{data.frame} of x 
#' (long) and y (lat) or a \code{SpatialPoints}/\code{SpatialPointsDataFame} as 
#' input.  A SpatialPointsDataFrame is returned with elevation as an added 
#' \code{data.frame}. 
#' 
#' 
#' @param locations Either a \code{data.frame} with x (e.g. longitude) as the 
#'                  first column and y (e.g. latitude) as the second column, a 
#'                  \code{SpatialPoints}/\code{SpatialPointsDataFrame}, or a 
#'                  \code{sf} \code{POINT} or \code{MULTIPOINT} object.   
#'                  Elevation for these points will be returned in the 
#'                  originally supplied class.
#' @param prj A string defining the projection of the locations argument. The 
#'            string needs to be an acceptable SRS_string for 
#'            \code{\link[sp]{CRS-class}} for your version of PROJ. If a \code{sf} 
#'            object, a \code{sp} object or a \code{raster} object 
#'            is provided, the string will be taken from that.  This 
#'            argument is required for a \code{data.frame} of locations.
#' @param src A character indicating which API to use, either "epqs" or "aws" 
#'            accepted. The "epqs" source is relatively slow for larger numbers 
#'            of points (e.g. > 500).  The "aws" source may be quicker in these 
#'            cases provided the points are in a similar geographic area.  The 
#'            "aws" source downloads a DEM using \code{get_elev_raster} and then
#'            extracts the elevation for each point. 
#' @param overwrite A logical indicating that existing \code{elevation} and 
#'                  \code{elev_units} columns should be overwritten.  Default is 
#'                  FALSE and \code{get_elev_point} will error if these columns 
#'                  already exist.
#' @param ... Additional arguments passed to get_epqs or get_aws_points.  When 
#'            using "aws" as the source, pay attention to the `z` argument.  A 
#'            defualt of 5 is used, but this uses a raster with a large ~4-5 km 
#'            pixel.  Additionally, the source data changes as zoom levels 
#'            increase.  
#'            Read \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution} 
#'            for details.  
#' @return Function returns an \code{sf} object in the projection specified by 
#'         the \code{prj} argument.
#' @export
#' @examples
#' \dontrun{
#' library(elevatr)
#' library(sf)
#' library(terra)
#' 
#' mts <- data.frame(x = c(-71.3036, -72.8145), 
#'                   y = c(44.2700, 44.5438), 
#'                   names = c("Mt. Washington", "Mt. Mansfield"))
#' ll_prj <- 4326
#' mts_sf <- st_as_sf(x = mts, coords = c("x", "y"), crs = ll_prj)
#' #Empty Raster
#' mts_raster <- rast(mts_sf, nrow = 5, ncol = 5)
#' # Raster with cells for each location
#' mts_raster_loc <- terra::rasterize(x, rast(x, nrow = 10, ncol = 10))
#' 
#' get_elev_point(locations = mts, prj = ll_prj)
#' get_elev_point(locations = mts, units="feet", prj = ll_prj)
#' get_elev_point(locations = mts_sf)
#' get_elev_point(locations = mts_raster)
#' get_elev_point(locations = mts_raster_loc)
#' 
#' 
#' # Code to split into a loop and grab point at a time.
#' # This is usually faster for points that are spread apart 
#'  
#' library(dplyr)
#' 
#' elev <- vector("numeric", length = nrow(mts))
#' for(i in seq_along(mts)){
#' elev[i]<-get_elev_point(locations = mts[i,], prj = ll_prj, src = "aws", 
#'                         z = 10)$elevation}
#' mts_elev <- cbind(mts, elev)
#' mts_elev
#' }
#'
get_elev_point <- function(locations, prj = NULL, src = c("epqs", "aws"), 
                           overwrite = FALSE, ...){
  
  # First Check for internet
  if(!curl::has_internet()) {
    message("Please connect to the internet and try again.")
    return(NULL)
  }
  
  src <- match.arg(src)
  
  # Check for existing elevation/elev_units columns and overwrite or error
  if(!overwrite & any(names(locations) %in% c("elevation", "elev_units"))){
    stop(paste0("The elevation and elev_units columns already exist.\n", 
    "  To replace these columns set the overwrite argument to TRUE."))
  }
  
  locations <- loc_check(locations,prj)
  
  if(is.null(prj)){
    prj <- sf::st_crs(locations)
  }
  
  # Pass of reprojected to epqs or aws to get data as spatialpointsdataframe
  if (src == "epqs"){
    locations_prj <- get_epqs(locations, ...)
    units <- locations_prj[[2]]
    locations_prj <- locations_prj[[1]]
  } 
  
  if(src == "aws"){
    locations_prj <- get_aws_points(locations, verbose = FALSE, ...)
    units <- locations_prj[[2]]
    locations_prj <- locations_prj[[1]]
  }

  # Re-project back to original, add in units, and return
  locations <- sf::st_transform(sf::st_as_sf(locations_prj), 
                                sf::st_crs(locations))
  
  if(is.null(nrow(locations))){
    nfeature <- length(locations) 
  } else {
    nfeature <- nrow(locations)
  }
  
  unit_column_name <- "elev_units"
  
  if(any(names(list(...)) %in% "units")){
    if(list(...)$units == "feet"){
      locations[[unit_column_name]] <- rep("feet", nfeature)
    } else {
      locations[[unit_column_name]] <- rep("meters", nfeature)
    }
  } else {
    locations[[unit_column_name]] <- rep("meters", nfeature)
  }
  
  if(src == "aws") {
    message(paste("Note: Elevation units are in", units))
  } else {
    message(paste("Note: Elevation units are in", 
                  tolower(strsplit(units, "=")[[1]][2])))
  }
  locations
}

#' Get point elevation data from the USGS Elevation Point Query Service
#' 
#' Function for accessing elevation data from the USGS epqs
#' 
#' @param locations A SpatialPointsDataFrame of the location(s) for which you 
#'                  wish to return elevation. The first column is Longitude and 
#'                  the second column is Latitude.  
#' @param units Character string of either meters or feet. Conversions for 
#'              'epqs' are handled by the API itself.
#' @param ncpu Number of CPU's to use when downloading epqs data.
#' @param serial Logical to determine if API should be hit in serial or in 
#'               parallel.  TRUE will use purrr, FALSE will use furrr.
#' @return a list with a SpatialPointsDataFrame or sf POINT or MULTIPOINT object with 
#'         elevation added to the data slot and a character of the elevation units
#' @export
#' @importFrom progressr handlers progressor with_progress
#' @importFrom purrr map_dbl
#' @keywords internal
get_epqs <- function(locations, units = c("meters","feet"), 
                     ncpu = future::availableCores() - 1,
                     serial = NULL){
  
  ll_prj  <- "EPSG:4326"
  
  if(is.null(nrow(locations))){
    nfeature <- length(locations) 
  } else {
    nfeature <- nrow(locations)
  }
  
  if(is.null(serial)){
    if(nfeature < 25){
      serial <- TRUE
    } else {
     serial <- FALSE
    }
  }
  
  base_url <- "https://epqs.nationalmap.gov/v1/json?"
  
  if(match.arg(units) == "meters"){
    units <- "Meters"
  } else if(match.arg(units) == "feet"){
    units <- "Feet"
  }
  
  locations <- sf::st_transform(locations, sf::st_crs(ll_prj))
  units <- paste0("&units=",units)
  
  get_epqs_resp <- function(coords, base_url, units, progress = FALSE) {
    
    Sys.sleep(0.001) #Getting non-repeateable errors maybe too many hits...
    x <- coords[1]
    y <- coords[2]
    
    loc <- paste0("x=",x, "&y=", y)
    url <- paste0(base_url,loc,units,"&output=json")
    
    resp <- tryCatch(httr::GET(url), error = function(e) e)
    n<-1
    
    while(n <= 5 & any(class(resp) == "simpleError")) {
      # Hit it again to test as most times this is a unexplained timeout that
      # Corrects on next hit
      
      resp <- tryCatch(httr::GET(url), error = function(e) e)
      n <- n + 1
    }
    
    if(n > 5 & any(class(resp) == "simpleError"))  {
      message(paste0("API returned:'", resp$message, 
                     "'. NA returned for elevation"), 
              call. = FALSE)
      return(NA)
    }
    
    if(httr::status_code(resp) == 200 & httr::content(resp, "text", encoding = "UTF-8") == ""){
      message("API returned an empty repsonse (e.g. location in ocean or not in U.S.). NA returned for elevation")
      return(NA)
    } else if(httr::status_code(resp) == 200){
      
      resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
                               simplifyVector = FALSE)
    } else {
      message(paste0("API returned a status code:'", resp$status_code, 
                     "'. NA returned for elevation"), 
              call. = FALSE)
      return(NA)
    }
    round(as.numeric(resp$value), 2)
  }
   
  coords_df <- split(data.frame(sf::st_coordinates(locations)), 
                     seq_along(locations$elevation))   
  
  #elev_column_name <- make.unique(c(names(locations), "elevation"))
  #elev_column_name <- elev_column_name[!elev_column_name %in% names(locations)]
  elev_column_name <- "elevation"
  browser()
  message("Downloading point elevations:")
  
  progressr::handlers(
    progressr::handler_progress(
      format = " Accessing point elevations [:bar] :percent",
      clear = FALSE, 
      width= 60
    ))
  
  progressr::with_progress({
  if(serial){
    p <- progressor(along = coords_df)
    locations[[elev_column_name]] <- purrr::map_dbl(coords_df,
                                          function(x) {
                                            p()
                                            get_epqs_resp(x, base_url, units)
                                            })
  } else {
    
    future::plan(future::multisession, workers = ncpu)
    p <- progressor(along = coords_df)
    locations[[elev_column_name]] <-furrr::future_map_dbl(coords_df,
                                               function(x) {
                                                 p()
                                                 get_epqs_resp(x, base_url, 
                                                               units)})
    
  }
  })
  
  # For areas without epqs values that return -1000000, switch to NA
  locations$elevation[locations[[elev_column_name]] == -1000000] <- NA
  location_list <- list(locations, units)
  if(serial==FALSE){future::plan(future::sequential)}
  location_list
}

#' Get point elevation data from the AWS Terrain Tiles
#' 
#' Function for accessing elevation data from AWS and extracting the elevations 
#' 
#' @param locations Either a \code{data.frame} with x (e.g. longitude) as the 
#'                  first column and y (e.g. latitude) as the second column, a 
#'                  \code{SpatialPoints}/\code{SpatialPointsDataFrame}, or a 
#'                  \code{sf} \code{POINT} or \code{MULTIPOINT} object.   
#'                  Elevation for these points will be returned in the 
#'                  originally supplied class.
#' @param z The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'           of the resultant raster is determined by the zoom and latitude.  For 
#'           details on zoom and resolution see the documentation from Mapzen at 
#'           \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution}.  
#'           default value is 5 is supplied.   
#' @param units Character string of either meters or feet. Conversions for 
#'              'aws' are handled in R as the AWS terrain tiles are served in 
#'              meters.
#' @param verbose Report back messages.                             
#' @param ... Arguments to be passed to \code{get_elev_raster}
#' @return a list with a SpatialPointsDataFrame or sf POINT or MULTIPOINT object with 
#'         elevation added to the data slot and a character of the elevation units
#' @export
#' @keywords internal
get_aws_points <- function(locations, z = 5, units = c("meters", "feet"), 
                           verbose = TRUE, ...){
  units <- match.arg(units)
  dem <- get_elev_raster(locations, z, verbose  = verbose, ...)
  elevation <- terra::extract(dem, locations)
  if(units == "feet") {elevation <- elevation * 3.28084}
  locations$elevation <- round(elevation, 2)
  location_list <- list(locations, units)
  location_list
}






































