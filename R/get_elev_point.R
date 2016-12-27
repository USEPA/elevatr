#' Get Point Elevation
#' 
#' Several web services provide access to point elevations.  This function 
#' provides access to several of those.  Currently it uses either the Mapzen 
#' Elevation Service or the USGS Elevation Point Query Service (US Only).  The 
#' function accepts a \code{data.frame} of x (long) and y (lat) or a 
#' \code{SpatialPoints}/\code{SpatialPointsDataFame} as input.  A 
#' SpatialPointsDataFrame is returned with elevation as an added 
#' \code{data.frame}.
#' 
#' @param locations Either a \code{data.frame} with x (e.g. longitude) as the 
#'                  first column and y (e.g. latitude) as the second column or a 
#'                  \code{SpatialPoints}/\code{SpatialPointsDataFrame}.  
#'                  Elevation for these points will be returned.
#' @param prj A PROJ.4 string defining the projection of the locations argument. 
#'            If a \code{SpatialPoints} or \code{SpatialPointsDataFrame} is 
#'            provided, the PROJ.4 string will be taken from that.  This 
#'            argument is required for a \code{data.frame} of locations.
#' @param src A character indicating which API to use, currently "mapzen" or 
#'               "epqs".  Default is "mapzen".  Note that the Mapzen Elevation 
#'               Service is subject to rate limits.  Keyless access limits 
#'               requests to 1,000 requests per day, 6 per minute, and 1 per 
#'               second.  With a Mapzen API key 
#'               (\url{https://mapzen.com/developers/}) requests are limited to
#'               20,000 per day or 2 per second.  Per day and per second rates
#'               are not yet enforced by the \code{\link{elevatr}} package, but 
#'               will be in the future.  The "epqs" source is relatively slow 
#'               for larger numbers of points (e.g. > 500). 
#' @param api_key A character for the approriate API key.  Default is to use key
#'                as defined in \code{.Renviron}.  Acceptable environment 
#'                variable name is currently only "mapzen_key".
#' @param ... Additional arguments passed to get_epqs or get_mapzen_elevation
#' @return Function returns a \code{SpatialPointsDataFrame} in the projection 
#'         specified by the \code{prj} argument.
#' @export
#' @examples
#' \dontrun{
#' mt_wash <- data.frame(x = -71.3036, y = 44.2700)
#' mt_mans <- data.frame(x = -72.8145, y = 44.5438)
#' mts <- rbind(mt_wash,mt_mans)
#' ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#' mts_sp <- sp::SpatialPoints(sp::coordinates(mts), 
#'                             proj4string = sp::CRS(ll_prj)) 
#' get_elev_point(locations = mt_wash,prj = ll_prj)
#' get_elev_point(locations = mt_wash, src = "epqs", units="feet", prj = ll_prj)
#' get_elev_point(locations = mt_wash, src = "epqs", units="meters", 
#'                prj = ll_prj)
#' get_elev_point(locations = mts_sp)
#' data(sp_big)
#' get_elev_point(sp_big)}
get_elev_point <- function(locations, prj = NULL, src = c("mapzen","epqs"),
                           api_key = get_api_key(src), ...){
  src <- match.arg(src)
  # Check location type and if sp, set prj.  If no prj (for either) then error
  locations <- loc_check(locations,prj)
  prj <- sp::proj4string(locations)
  
  # Pass of reprojected to epqs or mapzen to get data as spatialpointsdataframe
  if(src == "mapzen"){ 
    locations_prj <- get_mapzen_elev(locations,api_key = api_key, ...)
  } else if (src == "epqs"){
    locations_prj <- get_epqs(locations, ...)
  }
  # Re-project back to original and return
  locations <- sp::spTransform(locations_prj,sp::CRS(prj))
  if(length(list(...)) > 0){ 
    if(names(list(...)) %in% "units" & list(...)$units == "feet"){
      locations$elev_units <- rep("feet", nrow(locations))
    } else {
      locations$elev_units <- rep("meters", nrow(locations))
    }
  } else {
    locations$elev_units <- rep("meters", nrow(locations))
  }
  locations
}

#' Get point elevation data from the USGS Elevation Point Query Service
#' 
#' Function for accessing elevation data from the USGS epqs
#' 
#' @param locations A SpatialPointsDataFrame of the location(s) for which you 
#'                  wish to return elevation. The first colum is Longitude and 
#'                  the second column is Latitude.  
#' @param units Character string of either meters or feet. Only works for 'epqs'
#' @return a SpatialPointsDataFrame with elevation added to the data slot
#' @export
#' @keywords internal
get_epqs <- function(locations, units = c("meters","feet")){
  base_url <- "http://ned.usgs.gov/epqs/pqs.php?"
  if(match.arg(units) == "meters"){
    units <- "Meters"
  } else if(match.arg(units) == "feet"){
    units <- "Feet"
  }
  # Re-project locations to dd
  
  locations <- sp::spTransform(locations,
                                   sp::CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
  units <- paste0("&units=",units)
  pb <- progress::progress_bar$new(format = " Accessing point elevations [:bar] :percent",
                                   total = nrow(locations), clear = FALSE, 
                                   width= 60)
  for(i in seq_along(locations[,1])){
    x <- sp::coordinates(locations)[i,1]
    y <- sp::coordinates(locations)[i,2]
    loc <- paste0("x=",x, "&y=", y)
    url <- paste0(base_url,loc,units,"&output=json")
    resp <- httr::GET(url)
    if (httr::http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    } 
    resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
                               simplifyVector = FALSE
                                )
    locations$elevation[i] <- as.numeric(resp[[1]][[1]]$Elevation)
    pb$tick()
    Sys.sleep(1 / 100)
  }
  
  locations
}

#' Get point elevations from Mapzen
#' 
#' @param api_key A valid Mapzen API key.  Although not required by the API, the
#'                rate limits are low without a key.  The \code{elevatr} package
#'                requires a key.  To get a key, visit 
#'                \url{https://mapzen.com/developers}. Defaults to 
#'                \code{Sys.getenv("mapzen_key")}.
#' @source Attribution: Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 
#'         content courtesy of the U.S. Geological Survey and ETOPO1 content 
#'         courtesy of U.S. National Oceanic and Atmospheric Administration. 
#'         \url{https://mapzen.com/documentation/elevation/elevation-service/} 
#' @export
#' @keywords internal
get_mapzen_elev <- function(locations, api_key = NULL){
  #elevation.mapzen.com/height?json={"shape":[{"lat":40.712431,"lon":-76.504916},{"lat":40.712275,"lon":-76.605259}]}&api_key=mapzen-RVEVhbW
  if(is.null(api_key)){
    get_slowly <- mapzen_elev_GET_nokey
  } else {
    get_slowly <- mapzen_elev_GET_withkey
  }  
  base_url <- "https://elevation.mapzen.com/height?json="
  key <- paste0("&api_key=",api_key)
  locations <- sp::spTransform(locations,
                           sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  coords <- data.frame(sp::coordinates(locations))
  names(coords) <- c("lon","lat")
  if(nrow(coords)<201){
    json_coords <- jsonlite::toJSON(list(shape=coords))
    if(is.null(api_key)){
      #really only here for tests
      url <- paste0(base_url,json_coords)
    } else {
      url <- paste0(base_url,json_coords,key)
    }
    resp <- get_slowly(url)
    if (httr::http_type(resp) != "application/json") {
      if(resp$status_code == 429){
        stop("Mapzen Rate Limit Exceeded", call. = FALSE)
      } else {
        stop("API did not return json", call. = FALSE)
      }
    } 
    resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
                               simplifyVector = FALSE)
    locations$elevation <- unlist(resp$height)
  } else if(nrow(coords)>200){
    #Break up becuase larger requests would time out 
    idx_e <- seq(0,nrow(coords),by=200)
    if(nrow(coords)%%200 == 0){
      idx_e <- idx_e[-1]
    } else {
      idx_e <- c(idx_e[-1],nrow(coords))
    }
    idx_s <- seq(1,nrow(coords),by=200)
    pb <- progress::progress_bar$new(format = " Accessing point elevations [:bar] :percent",
                                     total = length(idx_e), clear = FALSE, 
                                     width= 60)
    for(i in seq_along(idx_e)){
      json_coords <- jsonlite::toJSON(list(shape=coords[idx_s[i]:idx_e[i],]))
      if(is.null(api_key)){
        #really only here for tests
        url <- paste0(base_url,json_coords)
      } else {
        url <- paste0(base_url,json_coords,key)
      }
      resp <- get_slowly(url)
      if (httr::http_type(resp) != "application/json") {
        if(resp$status_code == 429){
          stop("Mapzen Rate Limit Exceeded", call. = FALSE)
        } else {
          stop("API did not return json", call. = FALSE)
        }
      } 
      resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
                                 simplifyVector = FALSE)
      locations$elevation[idx_s[i]:idx_e[i]] <- unlist(resp$height)
      pb$tick()
    }
  }
  locations
}
