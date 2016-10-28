#' Get Point Elevation
#' 
#' Several web services provide access to point elevations.  This function provides
#' access to several of those.  Currently it uses either the Mapzen Elevation 
#' Service or the USGS Elevation Point Query Service (US Only).  The function 
#' accepts a \code{data.frame} of x (long) and y (lat) or a 
#' \code{SpatialPoints}/\code{SpatialPointsDataFame} as input.  A 
#' SpatialPointsDataFrame is returned with elevation as an added 
#' \code{data.frame}.
#' 
#' @param locations Either a \code{data.frame} with x (e.g. longitude) as the first column 
#'                 and y (e.g. latitude) as the second column or a 
#'                 \code{SpatialPoints}/\code{SpatialPointsDataFrame}.  
#'                 Elevation for these points will be returned.
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
#'               are enforced by the \code{\link{elevatr}} package.
#' @param api_key A character for the approriate API key.  Default is to use key
#'                as defined in \code{\link{options}}.  Acceptable option name 
#'                is currently only "mapzen_key".
#' @param ... Additional arguments passed to get_epqs or get_mapzen_elevation
#' @return Function returns a \code{SpatialPointsDataFrame} in the projection 
#'         specified by the \code{prj} argument.
#' @export
#' @examples
#' mt_wash <- data.frame(x = -71.3036, y = 44.2700)
#' mt_wash_sp <- sp::SpatialPoints(coordinates(mt_wash),
#'                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
#' get_elev_point(locations = mt_wash, 
#'                prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' get_elev_point(locations = mt_wash, src = "epqs", units="feet",
#'                prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' get_elev_point(locations = mt_wash, src = "epqs", units="meters",
#'                prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#' \dontrun{
#' data(lake)
#' options(mapzen_key = "mapzen-XXXXXXX")
#' get_elev_point(samples)}
get_elev_point <- function(locations, prj = NULL, src = c("mapzen","epqs"),
                           api_key = get_api_key(src), ...){
  src <- match.arg(src)
  # Check location type and if sp, set prj.  If no prj (for either) then error
  locations <- loc_check(locations,prj)
  prj <- proj4string(locations)
  # Re-project locations to dd
  locations_dd <- sp::spTransform(locations,
                  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  # Pass of reprojected to epqs or mapzen to get data as spatialpointsdataframe
  if(src == "mapzen"){ 
    locations_dd <- get_mapzen_elev(locations_dd,...)
  } else if (src == "epqs"){
    locations_dd <- get_epqs(locations_dd,...)
  }
  # Re-project back to original and return
  locations <- sp::spTransform(locations_dd,CRS(prj))
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
#' @param locations  A SpatialPointsDataFrame of the location(s) for which you wish to return 
#'                  elevation. The first colum is Longitude and the second 
#'                  column is Latitude.  
#' @param units Character string of either meters or feet.  Only works for 'epqs'
#' @return a SpatialPointsDataFrame with elevation added to the data slot
#' @export
#' @keywords internal
#' @examples 
#' xdf<-data.frame(runif(10,-75,-72),runif(10,40,45))
#' get_epqs(xdf)
get_epqs <- function(locations, units = c("meters","feet"), ...){
  base_url <- "http://ned.usgs.gov/epqs/pqs.php?"
  if(match.arg(units) == "meters"){
    units <- "Meters"
  } else if(match.arg(units) == "feet"){
    units <- "Feet"
  }
  units <- paste0("&units=",units)
  #Add Progress bar
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
    locations$elevation[i] <- resp[[1]][[1]]$Elevation
  }
  locations
}

#' Get point elevations from Mapzen
#' 
#' @param api_key A valid Mapzen API key.  Not required, but higher rate limits
#'                are allowed with a key. Defaults to 
#'                \code{getOption("mapzen_key")}.
#' @source Attribution: Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 
#'         content courtesy of the U.S. Geological Survey and ETOPO1 content 
#'         courtesy of U.S. National Oceanic and Atmospheric Administration. 
#'         \url{https://mapzen.com/documentation/elevation/elevation-service/} 
#' @export
#' @keywords internal
get_mapzen_elev <- function(locations, api_key = getOption("mapzen_key")){
  #elevation.mapzen.com/height?json={"shape":[{"lat":40.712431,"lon":-76.504916},{"lat":40.712275,"lon":-76.605259}]}&api_key=mapzen-RVEVhbW
  base_url <- "https://elevation.mapzen.com/height?json="
  key <- paste0("&api_key=",api_key)
  coords <- data.frame(coordinates(locations))
  names(coords) <- c("lon","lat")
  json_coords <- jsonlite::toJSON(list(shape=coords))
  url <- paste0(base_url,json_coords,key)
  resp <- httr::GET(url)
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  } 
  resp <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), 
                             simplifyVector = FALSE)
  #Add Progress bar
  locations$elevation <- unlist(resp$height)
  locations
}
