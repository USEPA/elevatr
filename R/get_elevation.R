#' Get elevation data
#' 
#' Primary function for accessing elevation data from a variety of online 
#' sources
#' 
#' @param location  The location for which you wish to return elevation.  You 
#'                  may either enter a data frame of Longitude and Latitude, a 
#'                  \code{sp} object, a \code{bbox}, or \code{extent} object.  A 
#'                  data frame will return a data frame, and the other object 
#'                  will return a raster.
#' @param source
#' @param units
#'
#' @importFrom httr GET
#' @importFrom jsonlite::fromJSON
#' @export
#' @examples 
get_elevation <- function(location, source = c("epqs","srtm"), 
                          units = c("Meters","Feet")){
  source <- match.arg(source)
  units <- match.arg(units)
  if(source == "epqs"){
  df <- data.frame(matrix(ncol = 3, nrow = nrow(location)))
  base_url <- "http://ned.usgs.gov/epqs/pqs.php?"
  units <- paste0("&units=",units)
    for(i in seq_along(location[,1])){
      x <- location[i,1]
      y <- location[i,2]
      loc <- paste0("x=",x, "&y=", y)
      url <- paste0(base_url,loc,units,"&output=json")
      resp <- GET(url)
      if (http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
      } 
      resp <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
      df[i,] <- c(x,y,resp[[1]][[1]]$Elevation)
    }
  df
  }
}