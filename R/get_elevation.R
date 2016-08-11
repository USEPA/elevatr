#' Get elevation data
#' 
#' Primary function for accessing elevation data from a variety of online 
#' sources
#' 
#' @param location  a data frame, \code{sp} object, a \code{bbox}, or 
#'                  \code{extent} object to get elevation data for.
#' @param source
#' @param key
#' @param out
#' @importFrom httr GET
#' @importFrom jsonlite::fromJSON
#' @export
#' @examples 
get_elevation <- function(location, source, key, out){
  url <- "http://ned.usgs.gov/epqs/pqs.php?x=-72&y=42&units=Meters&output=json"
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  resp
}