#' Get Raster Elevation
#'
#' Several web services provide access to raster elevation. Currently, this
#' function provides access to the Amazon Web Services Terrain Tiles and the
#' Open Topography global datasets API. The function accepts a \code{data.frame}
#' of x (long) and y (lat), an \code{sf}, or \code{terra} object as input.  A
#' \code{RasterLayer} object is returned. In subsequent versions, a \code{SpatRaster}
#' will be returned.
#'
#' @param locations Either a \code{data.frame} of x (long) and y (lat), an
#'                   \code{sf}, or \code{terra} object as input.
#' @param z  The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'           of the resultant raster is determined by the zoom and latitude.  For
#'           details on zoom and resolution see the documentation from Mapzen at
#'           \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution}.
#'           The z is not required for the OpenTopography data sources.
#' @inheritParams loc_check
#' @param src A character indicating which API to use.  Currently supports "aws"
#'            and "gl3", "gl1", "alos", or "srtm15plus" from the OpenTopography API global
#'            datasets. "aws" is the default.
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can
#'               be used for features that fall close to the edge of a tile or
#'               for retrieving additional area around the feature. If the
#'               feature is a single point, the area it returns will be small if
#'               clip is set to "bbox". Default is NULL.
#' @param clip A character value used to determine clipping of returned DEM.
#'             The default value is "tile" which returns the full tiles.  Other
#'             options are "bbox" which returns the DEM clipped to the bounding
#'             box of the original locations (or expanded bounding box if used),
#'             or "locations" if the spatial data (e.g. polygons) in the input
#'             locations should be used to clip the DEM.  Locations are not used
#'             to clip input point datasets.  Instead the bounding box is used.
#' @param verbose Toggles on and off the note about units and coordinate
#'                reference system.
#' @param neg_to_na Some of the data sources return large negative numbers as
#'                  missing data.  When the end result is a projected those
#'                  large negative numbers can vary.  When set to TRUE, only
#'                  zero and positive values are returned.  Default is FALSE.
#' @param override_size_check Boolean to override size checks.  Any download
#'                            between 100 Mb and 500Mb report a message but
#'                            continue.  Between 500Mb and 3000Mb requires
#'                            interaction and greater than 3000Mb fails.  These
#'                            can be overriden with this argument set to TRUE.
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a
#'                temporary location.  Alternatively, the user may supply an
#'                existing path for these raster files.  New folders are not
#'                created by \code{get_elev_raster}.
#' @param ncpu Number of CPU's to use when downloading aws tiles. Defaults to 2
#'             if more than two available, 1 otherwise.
#' @param coords Coordinate column names passed to [sf::st_as_sf()]. Defaults to
#'               `c("x", "y")`.
#' @param ... Extra arguments to pass to \code{httr::GET} via a named vector,
#'            \code{config}.   See
#'            \code{\link{get_aws_terrain}} for more details.
#' @return Function returns a \code{RasterLayer} in the projection
#'         specified by the \code{prj} argument or in the projection of the
#'         provided locations.  In subsequent versions, a \code{SpatRaster}
#'         will be returned.
#' @details Currently, the \code{get_elev_raster} function utilizes the
#'          Amazon Web Services
#'          (\url{https://registry.opendata.aws/terrain-tiles/}) terrain
#'          tiles and the Open Topography Global Datasets API
#'          (\url{https://opentopography.org/developers}).
#'
#'          The AWS Terrain Tiles data is provided via x, y, and z tiles (see
#'          \url{https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames} for
#'          details.) The x and y are determined from the bounding box of the
#'          object submitted for \code{locations} argument, and the z argument
#'          must be specified by the user.
#' @export
#' @examples
#' \dontrun{
#' library(elevatr)
#' library(sf)
#' data(lake)
#' lake_buff  <- st_buffer(lake, 1000)
#' loc_df <- data.frame(x = runif(6,min=sf::st_bbox(lake)$xmin,
#'                                max=sf::st_bbox(lake)$xmax),
#'                      y = runif(6,min=sf::st_bbox(lake)$ymin,
#'                                max=sf::st_bbox(lake)$ymax))
#'
#' x <- get_elev_raster(locations = loc_df, prj = st_crs(lake) , z=10)
#' x <- get_elev_raster(lake, z = 14)
#' x <- get_elev_raster(lake, src = "gl3", expand = 5000)
#' x <- get_elev_raster(lake_buff, z = 10, clip = "locations")
#' }

get_elev_raster <- function(locations, z, prj = NULL,
                            src = c("aws", "gl3", "gl1", "alos", "srtm15plus"),
                            expand = NULL, clip = c("tile", "bbox", "locations"),
                            verbose = TRUE, neg_to_na = FALSE,
                            override_size_check = FALSE, tmp_dir = tempdir(),
                            ncpu = ifelse(future::availableCores() > 2, 2, 1),
                            coords = c("x", "y"),
                            ...){
  # First Check for internet
  if(!curl::has_internet()) {
    message("Please connect to the internet and try again.")
    return(NULL)
  }

  tmp_dir <- normalizePath(tmp_dir, mustWork = TRUE)
  src  <- match.arg(src)
  clip <- match.arg(clip)

  # Check location type and if sf, set prj.  If no prj (for either) then error
  locations <- loc_check(locations, prj, coords = coords)

  if (is.null(prj)) {
    prj <- sf::st_crs(locations)
  }
   #need to check what is going on with PRJ when no prj passed.
  # Check download size and provide feedback, stop if too big!
  dl_size <- estimate_raster_size(locations, prj, src, z)
  if(dl_size > 500 & dl_size < 1000){
    message(paste0("Note: Your request will download approximately ",
                   round(dl_size, 1), "Mb."))
  } else if(dl_size > 1000 & dl_size <= 3000){
    message(paste0("Your request will download approximately ",
                   round(dl_size, 1), "Mb."))
    if(!override_size_check){
      y <- readline(prompt = "Press [y] to continue with this request.")
      if(tolower(y) != "y"){return()}
    }
  } else if(!override_size_check & dl_size > 3000){
    stop(paste0("Your request will download approximately ",
                   round(dl_size, 1), "Mb. That's probably too big. If you
                   really want to do this, set override_size_check = TRUE. Note
                   that the OpenTopography API Limit will likely be exceeded."))
  }


  # Pass of locations to APIs to get data as raster
  if(src == "aws") {
    raster_elev <- get_aws_terrain(locations, z, prj = prj, expand = expand, 
                                   tmp_dir = tmp_dir, ncpu = ncpu, ...)
  } else if(src %in% c("gl3", "gl1", "alos", "srtm15plus")){
    raster_elev <- get_opentopo(locations, src, prj = prj, expand = expand,
                                tmp_dir = tmp_dir, ...)
  }
  sources <- attr(raster_elev, "sources")
  if(is.null(sources)){sources <- src}

  if(clip != "tile"){
    message(paste("Clipping DEM to", clip))

    raster_elev <- clip_it(raster_elev, locations, expand, clip)
  }

  if (verbose){
    message(paste("Note: Elevation units are in meters."))
  }


  if (neg_to_na){
    raster_elev[raster_elev < 0] <- NA
  }

  attr(raster_elev, "sources") <- sources

  if (requireNamespace("raster", quietly = TRUE)) {
    # Return raster if raster package is installed
    raster::raster(raster_elev)
  } else {
    # Otherwise return SpatRaster
    terra::rast(raster_elev)
  }
}

#' Get a digital elevation model from the AWS Terrain Tiles
#'
#' This function uses the AWS Terrain Tile service to retrieve an elevation
#' raster from the geotiff service.  It accepts a \code{sf::st_bbox} object as
#' input and returns a single raster object covering that extent.
#'
#' @source Attribution: Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010
#'         content courtesy of the U.S. Geological Survey and ETOPO1 content
#'         courtesy of U.S. National Oceanic and Atmospheric Administration.
#'         \url{https://github.com/tilezen/joerd/tree/master/docs}
#'
#' @param locations Either a \code{data.frame} of x (long) and y (lat), an
#'                  \code{sp}, \code{sf}, or \code{raster} object as input.
#' @param z The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'          of the resultant raster is determined by the zoom and latitude.  For
#'          details on zoom and resolution see the documentation from Mapzen at
#'          \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution}
#' @inheritParams loc_check
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can
#'               be used for features that fall close to the edge of a tile and
#'               additional area around the feature is desired. Default is NULL.
#' @param ncpu Number of CPU's to use when downloading aws tiles. Defaults to 2 
#'             if more than two available, 1 otherwise.  
#' @param serial Logical to determine if API should be hit in serial or in 
#'               parallel.  TRUE will use purrr, FALSE will use furrr. 
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a 
#'                temporary location.  Alternatively, the user may supply an 
#'                existing path for these raster files.  New folders are not 
#'                created by \code{get_elev_raster}.
#' @param ... Extra configuration parameters to be passed to httr::GET.  Common
#'            usage is to adjust timeout.  This is done as
#'            \code{config=timeout(x)} where \code{x} is a numeric value in
#'            seconds.  Multiple configuration functions may be passed as a
#'            vector.
#' @export
#' @importFrom progressr handlers progressor with_progress
#' @keywords internal

get_aws_terrain <- function(locations, z, prj, expand=NULL, 
                            ncpu = ifelse(future::availableCores() > 2, 2, 1),
                            serial = NULL, tmp_dir = tempdir(), ...){
  # Expand (if needed) and re-project bbx to dd

  bbx <- proj_expand(locations,prj,expand)

  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff"


  tiles <- get_tilexy(bbx,z)

  urls  <-  sprintf("%s/%s/%s/%s.tif", base_url, z, tiles[,1], tiles[,2])

  for(i in urls){
    if(httr::http_error(i)) {
      message("An AWS URL is invalid.")
      return(NULL)
    }
  }


  dir <- tempdir()

  nurls <- length(urls)
  if(is.null(serial)){
    if(nurls < 175){
      serial <- TRUE
    } else {
      serial <- FALSE
    }
  }

  progressr::handlers(
    progressr::handler_progress(
      format = " Accessing raster elevation [:bar] :percent",
      clear = FALSE,
      width= 60
    ))

  progressr::with_progress({
  if(serial){

    p <- progressr::progressor(along = urls)
    dem_list <- purrr::map(urls,
                           function(x){
                             p()
                             tmpfile <- tempfile(tmpdir = tmp_dir,
                                                 fileext = ".tif")
                             resp <- httr::GET(x,
                                               httr::user_agent("elevatr R package (https://github.com/usepa/elevatr)"),
                                               httr::write_disk(tmpfile,overwrite=TRUE), ...)
                             if (!grepl("image/tif", httr::http_type(resp))) {
                               stop(paste("This url:", x,"did not return a tif"), call. = FALSE)
                             }
                             tmpfile2 <- tmpfile
                             attr(tmpfile2, "source") <-
                               httr::headers(resp)$'x-amz-meta-x-imagery-sources'
                             tmpfile2
                           })
  } else {
    future::plan(future::multisession, workers = ncpu)
    p <- progressr::progressor(along = urls)
    dem_list <- furrr::future_map(urls,
                                  function(x){
                                    p()
                                    tmpfile <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
                                    resp <- httr::GET(x,
                                                      httr::user_agent("elevatr R package (https://github.com/usepa/elevatr)"),
                                                      httr::write_disk(tmpfile,overwrite=TRUE), ...)
                                    if (!grepl("image/tif", httr::http_type(resp))) {
                                      stop(paste("This url:", x,"did not return a tif"), call. = FALSE)
                                    }
                                    tmpfile2 <- tmpfile
                                    attr(tmpfile2, "source") <-
                                      httr::headers(resp)$'x-amz-meta-x-imagery-sources'
                                    tmpfile2
                                  })
  }
  })

  merged_elevation_grid <- merge_rasters(dem_list, target_prj = prj, tmp_dir = tmp_dir)
  sources <- unlist(lapply(dem_list, function(x) attr(x, "source")))
  if(!is.null(sources)){
    sources <- trimws(unlist(strsplit(sources, ",")))
    sources <- strsplit(sources, "/")
    sources <- unlist(unique(lapply(sources, function(x) x[1])))
  }
  attr(merged_elevation_grid, "sources") <-
    paste(sources, collapse = ",")

  if(serial==FALSE){future::plan(future::sequential)}

  merged_elevation_grid
}

#' Merge Rasters
#'
#' Merge multiple downloaded raster files into a single file. The input `target_prj`
#' describes the projection for the new grid.
#'
#' @param raster_list a list of raster file paths to be mosaiced
#' @param target_prj the target projection of the output raster
#' @param method the method for resampling/reprojecting. Default is 'bilinear'.
#' Options can be found [here](https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-r)
#' @param returnRaster if TRUE, return a raster object (default), else, return the file path to the object
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a
#'                temporary location.  Alternatively, the user may supply an
#'                existing path for these raster files.  New folders are not
#'                created by \code{get_elev_raster}.
#' @export
#' @keywords internal

merge_rasters <- function(raster_list,  target_prj, method = "bilinear",
                          returnRaster = TRUE, tmp_dir = tempdir()){

  message(paste("Mosaicing & Projecting"))

  destfile <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
  files    <- unlist(raster_list)

  if(is.null(target_prj)){
    r <- terra::rast(files[1])
    target_prj <- terra::crs(r)
  }

  sf::gdal_utils(util = "warp",
                 source = files,
                 destination = destfile,
                 options = c("-r", method)
             )
  # Using two steps now as gdal with one step introduced NA's along seams
  # Slower but more accurate!
  destfile2 <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
  sf::gdal_utils(util = "warp",
                 source = destfile,
                 destination = destfile2,
                 options = c("-r", method,
                   "-t_srs", sf::st_crs(target_prj)$wkt)
  )

  if(returnRaster){
    terra::rast(destfile2)
  } else {
    destfile2
  }
}

#' Get a digital elevation model from the Open Topography SRTM Version 3
#'
#' This function uses the Open Topography SRTM Version 3 files.
#'
#' @source Attribution: Details here
#'
#' @param locations Either a \code{data.frame} of x (long) and y (lat), an
#'                  \code{sp}, an \code{sf}, or \code{raster} object as input.
#' @inheritParams loc_check
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the SRTM data.
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a
#'                temporary location.  Alternatively, the user may supply an
#'                existing path for these raster files.  New folders are not
#'                created by \code{get_elev_raster}.
#' @param ... Extra configuration parameters to be passed to httr::GET.  Common
#'            usage is to adjust timeout.  This is done as
#'            \code{config=timeout(x)} where \code{x} is a numeric value in
#'            seconds.  Multiple configuration functions may be passed as a
#'            vector.
#' @export
#' @keywords internal
get_opentopo <- function(locations, src, prj, expand=NULL, tmp_dir = tempdir(),
                         ...){

  api_key <- get_opentopo_key()

  # Expand (if needed) and re-project bbx to ll_geo
  bbx <- proj_expand(locations,prj,expand)

  tmpfile <- tempfile(tmpdir = tmp_dir)
  base_url <- "https://portal.opentopography.org/API/globaldem?demtype="
  data_set <- switch(src,
                     gl3 = "SRTMGL3",
                     gl1 = "SRTMGL1",
                     alos = "AW3D30",
                     srtm15plus = "SRTM15Plus")

  url <- paste0(base_url, data_set,
                "&west=",min(bbx["xmin"]),
                "&south=",min(bbx["ymin"]),
                "&east=",max(bbx["xmax"]),
                "&north=",max(bbx["ymax"]),
                "&outputFormat=GTiff",
                "&API_Key=", api_key)

  if(httr::http_error(url)) {
    message("The OpenTopography URL is invalid.")
    return(NULL)
  }

  message("Downloading OpenTopography DEMs")
  resp <- httr::GET(url,httr::write_disk(tmpfile,overwrite=TRUE),
                    httr::user_agent("elevatr R package (https://github.com/usepa/elevatr)"),
                    httr::progress(), ...)
  message("")
  if (httr::http_type(resp) != "application/octet-stream") {
    stop("API did not return octet-stream as expected", call. = FALSE)
  }
  dem <- merge_rasters(tmpfile, target_prj = prj, tmp_dir = tmp_dir)
  dem
}

#' Store OpenTopography Key
#'
#' This function stores an OpenTopgrapy key in a local .Renviron file. If the
#' .Renviron file exists, the key will be appended. This will typically only
#' need to be done once per machine.
#'
#'
#' @param key An OpenTopography API Key as a character.  For details on obtaining an
#'            OpenTopgraphy key see \url{https://opentopography.org/blog/introducing-api-keys-access-opentopography-global-datasets}.
#' @export
set_opentopo_key <- function(key){
  home <- normalizePath("~/")
  if(Sys.getenv("OPENTOPO_KEY")==""){
    cat(paste0("OPENTOPO_KEY=", key, "\n"), file = paste0(home, "/.Renviron"),
        append = TRUE)
    message("Your OpenTopography Key has been added to .Renviron.
            You will need to restart R for the changes to take effect.")
  } else {
    message("An existing OpenTopography Key already exists.
            To edit try usethis::edit_r_environ().")
  }
}
