#' Get Point Elevation along a Profile Line
#'
#' [get_elev_profile()] allows users to provide LINESTRING inputs to
#' [sf::st_line_sample()] or to cast LINESTRING to POINT before calling
#' [get_elev_point()] to get the point elevations. The function allows users to
#' get elevation along a profile line and, optionally, include a distance or
#' cumulative distance column in the output sf data frame.
#'
#' @inheritParams loc_linestring_to_point
#' @param include Option of columns to include: one of "default", "dist", or
#'   "cumdist". Default value returns same columns as [get_elev_point()]. If
#'   `include = "dist"`, the returned locations include the distance between
#'   each successive pair of points. If `include = "cumdist"`, the distances are
#'   provided as a cumulative sum.
#' @inheritParams get_elev_point
#' @param dist_col Column name to use for optional distance column. Only used if
#'   `include` is set to `"dist"` or `"cumdist"`.
#' @examples
#' \dontrun{
#' library(sf)
#' library(elevatr)
#'
#' nc <- st_read(system.file("shape/nc.shp", package = "sf")) |>
#'   st_transform(3857)
#'
#' nc_line <- suppressWarnings(
#'   st_cast(
#'     st_union(
#'       st_centroid(nc[1, ]),
#'       st_centroid(nc[2, ])
#'     ),
#'     to = "LINESTRING"
#'   )
#' )
#'
#' elev_point <- get_elev_profile(
#'   nc_line,
#'   units = "ft",
#'   dist = TRUE,
#'   cumulative = TRUE,
#'   n = 10
#' )
#'
#' elev_point
#'
#' }
#' @export
get_elev_profile <- function(locations,
                             n = NULL,
                             density = NULL,
                             type = "regular",
                             sample = NULL,
                             units = NULL,
                             include = c("default", "dist", "cumdist"),
                             ...,
                             prj = NULL,
                             overwrite = FALSE,
                             coords = c("x", "y"),
                             elev_col = "elevation",
                             elev_units_col = "elev_units",
                             dist_col = "distance") {
  locations <- loc_check(locations, prj = prj, elev_col = elev_col, coords = coords)

  if (sf::st_is(locations, "LINESTRING")) {
    location_coords <- loc_linestring_to_point(
      locations,
      n = n,
      density = density,
      type = type,
      sample = sample
    )
  }

  stopifnot(
    "`locations` must use POINT or LINESTRING geometry" = sf::st_is(locations, "POINT")
  )

  if (inherits(locations, c("sfc", "sf")) && is.null(prj)) {
    prj <- sf::st_crs(locations)
  }

  elev_point <- get_elev_point(
    locations,
    prj = prj,
    units = units,
    elev_col = elev_col,
    elev_units_col = elev_units_col
  )

  include <- match.arg(include)

  if (include != "default") {
    cumulative <- include == "cumdist"

    dist_values <- st_point_distances(
      elev_point,
      cumulative = cumulative,
      units = units,
      prj = prj
    )

    locations[[dist_col]] <- dist_values
    locations <- relocate_sf_col_end(locations)
  }

  locations
}
