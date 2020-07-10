context("get_elev_raster")
data("pt_df")
data("sp_big")
data("lake")
library(sp)
#ll_prj  <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
 ll_prj  <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#aea_prj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
 aea_prj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

sp_sm     <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))

test_that("get_elev_raster returns correctly", {
  skip_on_cran()
  skip_on_appveyor()
  
  aws <- get_elev_raster(locations = sp_sm, z = 6, src = "aws")
  aws_prj <- get_elev_raster(locations = sp_sm_prj, z = 6, src = "aws")
  
  #class
  expect_is(aws,"RasterLayer")
  expect_is(aws_prj,"RasterLayer")
  
  #project
  expect_equal(proj4string(aws),ll_prj)
  expect_equal(proj4string(aws_prj),aea_prj)

})

test_that("get_elev_raster clip argument works", {
  skip_on_cran()
  skip_on_appveyor()
  
  default_clip <- get_elev_raster(lake, z = 5, clip = "tile")
  bbox_clip <- get_elev_raster(lake, z = 5, clip = "bbox")
  locations_clip <- get_elev_raster(lake, z = 5, clip = "locations")
  
  default_values <- raster::getValues(default_clip)
  num_cell_default <- length(default_values[!is.na(default_values)])
  bbox_values <- raster::getValues(bbox_clip)
  num_cell_bbox <- length(bbox_values[!is.na(bbox_values)])
  locations_values <- raster::getValues(locations_clip)
  num_cell_locations <- length(locations_values[!is.na(locations_values)])
  
  expect_true(num_cell_default > num_cell_bbox)
  expect_true(num_cell_bbox > num_cell_locations)
})

