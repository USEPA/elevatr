context("get_elev_raster")
library(sf)
library(elevatr)
library(terra)
data("pt_df")
data("sf_big")
data("lake")
skip_on_cran()
#skip_on_os("solaris")
ll_prj  <- 4326
aea_prj <- 5072

sf_sm <- st_as_sf(pt_df, coords = c("x", "y"), crs = ll_prj)
sf_sm_prj <- st_transform(sf_sm, crs = aea_prj) 
bad_sf <- st_as_sf(data.frame(x = 1000, y = 1000), coords = c("x", "y"), 
                   crs = ll_prj)
blank_raster <- rast(sf_sm, nrow = 5, ncol = 5, vals = 1)
sf_sm_raster <- rasterize(sf_sm, rast(sf_sm, nrow = 10, ncol = 10))

test_that("get_elev_raster returns correctly", {
  
  aws <- get_elev_raster(locations = sf_sm, z = 6, src = "aws")
  aws_prj <- get_elev_raster(locations = sf_sm_prj, z = 6, src = "aws")
  aws_blnk_raster <- get_elev_raster(locations = blank_raster, z = 6, src = "aws")
  aws_sf_raster <- get_elev_raster(locations = sf_sm_raster, z = 6, src = "aws")
  
  #class
  expect_is(aws,"SpatRaster")
  expect_is(aws_prj,"SpatRaster")
  expect_is(aws_blnk_raster, "SpatRaster")
  expect_is(aws_sf_raster, "SpatRaster")
  
  #project
  #expect_equal(st_crs(aws)$wkt,st_crs(ll_prj)$wkt)
  expect_equal(st_crs(aws_prj)$wkt,st_crs(aea_prj)$wkt)

})

test_that("get_elev_raster clip argument works", {
  
  default_clip <- get_elev_raster(lake, z = 5, clip = "tile")
  bbox_clip <- get_elev_raster(lake, z = 5, clip = "bbox")
  locations_clip <- get_elev_raster(lake, z = 5, clip = "locations")
  spat_rast_tile <- get_elev_raster(locations = sf_sm_raster, z = 5, 
                                    src = "aws", clip = "tile")
  spat_rast_loc <- get_elev_raster(locations = sf_sm_raster, z = 5, 
                                    src = "aws", clip = "locations")
  
  default_values <- terra::values(default_clip)
  num_cell_default <- length(default_values[!is.na(default_values)])
  bbox_values <- terra::values(bbox_clip)
  num_cell_bbox <- length(bbox_values[!is.na(bbox_values)])
  locations_values <- terra::values(locations_clip)
  num_cell_locations <- length(locations_values[!is.na(locations_values)])
  default_spat_rast <- terra::values(spat_rast_tile)
  num_cell_default_spat_rast <- length(default_spat_rast[!is.na(default_spat_rast)])
  loc_spat_rast <- terra::values(spat_rast_loc)
  num_cell_loc_spat_rast <- length(loc_spat_rast[!is.na(loc_spat_rast)])
  
  
  expect_true(num_cell_default > num_cell_bbox)
  expect_true(num_cell_bbox > num_cell_locations)
  expect_true(num_cell_default_spat_rast > num_cell_loc_spat_rast)
})

test_that("get_elev_raster returns correctly from opentopo", {
  skip_on_os("solaris")
  
  gl1 <- get_elev_raster(locations = sf_sm[3:4,], src = "gl1", neg_to_na = TRUE)
  gl1_prj <- get_elev_raster(locations = sf_sm_prj[3:4,], src = "gl1", 
                             clip = "bbox")
  
  #class
  expect_is(gl1,"SpatRaster")
  expect_is(gl1_prj,"SpatRaster")
  
  #project
  #expect_equal(st_crs(gl1)$wkt,st_crs(ll_prj)$wkt)
  expect_equal(st_crs(gl1_prj)$wkt,st_crs(aea_prj)$wkt)
  
})

test_that("A bad location file errors",{
  
  expect_error(suppressWarnings(get_elev_raster(bad_sf, z = 6)))
  expect_error(suppressWarnings(get_elev_raster(bad_sf, src = "gl3")))
})

test_that("Parallel processing works",{
  serial_elev <- get_elev_raster(sf_sm, z = 6, serial = FALSE)
  
  #class
  expect_is(serial_elev,"SpatRaster")
  
  #same size as serial
  expect_equal(ncell(serial_elev),ncell(aws))
})
