context("get_elev_raster")
library(sp)
library(elevatr)
data("pt_df")
data("sp_big")
data("lake")
#skip_on_os("solaris")
ll_prj  <- "EPSG:4326"
aea_prj <- "EPSG:5072"

sp_sm <- SpatialPoints(coordinates(pt_df),
                       CRS(SRS_string = ll_prj))
sp_sm_prj <- spTransform(sp_sm, CRS(SRS_string = aea_prj))
bad_sp <- SpatialPoints(coordinates(data.frame(x = 1000, y = 1000)),
                        CRS(SRS_string = ll_prj))

test_that("get_elev_raster returns correctly", {
  skip_on_cran()
  
  aws <- get_elev_raster(locations = sp_sm, z = 6, src = "aws")
  aws_prj <- get_elev_raster(locations = sp_sm_prj, z = 6, src = "aws")
  
  #class
  expect_is(aws,"RasterLayer")
  expect_is(aws_prj,"RasterLayer")
  
  #project
  #expect_equal(wkt(aws),ll_prj)
  #expect_equal(wkt(aws_prj),aea_prj)

})

test_that("get_elev_raster clip argument works", {
  skip_on_cran()
  
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

test_that("get_elev_raster returns correctly from opentopo", {
  skip_on_cran()
  
  gl1 <- get_elev_raster(locations = sp_sm[3:4,], src = "gl1", neg_to_na = TRUE)
  gl1_prj <- get_elev_raster(locations = sp_sm_prj[3:4,], src = "gl1", 
                             clip = "bbox")
  
  #class
  expect_is(gl1,"RasterLayer")
  expect_is(gl1_prj,"RasterLayer")
  
  #project
  #expect_equal(wkt(gl1),ll_prj$wkt)
  #expect_equal(wkt(gl1_prj),aea_prj$wkt)
  
})

test_that("A bad location file errors",{
  
  expect_error(suppressWarnings(get_elev_raster(bad_sp, z = 6)))
  expect_error(suppressWarnings(get_elev_raster(bad_sp, src = "gl3")))
})

