context("get_elev_raster")
data("pt_df")
data("sp_big")
library(sp)
ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
aea_prj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
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

