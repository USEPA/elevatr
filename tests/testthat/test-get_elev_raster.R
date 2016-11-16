context("get_elev_raster")
data("pt_df")
data("sp_big")
library(sp)


ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
aea_prj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))

test_that("get_elev_raster returns correctly", {
  #skip_on_cran()
  
  mz_df <- get_elev_raster(locations = pt_df,prj = ll_prj, api_key = NULL, 
                           z = 6)
  mz_sp <- get_elev_raster(locations = sp_sm, api_key = NULL, 
                           z = 6)
  mz_sp_prj <- get_elev_raster(locations = sp_sm_prj, api_key = NULL, 
                               z = 6)
  #class
  expect_is(mz_df, "RasterLayer")
  expect_is(mz_sp, "RasterLayer")
  expect_is(mz_sp_prj, "RasterLayer")
  
  #project
  expect_equal(proj4string(mz_df),ll_prj)
  expect_equal(proj4string(mz_sp),ll_prj)
  expect_equal(proj4string(mz_sp_prj),aea_prj)

})
