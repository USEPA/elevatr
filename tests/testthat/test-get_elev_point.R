context("get_elev_point")
data("pt_df")
data("sp_big")
library(sp)

ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
aea_prj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))

test_that("get_elev_point returns correctly", {
  skip_on_cran()
  skip_on_appveyor()
  key <- readRDS("key_file.rds")
  mz_df <- get_elev_point(locations = pt_df,prj = ll_prj, api_key = key)
  Sys.sleep(10)
  mz_sp <- get_elev_point(locations = sp_big, api_key = key)
  Sys.sleep(10)
  mz_sp_prj <- get_elev_point(locations = sp_sm_prj, api_key = key)
  Sys.sleep(10)
  mz_sp_200 <- get_elev_point(locations = sp_big[1:200,], api_key = key)
  epqs_df <- get_elev_point(locations = pt_df, prj = ll_prj, src = "epqs")
  epqs_sp <- get_elev_point(locations = sp_sm, src = "epqs")
  epqs_sp_prj <- get_elev_point(locations = sp_sm_prj, src = "epqs")
  epqs_ft <- get_elev_point(locations = sp_sm, src = "epqs", units = "feet")
  epqs_m <- get_elev_point(locations = sp_sm, src = "epqs", units = "meters")
  
  #class
  expect_is(mz_df, "SpatialPointsDataFrame")
  expect_is(mz_sp, "SpatialPointsDataFrame")
  expect_is(mz_sp_200, "SpatialPointsDataFrame")
  expect_is(mz_sp_prj, "SpatialPointsDataFrame")
  expect_is(epqs_df, "SpatialPointsDataFrame")
  expect_is(epqs_sp, "SpatialPointsDataFrame")
  expect_is(epqs_sp_prj, "SpatialPointsDataFrame")
  expect_is(epqs_sp_prj, "SpatialPointsDataFrame")
  
  #proj
  expect_equal(proj4string(sp_sm),proj4string(mz_sp))
  expect_equal(proj4string(sp_sm_prj),proj4string(mz_sp_prj))
  expect_equal(proj4string(sp_sm),proj4string(epqs_sp))
  expect_equal(proj4string(sp_sm_prj),proj4string(epqs_sp_prj))
  
  #units
  expect_equal(epqs_ft$elev_units[1],"feet")
  expect_equal(epqs_m$elev_units[1],"meters")
  
})
