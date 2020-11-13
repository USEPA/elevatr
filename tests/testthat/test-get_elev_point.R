context("get_elev_point")
library(sp)
library(sf)
library(elevatr)
data("pt_df")
data("sp_big")


ll_prj  <- "+proj=longlat +datum=WGS84 +no_defs"
aea_prj <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sf_sm <- st_as_sf(sp_sm)
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))
bad_sp <- SpatialPoints(coordinates(data.frame(x = 1000, y = 1000)),
                        CRS(ll_prj))

test_that("get_elev_point returns correctly", {
  #skip_on_cran()
  #skip_on_appveyor()
  epqs_df <- get_elev_point(locations = pt_df, prj = ll_prj, src = "epqs")
  epqs_sp <- get_elev_point(locations = sp_sm, src = "epqs")
  epqs_sf <- get_elev_point(locations = sf_sm, src = "epqs")
  epqs_sp_prj <- get_elev_point(locations = sp_sm_prj, src = "epqs")
  epqs_ft <- get_elev_point(locations = sp_sm, src = "epqs", units = "feet")
  epqs_m <- get_elev_point(locations = sp_sm, src = "epqs", units = "meters")
  epqs_df_aws <- get_elev_point(locations = pt_df, prj = ll_prj, src = "aws")
  epqs_sp_aws <- get_elev_point(locations = sp_sm, src = "aws")
  epqs_sp_aws_z <- get_elev_point(locations = sp_sm, src = "aws", z = 4)
  epqs_sf_aws <- get_elev_point(locations = sf_sm, src = "aws")
  epqs_ft_aws <- get_elev_point(locations = sp_sm, src = "aws", units = "feet")
  
  
  
  #class
  expect_is(epqs_df, "SpatialPointsDataFrame")
  expect_is(epqs_sp, "SpatialPointsDataFrame")
  expect_is(epqs_sp_prj, "SpatialPointsDataFrame")
  expect_is(epqs_sp_prj, "SpatialPointsDataFrame")
  expect_is(epqs_sf, "sf")
  
  #proj
  expect_equal(proj4string(sp_sm),proj4string(epqs_sp))
  expect_equal(proj4string(sp_sm_prj),proj4string(epqs_sp_prj))
  expect_equal(proj4string(sp_sm),proj4string(epqs_sp_aws))
  
  #units
  expect_equal(epqs_ft$elev_units[1],"feet")
  expect_equal(epqs_m$elev_units[1],"meters")
  expect_equal(epqs_ft_aws$elev_units[1],"feet")
  expect_equal(epqs_sf_aws$elev_units[1],"meters")
  
})
