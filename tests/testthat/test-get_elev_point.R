context("get_elev_point")
library(sf)
library(elevatr)
data("pt_df")
data("sf_big")
skip_on_cran()
#skip_on_os("solaris")
ll_prj  <- 4326
aea_prj <- 5072

sf_sm <- st_as_sf(pt_df, coords = c("x", "y"), crs = ll_prj)
sf_sm_prj <- st_transform(sf_sm, crs = aea_prj) 
bad_sf <- st_as_sf(data.frame(x = 1000, y = 1000), coords = c("x", "y"), 
                   crs = ll_prj)

test_that("get_elev_point returns correctly", {
  
  epqs_df <- get_elev_point(locations = pt_df, prj = ll_prj, src = "epqs")
  epqs_sf <- get_elev_point(locations = sf_sm, src = "epqs")
  epqs_sf_prj <- get_elev_point(locations = sf_sm_prj, src = "epqs")
  epqs_ft <- get_elev_point(locations = sf_sm, src = "epqs", units = "feet")
  epqs_m <- get_elev_point(locations = sf_sm, src = "epqs", units = "meters")
  epqs_df_aws <- get_elev_point(locations = pt_df, prj = ll_prj, src = "aws")
  epqs_sf_aws <- get_elev_point(locations = sf_sm, src = "aws")
  epqs_sf_aws_z <- get_elev_point(locations = sf_sm, src = "aws", z = 4)
  epqs_sf_aws <- get_elev_point(locations = sf_sm, src = "aws")
  epqs_ft_aws <- get_elev_point(locations = sf_sm, src = "aws", units = "feet")
  
  
  
  # class
  expect_is(epqs_df, "sf")
  expect_is(epqs_sf, "sf")
  expect_is(epqs_sf_prj, "sf")
  
  # crs
  expect_equal(st_crs(sf_sm)$wkt,st_crs(epqs_sf)$wkt)
  expect_equal(st_crs(sf_sm_prj)$wkt,st_crs(epqs_sf_prj)$wkt)
  expect_equal(st_crs(sf_sm)$wkt,st_crs(epqs_sf_aws)$wkt)
  
  
  # units
  expect_equal(epqs_ft$elev_units[1],"feet")
  expect_equal(epqs_m$elev_units[1],"meters")
  expect_equal(epqs_ft_aws$elev_units[1],"feet")
  expect_equal(epqs_sf_aws$elev_units[1],"meters")
  
})
