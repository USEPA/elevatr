context("get_elev_point")
library(sp)
library(sf)
library(elevatr)
data("pt_df")
data("sp_big")
#skip_on_os(os = "solaris")
#if(R.version$major == "3" & R.version$minor == "6.2"){
#  skip("Skipping on R Version 3.6.2")
#}

ll_prj  <- st_crs(4326)
aea_prj <- st_crs(5072)

sp_sm <- SpatialPoints(coordinates(pt_df),
                       CRS(SRS_string = paste0("EPSG:", ll_prj$epsg)))
sp_sm_prj <- spTransform(sp_sm, CRS(SRS_string = paste0("EPSG:", aea_prj$epsg)))
bad_sp <- SpatialPoints(coordinates(data.frame(x = 1000, y = 1000)),
                        CRS(SRS_string = paste0("EPSG:", ll_prj$epsg)))
sf_sm <- st_as_sf(sp_sm)

test_that("get_elev_point returns correctly", {
  skip_on_cran()
  #skip_on_ci()
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
  expect_equal(wkt(sp_sm),wkt(epqs_sp))
  expect_equal(wkt(sp_sm_prj),wkt(epqs_sp_prj))
  expect_equal(wkt(sp_sm),wkt(epqs_sp_aws))
  
  #units
  expect_equal(epqs_ft$elev_units[1],"feet")
  expect_equal(epqs_m$elev_units[1],"meters")
  expect_equal(epqs_ft_aws$elev_units[1],"feet")
  expect_equal(epqs_sf_aws$elev_units[1],"meters")
  
})
