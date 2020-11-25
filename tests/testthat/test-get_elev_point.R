context("get_elev_point")
library(sp)
library(sf)
library(elevatr)
data("pt_df")
data("sp_big")


ll_prj  <- "GEOGCRS[\"unknown\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]],\n        ID[\"EPSG\",6326]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433],\n        ID[\"EPSG\",8901]],\n    CS[ellipsoidal,2],\n        AXIS[\"longitude\",east,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]],\n        AXIS[\"latitude\",north,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]]"
aea_prj <- "PROJCRS[\"unknown\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"Unknown based on GRS80 ellipsoid\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1],\n                ID[\"EPSG\",7019]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8901]]],\n    CONVERSION[\"unknown\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",40,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",20,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"


sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sf_sm <- st_as_sf(sp_sm)
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))
bad_sp <- SpatialPoints(coordinates(data.frame(x = 1000, y = 1000)),
                        CRS(ll_prj))

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
