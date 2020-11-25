context("internals not yet caught")
library(sp)
library(raster)
library(rgdal)
library(elevatr)
data("pt_df")
data("sp_big")
data("lake")

ll_prj  <- "GEOGCRS[\"unknown\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]],\n        ID[\"EPSG\",6326]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433],\n        ID[\"EPSG\",8901]],\n    CS[ellipsoidal,2],\n        AXIS[\"longitude\",east,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]],\n        AXIS[\"latitude\",north,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]]"
aea_prj <- "PROJCRS[\"unknown\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"Unknown based on GRS80 ellipsoid\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1],\n                ID[\"EPSG\",7019]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8901]]],\n    CONVERSION[\"unknown\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",40,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",20,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))

sp::proj4string(sp_sm) <- ""
spdf_sm <- SpatialPointsDataFrame(sp_sm, data.frame(1:nrow(coordinates(sp_sm))))

rast <- rasterize(coordinates(spdf_sm),raster(spdf_sm))
#rast_prj <- raster::projection(rast) <- aea_prj

mt_wash <- data.frame(x = -71.3036, y = 44.2700)
mt_mans <- data.frame(x = -72.8145, y = 44.5438)
mts <- rbind(mt_wash,mt_mans)
mts$name <- c("Mount Washington", "Mount Mansfield")

test_that("data frame with more extra columns work", {
  skip_on_cran()
  #skip_on_ci()
  
  mts_with_names_and_elevation <- get_elev_point(mts, ll_prj)
  expect_true("name" %in% names(mts_with_names_and_elevation))
})

test_that("proj_expand works",{
  skip_on_cran()
  
  mans_sp <- SpatialPoints(coordinates(data.frame(x = -72.8145, y = 44.5438)),
                           CRS(ll_prj))
  mans <- get_elev_raster(locations =  mans_sp, z = 6)
  mans_exp <- get_elev_raster(locations = mans_sp, z = 6, expand = 2)
  
  origin_sp <- SpatialPoints(coordinates(data.frame(x = 0, y = 0)),
                             CRS(ll_prj))
  origins <- get_elev_raster(locations = origin_sp, z = 6)
  
  expect_gt(ncell(mans_exp),ncell(mans))
  
  expect_is(origins, "RasterLayer")
})

test_that("loc_check errors correctly", {
  skip_on_cran()
  #skip_on_ci()
  expect_error(get_elev_point(locations = pt_df), 
               "Please supply a valid WKT string.")
  expect_error(get_elev_point(locations = sp_sm), 
               "Please supply a valid WKT string.")
  expect_error(get_elev_point(locations = spdf_sm),
               "Please supply a valid WKT string.")
  expect_error(get_elev_point(locations = rast),
               "Please supply a valid WKT string.")
  expect_error(get_elev_point(locations = raster(sp_sm), prj = ll_prj),
               "No distinct points, all values NA.")
})

test_that("loc_check assigns prj correctly",{
  skip_on_cran()
  #skip_on_ci()
  expect_equal(wkt(get_elev_point(locations = sp_sm, prj = ll_prj)),
                           ll_prj)
  expect_equal(wkt(get_elev_point(locations = spdf_sm, prj = ll_prj)),
                           ll_prj)
  expect_equal(wkt(get_elev_point(locations = rast, prj = ll_prj)), 
               ll_prj)
})

test_that("Z of 1 or 0 works in get_tilexy",{
  skip_on_cran()
  
  sp_sm_1 <- get_elev_raster(sp_sm_prj, z = 1, clip = "bbox")
  sp_sm_0 <- get_elev_raster(sp_sm_prj, z = 0, clip = "bbox")
  
  expect_gt(max(res(sp_sm_1)), 0.27)
  expect_gt(max(res(sp_sm_0)), 0.54)
})
