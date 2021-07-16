context("internals not yet caught")
library(sp)
library(sf)
library(raster)
library(rgdal)
library(elevatr)
data("pt_df")
data("sp_big")
data("lake")

ll_prj  <- "EPSG:4326"
aea_prj <- "EPSG:5072"

sp_sm <- SpatialPoints(coordinates(pt_df),
                       CRS(SRS_string = ll_prj))
sp_sm_prj <- spTransform(sp_sm, CRS(SRS_string = aea_prj))
bad_sp <- SpatialPoints(coordinates(data.frame(x = 1000, y = 1000)),
                        CRS(SRS_string = ll_prj))

sp_sm <- SpatialPoints(coordinates(pt_df),
                       CRS(SRS_string = ""))
spdf_sm <- SpatialPointsDataFrame(sp_sm, data.frame(1:nrow(coordinates(sp_sm))))

rast <- rasterize(coordinates(spdf_sm),raster(spdf_sm))

mt_wash <- data.frame(x = -71.3036, y = 44.2700)
mt_mans <- data.frame(x = -72.8145, y = 44.5438)
mts <- rbind(mt_wash,mt_mans)
mts$name <- c("Mount Washington", "Mount Mansfield")

test_that("data frame with more extra columns work", {
  skip_on_cran()
  
  mts_with_names_and_elevation <- get_elev_point(mts, ll_prj)
  expect_true("name" %in% names(mts_with_names_and_elevation))
})

test_that("proj_expand works",{
  skip_on_cran()
  
  suppressWarnings({
  mans_sp <- SpatialPoints(coordinates(data.frame(x = -72.8145, y = 44.5438)),
                           CRS(SRS_string = ll_prj))
  mans <- get_elev_raster(locations =  mans_sp, z = 6)
  mans_exp <- get_elev_raster(locations = mans_sp, z = 6, expand = 2)
  
  origin_sp <- SpatialPoints(coordinates(data.frame(x = 0, y = 0)),
                             CRS(SRS_string = ll_prj))
  origins <- get_elev_raster(locations = origin_sp, z = 6)
  })
  expect_gt(ncell(mans_exp),ncell(mans))
  
  expect_is(origins, "RasterLayer")
})

test_that("loc_check errors correctly", {
  skip_on_cran()
  
  expect_error(get_elev_point(locations = pt_df), 
               "Please supply a valid crs via locations or prj.")
  expect_error(get_elev_point(locations = sp_sm), 
               "Please supply a valid crs via locations or prj.")
  expect_error(get_elev_point(locations = spdf_sm),
               "Please supply a valid crs via locations or prj.")
  expect_error(get_elev_point(locations = rast),
               "Please supply a valid crs via locations or prj.")
  expect_error(get_elev_point(locations = raster(sp_sm), prj = ll_prj),
               "No distinct points, all values NA.")
})

test_that("Z of 1 or 0 works in get_tilexy",{
  skip_on_cran()
  
  suppressWarnings({
  sp_sm_1 <- get_elev_raster(sp_sm_prj, z = 1, clip = "bbox")
  sp_sm_0 <- get_elev_raster(sp_sm_prj, z = 0, clip = "bbox")
  })
  expect_gt(max(res(sp_sm_1)), 0.27)
  expect_gt(max(res(sp_sm_0)), 0.54)
})
