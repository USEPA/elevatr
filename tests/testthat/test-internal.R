context("internals not yet caught")
library(sp)
library(raster)
library(rgdal)
library(elevatr)
data("pt_df")
data("sp_big")
data("lake")

ll_prj  <- "+proj=longlat +datum=WGS84 +no_defs"
aea_prj <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))

proj4string(sp_sm) <- ""
spdf_sm <- SpatialPointsDataFrame(sp_sm, data.frame(1:nrow(coordinates(sp_sm))))

rast <- rasterize(coordinates(spdf_sm),raster(spdf_sm))

mt_wash <- data.frame(x = -71.3036, y = 44.2700)
mt_mans <- data.frame(x = -72.8145, y = 44.5438)
mts <- rbind(mt_wash,mt_mans)
mts$name <- c("Mount Washington", "Mount Mansfield")

test_that("data frame with more extra columns work", {
  skip_on_cran()
  skip_on_appveyor()
  
  mts_with_names_and_elevation <- get_elev_point(mts, ll_prj)
  expect_true("name" %in% names(mts_with_names_and_elevation))
})

test_that("proj_expand works",{
  skip_on_cran()
  skip_on_appveyor()
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
  skip_on_appveyor()
  expect_error(get_elev_point(locations = pt_df), 
               "Please supply a valid proj.4 string.")
  expect_error(get_elev_point(locations = sp_sm), 
               "Please supply a valid proj.4 string.")
  expect_error(get_elev_point(locations = spdf_sm),
               "Please supply a valid proj.4 string.")
  expect_error(get_elev_point(locations = rast),
               "Please supply a valid proj.4 string.")
  expect_error(get_elev_point(locations = raster(sp_sm), prj = ll_prj),
               "No distinct points, all values NA.")
})

test_that("loc_check assigns prj correctly",{
  skip_on_cran()
  skip_on_appveyor()
  expect_equal(proj4string(get_elev_point(locations = sp_sm, prj = ll_prj)),
                           ll_prj)
  expect_equal(proj4string(get_elev_point(locations = spdf_sm, prj = ll_prj)),
                           ll_prj)
  expect_equal(proj4string(get_elev_point(locations = rast, prj = ll_prj)), 
               ll_prj)
})

test_that("Z of 1 or 0 works in get_tilexy",{
  skip_on_cran()
  skip_on_appveyor()
  
  sp_sm_1 <- get_elev_raster(sp_sm_prj, z = 1, clip = "bbox")
  sp_sm_0 <- get_elev_raster(sp_sm_prj, z = 0, clip = "bbox")
  
  expect_gt(max(res(sp_sm_1)), 0.27)
  expect_gt(max(res(sp_sm_0)), 0.54)
})
