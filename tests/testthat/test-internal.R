context("internals not yet caught")
library(sf)
library(raster)
library(elevatr)
data("pt_df")
data("sf_big")
data("lake")
skip_on_cran()
#skip_on_os("solaris")
ll_prj  <- 4326
aea_prj <- 5072

sf_sm <- st_as_sf(pt_df, coords = c("x", "y"), crs = ll_prj)
sf_sm_prj <- st_transform(sf_sm, crs = aea_prj) 
bad_sf <- st_as_sf(data.frame(x = 1000, y = 1000), coords = c("x", "y"), 
                   crs = ll_prj)
rast <- rasterize(st_coordinates(sf_sm),raster(sf_sm))
sf_sm_na <- sf_sm
st_crs(sf_sm_na) <- NA
rast_na <- rast
crs(rast_na) <- NA

mt_wash <- data.frame(x = -71.3036, y = 44.2700)
mt_mans <- data.frame(x = -72.8145, y = 44.5438)
mts <- rbind(mt_wash,mt_mans)
mts$name <- c("Mount Washington", "Mount Mansfield")

test_that("data frame with more extra columns work", {
  
  mts_with_names_and_elevation <- get_elev_point(mts, ll_prj)
  expect_true("name" %in% names(mts_with_names_and_elevation))
})

test_that("proj_expand works",{
  
  suppressWarnings({
  mans_sf <- st_as_sf(data.frame(x = -72.8145, y = 44.5438), 
                      coords = c("x","y"),
                      crs = ll_prj)
  mans <- get_elev_raster(locations =  mans_sf, z = 6)
  mans_exp <- get_elev_raster(locations = mans_sf, z = 6, expand = 2)
  
  origin_sf <- st_as_sf(data.frame(x = 0, y = 0), coords = c("x", "y"),
                        crs = ll_prj)
  origins <- get_elev_raster(locations = origin_sf, z = 6)
  })
  expect_gt(ncell(mans_exp),ncell(mans))
  
  expect_is(origins, "RasterLayer")
})

test_that("loc_check errors correctly", {
  
  expect_error(get_elev_point(locations = pt_df), 
               "Please supply a valid sf crs via locations or prj.")
  expect_error(get_elev_point(locations = rast_na),
               "Please supply a valid sf crs via locations or prj.")
  expect_error(get_elev_point(locations = sf_sm_na),
               "Please supply an sf object with a valid crs.")
  expect_error(get_elev_point(locations = raster(), prj = ll_prj),
               "No distinct points, all values NA.")
})

test_that("Z of 1 or 0 works in get_tilexy",{
  
  suppressWarnings({
  sf_sm_1 <- get_elev_raster(sf_sm_prj, z = 1, clip = "bbox")
  sf_sm_0 <- get_elev_raster(sf_sm_prj, z = 0, clip = "bbox")
  })
  expect_gt(max(res(sf_sm_1)), 0.27)
  expect_gt(max(res(sf_sm_0)), 0.54)
})
