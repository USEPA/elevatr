context("get_elev_raster")
library(sp)
library(elevatr)
data("pt_df")
data("sp_big")
data("lake")

ll_prj  <- "GEOGCRS[\"unknown\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n            LENGTHUNIT[\"metre\",1]],\n        ID[\"EPSG\",6326]],\n    PRIMEM[\"Greenwich\",0,\n        ANGLEUNIT[\"degree\",0.0174532925199433],\n        ID[\"EPSG\",8901]],\n    CS[ellipsoidal,2],\n        AXIS[\"longitude\",east,\n            ORDER[1],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]],\n        AXIS[\"latitude\",north,\n            ORDER[2],\n            ANGLEUNIT[\"degree\",0.0174532925199433,\n                ID[\"EPSG\",9122]]]]"
aea_prj <- "PROJCRS[\"unknown\",\n    BASEGEOGCRS[\"unknown\",\n        DATUM[\"Unknown based on GRS80 ellipsoid\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1],\n                ID[\"EPSG\",7019]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8901]]],\n    CONVERSION[\"unknown\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",40,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",20,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",60,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"


sp_sm     <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))

test_that("get_elev_raster returns correctly", {
  skip_on_cran()
  
  aws <- get_elev_raster(locations = sp_sm, z = 6, src = "aws")
  aws_prj <- get_elev_raster(locations = sp_sm_prj, z = 6, src = "aws")
  
  #class
  expect_is(aws,"RasterLayer")
  expect_is(aws_prj,"RasterLayer")
  
  #project
  expect_equal(wkt(aws),ll_prj)
  expect_equal(wkt(aws_prj),aea_prj)

})

test_that("get_elev_raster clip argument works", {
  skip_on_cran()
  
  default_clip <- get_elev_raster(lake, z = 5, clip = "tile")
  bbox_clip <- get_elev_raster(lake, z = 5, clip = "bbox")
  locations_clip <- get_elev_raster(lake, z = 5, clip = "locations")
  
  default_values <- raster::getValues(default_clip)
  num_cell_default <- length(default_values[!is.na(default_values)])
  bbox_values <- raster::getValues(bbox_clip)
  num_cell_bbox <- length(bbox_values[!is.na(bbox_values)])
  locations_values <- raster::getValues(locations_clip)
  num_cell_locations <- length(locations_values[!is.na(locations_values)])
  
  expect_true(num_cell_default > num_cell_bbox)
  expect_true(num_cell_bbox > num_cell_locations)
})

test_that("get_elev_raster returns correctly from opentopo", {
  skip_on_cran()
  
  gl1 <- get_elev_raster(locations = sp_sm[3:4,], src = "gl1", neg_to_na = TRUE)
  gl1_prj <- get_elev_raster(locations = sp_sm_prj[3:4,], src = "gl1", 
                             clip = "bbox")
  
  #class
  expect_is(gl1,"RasterLayer")
  expect_is(gl1_prj,"RasterLayer")
  
  #project
  expect_equal(wkt(gl1),ll_prj)
  expect_equal(wkt(gl1_prj),aea_prj)
  
})

test_that("A resp that isn't a tiff or octet-stream works",{
  bad_sp <- SpatialPoints(coordinates(data.frame(x = 1000, y = 1000)),
                             CRS(ll_prj))
  
  expect_error(get_elev_raster(bad_sp, z = 6))
  expect_error(get_elev_raster(bad_sp, src = "gl3"))
})
