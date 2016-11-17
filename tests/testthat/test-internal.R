context("internals not yet caught")
data("pt_df")
data("sp_big")
library(sp)
library(raster)

ll_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
aea_prj <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

sp_sm <- SpatialPoints(coordinates(pt_df),CRS(ll_prj))
sp_sm_prj <- spTransform(sp_sm,CRS(aea_prj))


test_that("proj_expand works",{
  mans_sp <- SpatialPoints(coordinates(data.frame(x = -72.8145, y = 44.5438)),
                           CRS(ll_prj))
  mans <- get_elev_raster(locations =  mans_sp, z = 6)
  mans_exp <- get_elev_raster(locations = mans_sp, z = 6, expand = 2)
  
  expect_gt(ncell(mans_exp),ncell(mans))
  
})
