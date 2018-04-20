## ----setup, include=FALSE, echo=FALSE------------------------------------
################################################################################
#Load packages
################################################################################
library("sp")
library("raster")
library("knitr")
library("elevatr")
library("httr")
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(purl = NOT_CRAN, 
                      eval = NOT_CRAN,
                      fig.width = 5, 
                      fig.height = 5, 
                      tidy = TRUE)

## ----environ, echo=FALSE-------------------------------------------------
#key <- readRDS("../tests/testthat/key_file.rds")
#Sys.setenv(mapzen_key=key)

## ----example_dataframe---------------------------------------------------
# Create an example data.frame
set.seed(65.7)
examp_df <- data.frame(x = runif(10, min = -73, max = -71), 
                       y = runif(10, min = 41 , max = 45))
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Create and example data.frame with additional columns
cats <- data.frame(category = c("H", "H", "L", "L", "L", 
                                "M", "H", "L", "M", "M"))

examp_df2 <- data.frame(examp_df, cats)

# Create an example SpatialPoints
examp_sp <- SpatialPoints(examp_df, proj4string = CRS(prj_dd))

# Create an example SpatialPointsDataFrame
examp_spdf <- SpatialPointsDataFrame(examp_sp, proj4string = CRS(prj_dd), data = cats )

## ------------------------------------------------------------------------
df_elev_epqs <- get_elev_point(examp_df, prj = prj_dd, src = "epqs")
data.frame(df_elev_epqs)
df2_elev_epqs <- get_elev_point(examp_df2, prj = prj_dd, src = "epqs")
data.frame(df2_elev_epqs)
sp_elev_epqs <- get_elev_point(examp_sp, src = "epqs")
sp_elev_epqs
spdf_elev_epqs <- get_elev_point(examp_spdf, src = "epqs")
spdf_elev_epqs

## ----get_raster----------------------------------------------------------
# SpatialPolygonsDataFrame example
data(lake)
elevation <- get_elev_raster(lake,z = 9)
plot(elevation)
plot(lake, add=TRUE)

# data.frame example
elevation_df <- get_elev_raster(examp_df,prj=prj_dd, z = 5)
plot(elevation_df)
plot(examp_sp, add = TRUE)

## ----expand--------------------------------------------------------------
# Bounding box on edge
elev_edge<-get_elev_raster(lake, z = 10)
plot(elev_edge)
plot(lake, add = TRUE)

# Use expand to grab additional tiles
elev_expand<-get_elev_raster(lake, z = 10, expand = 1500)
plot(elev_expand)
plot(lake, add = TRUE)

## ----timeout-------------------------------------------------------------
# Increase timeout:
get_elev_raster(lake, z = 5, config = timeout(5))

## ----timeout_verbose-----------------------------------------------------
# Increase timeout:
get_elev_raster(lake, z = 5, config = c(verbose(),timeout(5)))

