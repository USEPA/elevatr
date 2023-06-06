## ----setup, include=FALSE, echo=FALSE-----------------------------------------
################################################################################
#Load packages
################################################################################
library("raster")
library("knitr")
library("elevatr")
library("httr")
library("sf")
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(purl = NOT_CRAN, 
                      eval = NOT_CRAN,
                      fig.width = 4, 
                      fig.height = 4, 
                      tidy = TRUE,
                      dpi = 100)

## ----environ, echo=FALSE------------------------------------------------------
#key <- readRDS("../tests/testthat/key_file.rds")
#Sys.setenv(mapzen_key=key)

## ----example_dataframe--------------------------------------------------------
# Create an example data.frame
set.seed(65.7)
examp_df <- data.frame(x = runif(3, min = -73, max = -72.5), 
                       y = runif(3, min = 42 , max = 43))
crs_dd <- 4326

# Create and example data.frame with additional columns
cats <- data.frame(category = c("H", "M", "L"))

examp_df2 <- data.frame(examp_df, cats)

# Create an example 
examp_sf <- sf::st_as_sf(examp_df2, coords = c("x", "y"), crs = crs_dd)

## -----------------------------------------------------------------------------
df_elev_epqs <- get_elev_point(examp_df, prj = crs_dd, src = "epqs")
df_elev_epqs
df2_elev_epqs <- get_elev_point(examp_df2, prj = crs_dd, src = "epqs")
df2_elev_epqs
sf_elev_epqs <- get_elev_point(examp_sf, src = "epqs")
sf_elev_epqs

## -----------------------------------------------------------------------------
df_elev_aws <- get_elev_point(examp_df, prj = crs_dd, src = "aws")

## -----------------------------------------------------------------------------
df_elev_aws$elevation
df_elev_epqs$elevation

## -----------------------------------------------------------------------------
df_elev_aws_z12 <- get_elev_point(examp_df, prj = crs_dd, src = "aws", z = 12)
df_elev_aws_z12$elevation
df_elev_epqs$elevation

## -----------------------------------------------------------------------------
mt_everest <- data.frame(x = 86.9250, y = 27.9881)
everest_aws_elev <- get_elev_point(mt_everest, prj = crs_dd, z = 14, src = "aws")
everest_aws_elev

## ----get_raster---------------------------------------------------------------
# sf POLYGON example
data(lake)
elevation <- get_elev_raster(lake, z = 9)
plot(elevation)
plot(lake, add=TRUE)

# data.frame example
elevation_df <- get_elev_raster(examp_df, prj=crs_dd, z = 5)
plot(elevation_df)
plot(examp_sf, add = TRUE, col = "black", pch = 19)

## ----expand-------------------------------------------------------------------
# Bounding box on edge
elev_edge<-get_elev_raster(lake, z = 10)
plot(elev_edge)
plot(lake, add = TRUE)

# Use expand to grab additional tiles
elev_expand<-get_elev_raster(lake, z = 10, expand = 15000)
plot(elev_expand)
plot(lake, add = TRUE)

## ----timeout------------------------------------------------------------------
library(httr)
# Increase timeout:
get_elev_raster(lake, z = 5, config = timeout(5))

