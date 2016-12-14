## ----setup, include=FALSE, echo=FALSE------------------------------------
################################################################################
#Load packages
################################################################################
library("sp")
library("raster")
library("rgdal")
library("knitr")
library("elevatr")

## ----api_key, eval=FALSE-------------------------------------------------
#  cat("mapzen_key=mapzen-XXXXXXX\n",
#      file=file.path(normalizePath("~/"), ".Renviron"),
#      append=TRUE)

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

# Create and example SpatialPointsDataFrame

examp_spdf <- SpatialPointsDataFrame(examp_sp, proj4string = CRS(prj_dd), data = cats )

## ----examples_df1--------------------------------------------------------
# Example using data.frame with longitude and latitude
df_elev <- get_elev_point(examp_df, prj = prj_dd, src = "mapzen")

# Compare
examp_df
df_elev

## ----examples_df2--------------------------------------------------------
# Example using data.frame with longitud, latitude and an additional column
df2_elev <- get_elev_point(examp_df2, prj = prj_dd, src = "mapzen")

# Compare
examp_df2
df2_elev

## ----sleep1, echo=F------------------------------------------------------
Sys.sleep(1)

## ----examples_sp, eval=T-------------------------------------------------
# Example using SpatialPoints
# prj is taken from the SpatialPoints object
# api_key is taken from environment variable mapzen_key
sp_elev <- get_elev_point(examp_sp)

# Compare
examp_sp
sp_elev

## ----examples_sp2, eval=T------------------------------------------------
# Example using SpatialPoints
# prj is taken from the SpatialPoints object
# api_key is taken from environment variable mapzen_key

spdf_elev <- get_elev_point(examp_spdf)

# Compare
examp_spdf
spdf_elev

