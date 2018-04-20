## ----setup, include=FALSE, echo=FALSE------------------------------------
################################################################################
#Load packages
################################################################################
library("sp")
library("raster")
library("rgdal")
library("knitr")
library("elevatr")
library("httr")
library("prettyunits")
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(purl = NOT_CRAN, 
                      eval = NOT_CRAN,
                      fig.width = 5, 
                      fig.height = 5, 
                      tidy = TRUE)

