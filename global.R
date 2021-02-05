## Options ---------------------------------------------------------------------

## Quiet proj4 warnings
options("rgdal_show_exportToProj4_warnings" = "none",
        shiny.maxRequestSize = 150 * 1024 ^ 2)

## Libraries -------------------------------------------------------------------

## Spatial packages
library(rgdal)
library(rgeos)
library(raster)
library(sf)
library(mapboxapi)       ## for mapbox baselayers and functions
library(mapdeck)         ## for webgl mapping functions
library(leaflet)         ## for leaflet mapping
library(wopr)            ## interface to WorldPop database
library(spatialsampler)  ## spatial sampling functions

## Shiny packages
library(shiny)
library(shinyFiles)      ## ui for selecting and uploading directories in Shiny
library(colourpicker)
library(shinyjs)

## Utilities and data wrangling
library(magrittr)
library(dplyr)
library(countrycode)     ## conversion of country names <--> other formats
library(stringr)
library(RColorBrewer)

## Reporting
library(kableExtra)
library(openxlsx)

## Authenticate with Mapbox ----------------------------------------------------
mapboxapi::mb_access_token(token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"))

## Mapbox styles
moonlight <- "cj3nban30001z2rpahc10c9ef"
leShine   <- "ckk7gbtu20d9817qu6i8hbxet"
decimal   <- "ckk7gdtbw0d3a17lf2hnuky50"
satellite <- "ckk7geu550d8k17l1t39f1t43"
standard  <- "ckk7geic50d9417juktz7ajet"
northstar <- "ckk7iv17e0fnn17lt6cvi9ppz"
terminal  <- "cj6g0tzbd30kc2sph2wyh666m"

