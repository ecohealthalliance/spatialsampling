## Libraries -------------------------------------------------------------------

## Spatial packages
library(rgdal)
library(rgeos)
library(raster)
library(sf)
library(mapboxapi)
library(mapdeck)
library(leaflet)
library(spatialsampler)

## Shiny packages
library(shiny)
library(shinydashboard)
library(shinyFiles)

## Utilities and data wrangling
library(magrittr)
library(dplyr)

## Authenticate with Mapbox ----------------------------------------------------
mapboxapi::mb_access_token(token = Sys.getenv("MAPBOX_PUBLIC_TOKEN"),
                           install = TRUE)

## Mapbox styles
leShine   <- "ckk7gbtu20d9817qu6i8hbxet"
decimal   <- "ckk7gdtbw0d3a17lf2hnuky50"
satellite <- "ckk7geu550d8k17l1t39f1t43"
standard  <- "ckk7geic50d9417juktz7ajet"
northstar <- "ckk7iv17e0fnn17lt6cvi9ppz"
