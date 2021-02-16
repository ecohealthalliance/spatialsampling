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
library(leaflet)         ## for leaflet mapping

## spatial sampling functions
if (!require(remotes)) install.packages("remotes")
if (!require(spatialsampler)) remotes::install_github("spatialworks/spatialsampler")

## Shiny packages
library(shiny)
library(shinyFiles)
library(colourpicker)
library(shinyjs)

## Utilities and data wrangling
library(magrittr)
library(dplyr)
library(countrycode)     ## conversion of country names <--> other formats
library(stringr)
library(RColorBrewer)
library(RCurl)
library(curl)
library(jsonlite)
#library(dataverse)

if (!require(dataverse)) remotes::install_github("iqss/dataverse-client-r")

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

## Functions

list_wp_pop_files <- function(country,
                              base = "ftp://ftp.worldpop.org.uk/GIS/Population/Individual_countries/") {
  ## iso3
  iso3c <- countrycode::countrycode(
    sourcevar = country,
    origin = "country.name",
    destination = "iso3c",
    warn = FALSE)

  ## UN name
  un.name <- countrycode::countrycode(
    sourcevar = country,
    origin = "country.name",
    destination = "un.name.en",
    warn = FALSE
  )

  ## Create the full URL
  ftp_folder <- paste(
    base, iso3c, "/",
    un.name %>% stringr::str_replace_all(pattern = " ", replacement = "_"),
    "_100m_Population/", sep = ""
  )

  ##
  fn <- RCurl::getURL(ftp_folder, ftp.use.epsv = FALSE, dirlistonly = TRUE) %>%
    stringr::str_split(pattern = "\n") %>%
    unlist()

  ##
  fn <- fn[stringr::str_detect(string = fn, pattern = "\\.tif")]

  ##
  return(fn)
}

get_wp_pop_data <- function(fn,
                            base = "ftp://ftp.worldpop.org.uk/GIS/Population/Individual_countries/") {
  ## iso3
  iso3c <- stringr::str_extract(fn, pattern = "[A-Z]{3}")

  ## UN name
  un.name <- countrycode::countrycode(
    sourcevar = iso3c,
    origin = "iso3c",
    destination = "un.name.en"
  )

  ## Create the full URL
  ftp_folder <- paste(
    base, iso3c, "/",
    un.name %>% stringr::str_replace_all(pattern = " ", replacement = "_"),
    "_100m_Population/", sep = ""
  )

  ## Add filename
  file_path <- paste0(ftp_folder, fn)

  ## Create temp file
  tmp_file <- tempfile()

  ## Download raster
  curl::curl_download(url = file_path, destfile = tmp_file)

  ## Read raster
  pop_raster <- raster::raster(x = tmp_file)

  ## Return raster
  return(pop_raster)
}



list_wpgp_pop_files <- function(ccode,
                                base = "https://www.worldpop.org/rest/data") {
  ## create query
  query <- paste0(base, "/pop/wpgp?iso3=", ccode)

  ## Get JSON
  jsonlite::fromJSON(query)[[1]] %>%
    tibble::tibble()
}


get_wpgp_pop_data <- function(ccode, year) {
  ##
  data_list <- list_wpgp_pop_files(ccode = ccode)

  ##
  file_url <- data_list %>%
    dplyr::filter(popyear == year) %>%
    dplyr::select(files) %>%
    unlist()

  ##
  tmp_file <- tempfile()

  ##
  curl::curl_download(url = file_url, destfile = tmp_file)

  ##
  pop_raster <- raster::raster(x = tmp_file)

  ##
  pop_raster
}

