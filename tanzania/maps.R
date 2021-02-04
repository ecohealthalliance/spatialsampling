library(rgeos)
library(rgdal)
library(raster)

coords <- matrix(
  c(36.72108, -3.482266,
    37.27464, -2.561640,
    35.30809, -1.230760,
    34.77525, -2.380540),
  ncol = 2, byrow = TRUE)

survey_area_tza <- matrix(
  c(37.13074,-2.77001,
    35.30151,-1.74360,
    34.88171,-2.43222,
    36.73840,-3.48548),
  ncol = 2, byrow = TRUE)

## Get survey area for Tanzania
survey_area_tza <- Polygon(survey_area_tza) %>%
  list() %>%
  Polygons(ID = 1) %>%
  list() %>%
  SpatialPolygons(
    proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  ) %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Read water bodies map
water <- readOGR(dsn = "tanzania/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "tanzania/survey_area_tza.gpkg",
         layer = "survey_area_tza", driver = "GPKG")

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "tanzania/survey_area",
         layer = "survey_area", driver = "ESRI Shapefile")



coords_window = matrix(c(34.000000,-3.87000,
                         37.87000, -3.87000,
                         37.87000, -1.15000,
                         34.000000, -1.15000),
                       ncol = 2, byrow = TRUE)
