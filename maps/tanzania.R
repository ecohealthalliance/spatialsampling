## Libraries
library(rgdal)
library(rgeos)
library(raster)

## Read study area
study_area <- readOGR(dsn = "maps/survey_area_tza.gpkg", layer = "survey_area_tza")

## Read new maps data
wards_pop <- readOGR(dsn = "maps/Northern TZ 2012 Wards Shapefiles",
                     layer = "NorthernTZwards_Census2012_edited")
wards_pop <- spTransform(wards_pop, proj4string(study_area))

wards_ls <- readOGR(dsn = "maps/Livestock Data - WardsGIS",
                    layer = "Livestock_data_Wards_NorthTZ")
wards_ls <- spTransform(wards_ls, proj4string(study_area))

## Reduce polygons in pop to those found in ls
wards_pop <- subset(wards_pop, WardCode %in% c(wards_ls$WardCode, "2002101", "2401041", "2002231"))
wards_ls <- subset(wards_ls, WardCode %in% wards_pop$WardCode)

wards_pops <- wards_pop
wards_pops@data <- data.frame(wards_pops@data,
                              wards_ls@data[ , c("Pop_Den", "Cattle", "Goats",
                                                 "Sheep", "Pigs", "Dogs",
                                                 "Cattle_Den", "Goats_Den",
                                                 "Sheep_Den", "Dogs_Den")])

wards_pops <- sp::spTransform(x = wards_pops, CRSobj = crs(proj4string(study_area)))

study_area_pops <- wards_pops[study_area, ]

writeOGR(study_area_pops, "maps/study_area_tza_with_pops.gpkg",
         layer = "study_area_tza_with_pops", driver = "GPKG",
         overwrite_layer = TRUE, delete_dsn = TRUE)

study_area_tza_pops <- readOGR("maps/study_area_tza_pops", "study_area_tza_pops")

writeOGR(study_area_tza_pops, "maps/study_area_tza_pops.gpkg",
         layer = "study_area_tza_pops", driver = "GPKG",
         overwrite_layer = TRUE, delete_dsn = TRUE)




