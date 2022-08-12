library(rgeos)
library(rgdal)
library(raster)
library(magrittr)
library(tanzania)

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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area",
         layer = "survey_area", driver = "ESRI Shapefile", overwrite_layer = TRUE)


## Villages map
#villages_tza <- readOGR(dsn = "maps/Tanzania_Village_EA_2002_region",
#                        layer = "Tanzania_Village_EA_2002_region")

#writeOGR(obj = villages_tza, dsn = "maps/villages_tza.gpkg",
#         layer = "villages_tza", driver = "GPKG", overwrite_layer = TRUE)


## Map of Bangladesh area
survey_area_bgd <- raster::getData(country = "BGD",
                                   level = 3,
                                   path = "www/maps") %>%
  subset(NAME_2 == "Dhaka" & !NAME_3 %in% c("Dhamrai", "Nawabganj", "Dohar",
                                            "Savar", "Keraniganj"))

writeOGR(survey_area_bgd, dsn = "maps/survey_area_bgd.gpkg",
         layer = "survey_area_bgd", driver = "GPKG", overwrite_layer = TRUE)

## Map of liberia
survey_area_lbr <- raster::getData(country = "LBR",
                                   level = 3,
                                   path = "www/maps") %>%
  subset(NAME_2 == "Greater Monrovia")

writeOGR(survey_area_lbr, dsn = "maps/survey_area_lbr.gpkg",
         layer = "survey_area_lbr", driver = "GPKG", overwrite_layer = TRUE)

## Map of Philippines
survey_area_phl <- raster::getData(country = "phl",
                                   level = 3,
                                   path = "www/maps") %>%
  subset(NAME_2 == "Davao City")

writeOGR(survey_area_phl, dsn = "maps/survey_area_phl.gpkg",
         layer = "survey_area_phl", driver = "GPKG", overwrite_layer = TRUE)

## Map of South Africa
survey_area_zaf <- raster::getData(country = "zaf",
                                   level = 3,
                                   path = "www/maps") %>%
  subset(NAME_2 == "City of Cape Town")

writeOGR(survey_area_zaf, dsn = "maps/survey_area_zaf.gpkg",
         layer = "survey_area_zaf", driver = "GPKG", overwrite_layer = TRUE)

## Map of Jordan
survey_area_jor <- raster::getData(country = "jor",
                                   level = 2,
                                   path = "www/maps") %>%
  subset(NAME_2 == "Amman")

writeOGR(survey_area_jor, dsn = "maps/survey_area_jor.gpkg",
         layer = "survey_area_jor", driver = "GPKG", overwrite_layer = TRUE)


####################### NEW STUDY AREA - TANZANIA ##############################

survey_area_tza <- matrix(
  c(34.34645241311772,-3.050654247433482,
    36.46132057718022,-4.179975646524286,
    36.91176003030522,-3.24262503132466,
    34.8463303428052,-2.1122869673476683),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load districts map
districts <- tanzania::district
manyara <- subset(districts, NewDist20 == "Simanjiro DC")

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update1.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update1",
         layer = "survey_area_tza_update1", driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################### NEW STUDY AREA 2 - TANZANIA ############################

survey_area_tza <- matrix(
  c(36.1879265261047,-4.040355036555776,
    36.5889275026672,-3.300304122016921,
    34.7651970339172,-2.279800313216861,
    34.353209729229,-3.048006439706601),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load regions map
regions <- tanzania::region
manyara <- subset(region, Region_Nam == "Manyara")

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update2.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update2",
         layer = "survey_area_tza_update2", driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################### NEW STUDY AREA 2 - TANZANIA ############################

survey_area_tza <- matrix(
  c(36.1879265261047,-4.040355036555776,
    36.5889275026672,-3.300304122016921,
    34.8860466432922,-2.345664584993989,
    34.45757984641651,-3.1028589638335236),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load regions map
regions <- tanzania::region
manyara <- subset(region, Region_Nam == "Manyara")

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update3.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update3",
         layer = "survey_area_tza_update3", driver = "ESRI Shapefile", overwrite_layer = TRUE)

####################### NEW STUDY AREA 4 - TANZANIA ############################

survey_area_tza <- matrix(
  c(36.1879265261047,-4.040355036555776,
    36.5889275026672,-3.300304122016921,
    34.8860466432922,-2.345664584993989,
    34.45757984641651,-3.1028589638335236),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load regions map
regions <- tanzania::region
manyara <- subset(region, Region_Nam == "Manyara")

## Load protected areas map
protect <- readOGR(dsn = "maps/protectedareashptanzania",
                   layer = "ProtectedAreasTZ")

protect <- sp::spTransform(protect,
                           CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

serengeti <- subset(protect, PTANAME == "SERENGETI NATIONAL PARK")
maswa <- subset(protect, PTANAME == "MASWA GAME RESERVE")

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)
survey_area_tza <- rgeos::gDifference(survey_area_tza, maswa)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update4.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update4",
         layer = "survey_area_tza_update4", driver = "ESRI Shapefile", overwrite_layer = TRUE)


create_sp_grid(survey_area_tza, country = "Tanzania", n = 152, type = "csas")


####################### NEW STUDY AREA 5 - TANZANIA ############################

survey_area_tza <- matrix(
  c(36.1879265261047,-4.040355036555776,
    36.5889275026672,-3.300304122016921,
    34.8860466432922,-2.345664584993989,
    34.45757984641651,-3.1028589638335236),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load regions map
regions <- tanzania::region
manyara <- subset(region, Region_Nam == "Manyara")

## Load protected areas map
protect <- readOGR(dsn = "maps/protectedareashptanzania",
                   layer = "ProtectedAreasTZ")

protect <- sp::spTransform(protect,
                           CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

serengeti <- subset(protect, PTANAME == "SERENGETI NATIONAL PARK")
maswa <- subset(protect, PTANAME == "MASWA GAME RESERVE")
ngorongoro <- subset(protect, PTANAME == "NGORONGORO CONSERVATION AREA")

conservation2 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_2",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation1 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_1",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation0 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_0",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation <- sp::spTransform(conservation,
                                CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

ngorongoro_crater <- SpatialPoints(coords = matrix(c(35.58767, -3.161752), nrow = 1, ncol = 2, byrow = TRUE),
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
ngorongoro_crater <- rgeos::gBuffer(spgeom = ngorongoro_crater, width = 0.1)

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)
survey_area_tza <- rgeos::gDifference(survey_area_tza, maswa)
survey_area_tza <- rgeos::gDifference(survey_area_tza, ngorongoro_crater)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update5.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update5",
         layer = "survey_area_tza_update5", driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################### NEW STUDY AREA 6 - TANZANIA ############################

survey_area_tza <- matrix(
  c(34.50575417093022,-2.8421900753114095,
    36.36793678811772,-3.8950403326204035,
    36.78541725686772,-3.143901614694799,
    34.9452072959302,-2.0793500308320914),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load regions map
regions <- tanzania::region
manyara <- subset(region, Region_Nam == "Manyara")

## Load protected areas map
protect <- readOGR(dsn = "maps/protectedareashptanzania",
                   layer = "ProtectedAreasTZ")

protect <- sp::spTransform(protect,
                           CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

serengeti <- subset(protect, PTANAME == "SERENGETI NATIONAL PARK")
maswa <- subset(protect, PTANAME == "MASWA GAME RESERVE")
ngorongoro <- subset(protect, PTANAME == "NGORONGORO CONSERVATION AREA")

conservation2 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_2",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation1 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_1",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation0 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_0",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation <- sp::spTransform(conservation,
                                CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

ngorongoro_crater <- SpatialPoints(coords = matrix(c(35.58767, -3.161752), nrow = 1, ncol = 2, byrow = TRUE),
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
ngorongoro_crater <- rgeos::gBuffer(spgeom = ngorongoro_crater, width = 0.1)

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)
survey_area_tza <- rgeos::gDifference(survey_area_tza, maswa)
survey_area_tza <- rgeos::gDifference(survey_area_tza, ngorongoro_crater)
#survey_area_tza <- rgeos::gDifference(survey_area_tza, serengeti)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update6.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update6",
         layer = "survey_area_tza_update6", driver = "ESRI Shapefile", overwrite_layer = TRUE)


####################### NEW STUDY AREA 7 - TANZANIA ############################

survey_area_tza <- matrix(
  c(34.56068581155522,-2.8641354937126073,
    36.36793678811772,-3.8950403326204035,
    36.78541725686772,-3.143901614694799,
    35.0056321006177,-2.1122869673476683),
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
water <- readOGR(dsn = "maps/tza_water_areas_dcw", layer = "tza_water_areas_dcw")

## Load regions map
regions <- tanzania::region
manyara <- subset(region, Region_Nam == "Manyara")

## Load protected areas map
protect <- readOGR(dsn = "maps/protectedareashptanzania",
                   layer = "ProtectedAreasTZ")

protect <- sp::spTransform(protect,
                           CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

serengeti <- subset(protect, PTANAME == "SERENGETI NATIONAL PARK")
maswa <- subset(protect, PTANAME == "MASWA GAME RESERVE")
ngorongoro <- subset(protect, PTANAME == "NGORONGORO CONSERVATION AREA")

conservation2 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_2",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation1 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_1",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation0 <- readOGR(dsn = "maps/WDPA_WDOECM_May2021_Public_TZA_shp/WDPA_WDOECM_May2021_Public_TZA_shp_0",
                         layer = "WDPA_WDOECM_May2021_Public_TZA_shp-polygons")
conservation <- sp::spTransform(conservation,
                                CRSobj = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

ngorongoro_crater <- SpatialPoints(coords = matrix(c(35.58767, -3.161752), nrow = 1, ncol = 2, byrow = TRUE),
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
ngorongoro_crater <- rgeos::gBuffer(spgeom = ngorongoro_crater, width = 0.1)

##
survey_area_tza <- rgeos::gDifference(survey_area_tza, water)
survey_area_tza <- rgeos::gDifference(survey_area_tza, manyara)
survey_area_tza <- rgeos::gDifference(survey_area_tza, maswa)
survey_area_tza <- rgeos::gDifference(survey_area_tza, ngorongoro_crater)
#survey_area_tza <- rgeos::gDifference(survey_area_tza, serengeti)

survey_area_tza <- survey_area_tza %>%
  SpatialPolygonsDataFrame(data = data.frame(1))

## Save as geopackage
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update7.gpkg",
         layer = "survey_area_tza", driver = "GPKG", overwrite_layer = TRUE)

## Save as shapefile
writeOGR(obj = survey_area_tza, dsn = "maps/survey_area_tza_update7",
         layer = "survey_area_tza_update7", driver = "ESRI Shapefile", overwrite_layer = TRUE)



