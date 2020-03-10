#Rasterizing Polygons

library(rgdal)
library(sp)
library(sf)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)


####SETTING UP RASTER FILE####

# read in the ocean

oceans <- readOGR(dsn = "./shapefiles/ocean_raster", layer = "ne_10m_ocean")

# create a global raster layer

oceans <- spTransform(oceans, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))
oceans_raster <- raster(oceans)

#Setting raster resolution
# res in units of decimal degrees ----- 0.1 decimal degrees ~ 11.132 km

res(oceans_raster) <- .2

#crop extent of the oceans raster

extent_oc <- extent(-85,-75,25,40)
oceans_raster <- crop(oceans_raster, extent_oc)

# saving the world raster grid

#save(oceans_raster, file = './data/raster/oceans_raster.Rdata')
#load('./Data/raster/oceans_raster.Rdata')


# making continents polygon  

continents <- shapefile('./shapefiles/continent/continent/continent.shp')
continents <- spTransform(continents, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))





#### Rasterizing SEAMAP-SA Data ####
    # data frame created in data_processing_Caughron

#read in s_spread
s_spread <- read.csv("~./fish_stability/data/s_spread.csv", header = T)


#making coordinates numeric
s_spread$lat <- as.numeric(s_spread$lat)
s_spread$long <- as.numeric(s_spread$long)
s_spread$EVENTNAME <- as.character(s_spread$EVENTNAME)
s_spread$S <- as.numeric(s_spread$S)

#adding identity column called trawl number
s_spread$TRAWLNUMBER <- 1

#setting lat and long columns and projection
coordinates(s_spread) <- ~ long + lat


#Creating Trawl Density Raster 
Trawl_raster <- rasterize(s_spread, oceans_raster, s_spread$TRAWLNUMBER, fun = "sum")
res(Trawl_raster)
plot(Trawl_raster)

Trawl_raster@data@values <- Trawl_raster@data@values[!is.na(Trawl_raster@data@values)]
hist(Trawl_raster@data@values, breaks =25)
summary(Trawl_raster@data@values)
length(Trawl_raster@data@values)

#Creating Species Richness Raster
SpeciesRich_raster <- rasterize(s_spread, oceans_raster, s_spread$S, 
                                fun = function(x,...)mean(x))
res(SpeciesRich_raster)
plot(SpeciesRich_raster)

#Creating Biomass Variance Raster
BiomassVar_raster <- rasterize(s_spread, oceans_raster, s_spread$biomass,
                               fun = function(x,...)var(x))
res(BiomassVar_raster)
plot(BiomassVar_raster)

#Creating Identity Raster

vals <- 1:ncell(oceans_raster)
oceans_raster <- setValues(oceans_raster, vals)
rd <- data.frame(oceans_raster@data@values)
plot(oceans_raster)


#pulling cell IDs for each of the trawls for 0.2 res raster

coord_trawls <- data.frame(cbind(s_spread$long, s_spread$lat))
coord_trawls <- SpatialPoints(coord_trawls,
                              proj4string = CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))
coordinates(coord_trawls) <- ~ X1 + X2
proj4string(coord_trawls) <- "+proj=longlat +lat_0=32.4 +lon_0=-79.6 + ellps=WGS84"

raster_values0.2 <-raster::extract(oceans_raster, coord_trawls, df = T)


#write.csv(raster_values0.2, "~./fish_stability/data/raster_values0.2.csv")
# repeat process for multiple resolutions
#join cellID vectors for each resolution in data frame called cellIDs
#join cellIDs data frame with s_spread
#s_spread should be ready for analysis




#### trying to create raster map with only 3 yr bin subset regions #####

setofID <- c(1496, 1554, 1610, 1786, 1842, 1844, 1846, 1902, 1906, 1960, 2020,
             2080, 2137, 2138, 2196, 2255, 2313, 2314, 2372, 2373, 2431, 2432,
             2550, 2609, 2610, 2669, 2729, 2788, 2789, 2909, 3029, 3089, 3150,
             3210, 3271, 3331)

oc_raster <- oceans_raster


plot(oc_raster)
plot(oceans_raster)


#just 36 rasters that exist over all time bins 
timebin_raster <- oceans_raster

narep <- rep(NA, 6000)
narep <- replace(narep, setofID, setofID)
summary(narep)
timebin_raster <- setValues(timebin_raster,narep)
plot(timebin_raster)



allrasters <- rastervals$layer
narep <- rep(NA, 6000)
narep <- replace(narep, allrasters, allrasters)
summary(allrasters)
all_raster <- oceans_raster
plot(all_raster)
all_raster <- setValues(all_raster, narep)
plot(all_raster)



