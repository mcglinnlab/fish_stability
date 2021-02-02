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


#changing eventname and S to character and factor variables
s_spread$EVENTNAME <- as.character(s_spread$EVENTNAME)
s_spread$S <- as.numeric(s_spread$S)

#fixing two incorrect coordinates


#adding identity column called trawl number
s_spread$TRAWLNUMBER <- 1

#setting lat and long columns as coordinates
coordinates(s_spread) <- ~ long + lat


#Creating Trawl Density Raster 
Trawl_raster <- rasterize(s_spread, oceans_raster, s_spread$TRAWLNUMBER, fun = "sum")
res(Trawl_raster)
plot(Trawl_raster)


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
names(coord_trawls) <- c("long", "lat")
coord_trawls <- SpatialPoints(coord_trawls,
                              proj4string = CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6 + ellps=WGS84"))
#coordinates(coord_trawls) <- ~ long + lat
#proj4string(coord_trawls) <- "+proj=longlat +lat_0=32.4 +lon_0=-79.6 + ellps=WGS84"

raster_values0.2 <-raster::extract(oceans_raster, coord_trawls, df = T)


#write.csv(raster_values0.2, "~./fish_stability/data/raster_values0.2.csv")
# repeat process for multiple resolutions
#join cellID vectors for each resolution in data frame called cellIDs
#join cellIDs data frame with s_spread
#s_spread should be ready for analysis




#### trying to create raster map with only 3 yr bin subset regions #####

setofID <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
             1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
             2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
             2670, 2721, 2771)

oc_raster <- oceans_raster


plot(oc_raster)
plot(oceans_raster)


#just 36 rasters that exist over all time bins 
timebin_raster <- oceans_raster

narep <- rep(NA, 3750)
narep <- replace(narep, setofID, setofID)
summary(narep)
timebin_raster <- setValues(timebin_raster,narep)
plot(timebin_raster)

#all rasters
all_raster <- oceans_raster
narep <- rep(NA, 3750)
narep <- replace(narep, raster_values0.2$layer, raster_values0.2$layer)
all_raster <- setValues(all_raster, narep)
plot(all_raster)


