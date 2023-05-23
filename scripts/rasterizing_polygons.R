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

oceans <- readOGR(dsn = "./gitdat/shapefiles/ocean_raster", layer = "ne_10m_ocean")

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

#save(oceans_raster, file = './gitdat/raster/oceans_raster.Rdata')
#load('./gitdat/raster/oceans_raster.Rdata')


# making continents polygon  

continents <- shapefile('./gitdat/shapefiles/continent/continent/continent.shp')
continents <- spTransform(continents, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))





#### Rasterizing SEAMAP-SA Data ####
# data frame created in ./scripts/data_processing.R

#read in s_spread
s_spread <- read.csv("./gitdat/s_spread.csv")

#changing eventname and S to character and factor variables
s_spread$EVENTNAME <- as.character(s_spread$EVENTNAME)
s_spread$S <- as.numeric(s_spread$S)

#adding identity column called trawl number
s_spread$TRAWLNUMBER <- 1

#setting lat and long columns as coordinates
s_spread$lat <- as.numeric(s_spread$lat)
s_spread$long <- as.numeric(s_spread$long)
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
raster_vals <-raster::extract(oceans_raster, coord_trawls, df = T)


write.csv(raster_vals, "./gitdat/raster_vals.csv")





#### raster map with only 3 yr bin subset regions #####

goodID <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
             1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
             2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
             2670, 2721, 2771)



plot(oceans_raster)


#just 36 rasters that exist over all time bins 
goodID_raster <- oceans_raster

narep <- rep(NA, 3750)
narep <- replace(narep, goodID, goodID)
summary(narep)
goodID_raster <- setValues(goodID_raster,narep)
plot(goodID_raster)

#all rasters
all_raster <- oceans_raster
narep <- rep(NA, 3750)
narep <- replace(narep, na.omit(raster_vals$layer), na.omit(raster_vals$layer))
all_raster <- setValues(all_raster, narep)
plot(all_raster)


