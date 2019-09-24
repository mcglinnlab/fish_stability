#Rasterizing Polygons

library(rgdal)
library(sp)
library(sf)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)


##SETTING UP RASTER FILE##

# read in the ocean

oceans <- readOGR(dsn = "./shapefiles/ocean_raster", layer = "ne_10m_ocean")

# create a global raster layer

oceans <- spTransform(oceans, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))
oceans_raster <- raster(oceans)

#Setting raster resolution
# res in units of decimal degrees ----- 0.1 decimal degrees ~ 11.132 km

res(oceans_raster) <- .2

#crop extent of the oceans raster

extent <- extent(-85,-75,25,40)
oceans_raster <- crop(oceans_raster, extent)

# saving the world raster grid

#save(oceans_raster, file = './data/raster/oceans_raster.Rdata')
#load('./Data/raster/oceans_raster.Rdata')


# making continents polygon  

continents <- shapefile('./shapefiles/continent/continent/continent.shp')
continents <- spTransform(continents, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))








## Rasterizing SEAMAP-SA Data ##


# reading in SEAMAP sub data
SEAMAP_sub <- read.csv('./data/SEAMAP_sub.csv')



########### need to rework rasterize now that data frame is changed ############

#removing repeated collection number rows-- essentially removing each row is species. Invidiual species no longer important
#SEAMAP_nonrepeat <- SEAMAP_invest[!duplicated(SEAMAP_invest$EVENTNAME),]


#making coordinates numeric
s_spread$lat <- as.numeric(as.character(s_spread$lat))
s_spread$long <- as.numeric(as.character(s_spread$long))
s_spread$EVENTNAME <- as.numeric(as.character(s_spread$EVENTNAME))
s_spread$S <- as.numeric(as.character(s_spread$S))

#adding identity column called trawl number
s_spread$TRAWLNUMBER <- 1

#setting lat and long columns and projection
coordinates(s_spread) <- ~ long + lat
proj4string(s_spread) <- "+proj=longlat +lat_0=32.4 +lon_0=-79.6"


#Creating Trawl Density Raster
Trawl_raster <- rasterize(s_spread, oceans_raster, s_spread$TRAWLNUMBER, fun = "sum")
res(Trawl_raster)
plot(Trawl_raster)

#Creating Species Richness Raster
SpeciesRich_raster <- rasterize(s_spread, oceans_raster, s_spread$S, fun=function(x,...)mean(x))
res(SpeciesRich_raster)
plot(SpeciesRich_raster)

#Creating Biomass Variance Raster
BiomassVar_raster <- rasterize(s_spread, oceans_raster, s_spread$biomass, fun =function(x,...)var(x))
res(BiomassVar_raster)
plot(BiomassVar_raster)

#Creating Identity Raster

vals <- 1:ncell(oceans_raster)
oceans_raster <- setValues(oceans_raster, vals)
rd <- data.frame(oceans_raster@data@values)
plot(oceans_raster)
res(oceans_raster)


## can make example work but can't make it work for my data. Only NAs are being returned. 

coord_trawls <- data.frame(cbind(s_spread$lat, s_spread$long))
coord_trawls <- SpatialPoints(coord_trawls, proj4string = CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))
raster_values <-raster::extract(oceans_raster, coord_trawls, df = T)

crs(coord_trawls)


#made up raster to test if it works with another raster and it does. 
#Not sure why it will work here and not with oceans raster

r <- raster(ncol=36, nrow=18, vals=1:(18*36))
re <- raster::extract(r, coord_trawls, df = T)
plot(r)










