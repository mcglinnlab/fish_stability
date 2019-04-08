#Rasterizing Polygons

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)

# read in the ocean
oceans <- readOGR(dsn = "./Environment", layer = "ne_10m_ocean")

# create a global raster layer
oceans <- spTransform(oceans, CRS("+proj=cea +units=km"))
oceans_raster <- raster(oceans)
res(oceans_raster) <- 50

# saving the world raster grid
save(oceans_raster, file = './Data/raster/oceans_raster.Rdata')
load('./Data/raster/oceans_raster.Rdata')


# making continents polygon ## where is continent data coming from? 
continents <- shapefile('./data/continent/continent')
continents <- spTransform(continents, CRS("+proj=cea +units=km"))



#Raterizing SEAMAP-SA Data
#SEAMAP_sub <- read.csv('./data/Environment/temp.csv')
#Trawl_coord <-c('SEAMAP_sub$LATITUDESTART', 'SEAMAP_sub$LONGITUDESTART', 'SEAMAP_sub$COLLECTIONNUMBER')
coordinates(Trawl_coord) <- ~LONGITUDE + LATITUDE
proj4string(Trawl_coord) <- "+proj=longlat +datum=WGS84"
Trawl_coord <- spTransform(Trawl_coord, CRS("+proj=cea +units=km"))
Trawl_raster <- rasterize(Trawl_coord, oceans_raster, 'Meandepth')
save(Trawl_raster, file = './data/raster/temp_raster.Rdata')
load('./Data/raster/temp_raster.Rdata')



# Enivironmental variables

# create a temperature raster
temp <- read.csv('./Environment/temp.csv')
temp$Meandepth <- rowMeans(temp[,3:87], na.rm = TRUE)
coordinates(temp) <- ~LONGITUDE + LATITUDE
proj4string(temp) <- "+proj=longlat +datum=WGS84"
temp <- spTransform(temp, CRS("+proj=cea +units=km"))
temp_raster <- rasterize(temp, oceans_raster, 'Meandepth')
save(temp_raster, file = './data/raster/temp_raster.Rdata')
load('./data/raster/temp_raster.Rdata')

# create a salinity raster
salinity <- read.csv('./Environment/salinity.csv')
salinity$Meandepth <- rowMeans(salinity[,3:86], na.rm = TRUE)
coordinates(salinity) <- ~ Longitude + Latitude
proj4string(salinity) <- "+proj=longlat +datum=WGS84"
salinity <- spTransform(salinity, CRS("+proj=cea +units=km"))
salinity_raster <- rasterize(salinity, oceans_raster, 'Meandepth')
save(salinity_raster, file = './data/raster/salinity_raster.Rdata')
load('./data/raster/salinity_raster.Rdata')








