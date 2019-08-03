#Rasterizing Polygons

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)

# read in the ocean
oceans <- readOGR(dsn = "./shapefiles/ocean_raster", layer = "ne_10m_ocean")

# create a global raster layer
oceans <- spTransform(oceans, CRS("+proj=longlat +units=km +lat_0=32.4 +lon_0=-79.6"))
oceans_raster <- raster(oceans)
res(oceans_raster) <- 50

# saving the world raster grid
save(oceans_raster, file = './data/raster/oceans_raster.Rdata')
load('./Data/raster/oceans_raster.Rdata')


# making continents polygon  
continents <- shapefile('./shapefiles/continent/continent/continent.shp')
continents <- spTransform(continents, CRS("+proj=longlat +units=km +lat_0=32.4 +lon_0=-79.6"))



## Rasterizing SEAMAP-SA Data ##
SEAMAP_sub <- read.csv('./data/SEAMAP_sub.csv')

#subsetting columns
Trawl_coord <- SEAMAP_sub[,c("LONGITUDESTART","LATITUDESTART", "EVENTNAME")]

#taking out repeats of the event number
Trawl_coord <- Trawl_coord[!duplicated(Trawl_coord),]

#making coordinates numeric
Trawl_coord$LATITUDESTART <- as.numeric(as.character(Trawl_coord$LATITUDESTART))
Trawl_coord$LONGITUDESTART <- as.numeric(as.character(Trawl_coord$LONGITUDESTART))
Trawl_coord$COLLECTIONNUMBER <- as.numeric(as.character(Trawl_coord$EVENTNAME))

#adding identity column called trawl number
Trawl_coord$TRAWLNUMBER <- 1

#setting lat and long columns and projection
coordinates(Trawl_coord) <- ~ LONGITUDESTART + LATITUDESTART
proj4string(Trawl_coord) <- "+proj=longlat +units=km +lat_0=32.4 +lon_0=-79.6"
#Trawl_coord <- spTransform(Trawl_coord, "+proj=longlat +units=km +lat_0=32.4 +lon_0=-79.6" )

#rasterizing trawl sum data
Trawl_raster <- rasterize(Trawl_coord, oceans_raster, Trawl_coord$TRAWLNUMBER, fun = "sum")
res(Trawl_raster)
# When I plot here it gives me one large green square with 8000 trawls within so I tried adjusting resolution below#
plot(Trawl_raster)

Trawl_raster.disaggregate <- disaggregate(Trawl_raster, fact=10)
res(Trawl_raster.disaggregate)
#With resolution increased by 10 fold still shows same big green square with 8000 trawls. Not sure why its not becoming more detailed. 
plot(Trawl_raster.disaggregate)


#saving Trawl Raster File
#save(Trawl_raster, file = './data/raster/trawl_raster.Rdata')
#load('./data/raster/trawl_raster.Rdata')



# Enivironmental variables

# create a temperature raster
temp <- read.csv('./data/environmentaldata/temp.csv')
temp$Meandepth <- rowMeans(temp[,3:87], na.rm = TRUE)
coordinates(temp) <- ~LONGITUDE + LATITUDE
proj4string(temp) <- "+proj=longlat +datum=WGS84"
temp <- spTransform(temp, CRS("+proj=cea +units=km"))
temp_raster <- rasterize(temp, oceans_raster, 'Meandepth')
save(temp_raster, file = './data/raster/temp_raster.Rdata')
load('./data/raster/temp_raster.Rdata')

# create a salinity raster
salinity <- read.csv('./data/environmentaldata/salinity.csv')
salinity$Meandepth <- rowMeans(salinity[,3:86], na.rm = TRUE)
coordinates(salinity) <- ~ Longitude + Latitude
proj4string(salinity) <- "+proj=longlat +datum=WGS84"
salinity <- spTransform(salinity, CRS("+proj=cea +units=km"))
salinity_raster <- rasterize(salinity, oceans_raster, 'Meandepth')
save(salinity_raster, file = './data/raster/salinity_raster.Rdata')
load('./data/raster/salinity_raster.Rdata')









