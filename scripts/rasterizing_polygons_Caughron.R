#Rasterizing Polygons

library(rgdal)
library(sp)
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

extent <- extent(-87,-75,20,40)
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



#subsetting data
SEAMAP_invest <- SEAMAP_sub[,c("DATE", "Year", "LONGITUDESTART", "LATITUDESTART", "COLLECTIONNUMBER", "EVENTNAME", "SPECIESSCIENTIFICNAME", "SPECIESCOMMONNAME", "NUMBERTOTAL", "SPECIESTOTALWEIGHT")]

#adding columns for species richness per event and total biomass per event
SEAMAP_invest$speciesrichness <- with(SEAMAP_invest, ave(EVENTNAME, EVENTNAME, FUN = function(x) length(unique(x))))
SEAMAP_invest$biomass <- with(SEAMAP_invest, ave(SPECIESTOTALWEIGHT, EVENTNAME, FUN = sum))

tst = with(SEAMAP_invest, aggregate(SEAMAP_invest[ , c('weight', 'biomass')],
          by = list(SPECIESSCIENTIFICNAME, EVENTNAME),
          function(x) mean(x))

library(dplyr)
SEAMAP_invest %>%
  summarize()

dat <- SEAMAP_invest %>%
  group_by(EVENTNAME) %>%
  summarize(S = length(unique(SPECIESSCIENTIFICNAME)),
            biomass = sum(SPECIESTOTALWEIGHT))




#removing repeated collection number rows-- essentially removing each row is species. Invidiual species no longer important
SEAMAP_nonrepeat <- SEAMAP_invest[!duplicated(SEAMAP_invest$EVENTNAME),]


#making coordinates numeric
SEAMAP_nonrepeat$LATITUDESTART <- as.numeric(as.character(SEAMAP_nonrepeat$LATITUDESTART))
SEAMAP_nonrepeat$LONGITUDESTART <- as.numeric(as.character(SEAMAP_nonrepeat$LONGITUDESTART))
SEAMAP_nonrepeat$COLLECTIONNUMBER <- as.numeric(as.character(SEAMAP_nonrepeat$EVENTNAME))
SEAMAP_nonrepeat$speciesrichness <- as.numeric(as.character(SEAMAP_nonrepeat$speciesrichness))

#adding identity column called trawl number
SEAMAP_nonrepeat$TRAWLNUMBER <- 1

#setting lat and long columns and projection
coordinates(SEAMAP_nonrepeat) <- ~ LONGITUDESTART + LATITUDESTART
proj4string(SEAMAP_nonrepeat) <- "+proj=longlat +lat_0=32.4 +lon_0=-79.6"


#Creating Trawl Density Raster
Trawl_raster <- rasterize(SEAMAP_nonrepeat, oceans_raster, SEAMAP_nonrepeat$TRAWLNUMBER, fun = "sum")
res(Trawl_raster)

#Creating Species Richness Raster
SpeciesRich_raster <- rasterize(SEAMAP_nonrepeat, oceans_raster, SEAMAP_nonrepeat$speciesrichness, fun=function(x,...)mean(x))
res(SpeciesRich_raster)

#Creating Biomass Variance Raster
BiomassVar_raster <- rasterize(SEAMAP_nonrepeat, oceans_raster, SEAMAP_nonrepeat$biomass, fun =function(x,...)var(x))
res(BiomassVar_raster)










#output PDF with plot
#pdf('./figures/raster.pdf')
#plot(Trawl_raster)
#dev.off()


#saving Trawl Raster File
#save(Trawl_raster, file = './data/raster/trawl_raster.Rdata')
#load('./data/raster/trawl_raster.Rdata')



# Enivironmental variables

# create a temperature raster
#temp <- read.csv('./data/environmentaldata/temp.csv')
#temp$Meandepth <- rowMeans(temp[,3:87], na.rm = TRUE)
#coordinates(temp) <- ~LONGITUDE + LATITUDE
#proj4string(temp) <- "+proj=longlat +datum=WGS84"
#temp <- spTransform(temp, CRS("+proj=cea +units=km"))
#temp_raster <- rasterize(temp, oceans_raster, 'Meandepth')
#save(temp_raster, file = './data/raster/temp_raster.Rdata')
#load('./data/raster/temp_raster.Rdata')

# create a salinity raster
#salinity <- read.csv('./data/environmentaldata/salinity.csv')
#salinity$Meandepth <- rowMeans(salinity[,3:86], na.rm = TRUE)
#coordinates(salinity) <- ~ Longitude + Latitude
#proj4string(salinity) <- "+proj=longlat +datum=WGS84"
#salinity <- spTransform(salinity, CRS("+proj=cea +units=km"))
#salinity_raster <- rasterize(salinity, oceans_raster, 'Meandepth')
#save(salinity_raster, file = './data/raster/salinity_raster.Rdata')
#load('./data/raster/salinity_raster.Rdata')









