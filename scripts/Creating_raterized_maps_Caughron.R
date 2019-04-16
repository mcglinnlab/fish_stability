#Creating Rasterized Maps

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)
library(leaflet)


## basic plots ##

map('state')
points(SEAMAP_sub2[,c("LONGITUDESTART","LATITUDESTART")], col='red')

map('world')
points(SEAMAP_sub2[,c("LONGITUDESTART","LATITUDESTART")], col='red')
axis(side=1)



#Creating Map


## map experiment Trawl Data ## How to fix rasterizing to get data to show up? 

m <- leaflet() %>% setView(lng = -79.2532, lat = 32.8484, zoom = 5)
m %>% addTiles() %>%
  addRasterImage(Trawl_raster, opacity = 0.8)


## map experiment Temp Data ## Why is temp so choppy? Can be fixed? 

pal <- colorNumeric(palette = "YlGnBu", domain = temp_raster$layer)

t <- leaflet() %>% setView(lng = -79.2532, lat = 32.8484, zoom = 5)
t %>% addTiles() %>%
  addRasterImage(temp_raster, opacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values= ~temp_raster$layer, title = "SST", opacity = 1)


## map experiment Salinity ## 

s <- leaflet() %>% setView(lng = -79.2532, lat = 32.8484, zoom = 5)
s %>% addTiles() %>%
  addRasterImage(salinity_raster, opacity = 0.8) %>%
  addLegend("bottomright", pal = pal, values= ~salinity_raster$layer, title = "Salinity", opacity = 1)

