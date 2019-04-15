#Creating Rasterized Maps

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)
library(leaflet)

#Creating Map

### same error comes up for both maps in rgdal: : rawTransform(projfrom, projto, ...): 54 projected points not finite ###

## map experiment Trawl Data ## How to fix rasterizing to get data to show up? 

m <- leaflet() %>% setView(lng = -79.2532, lat = 32.8484, zoom = 5)
m %>% addTiles() %>%
  addRasterImage(Trawl_raster, opacity = 0.8)


## map experiment Temp Data ## Why is temp so choppy? Can be fixed? 

m <- leaflet() %>% setView(lng = -79.2532, lat = 32.8484, zoom = 5)
m %>% addTiles() %>%
  addRasterImage(temp_raster, opacity = 0.8)


