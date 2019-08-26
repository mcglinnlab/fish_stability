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


## Raster Map of Trawl Density Throughout Study Area

pdf('./figures/leafletmapraster.pdf')

m <- leaflet() %>% setView(lng = -79.2532, lat = 32.3484, zoom = 5.8)
m %>% addTiles() %>%
  addRasterImage(Trawl_raster, opacity = 0.8)
  addLegend("bottomright", pal = pal, values = Trawl_raster$layer, title = "Trawl Density", opacity = 1)

dev.off()
  






