#Creating Rasterized Maps

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)
library(leaflet)




##Creating Maps ##


## Raster Map of Trawl Density Throughout Study Area

#pdf('./figures/leafletmapraster.pdf')
pal <- colorNumeric(palette = "YlGnBu", domain = 0:100)
m <- leaflet() %>% setView(lng = -79.2532, lat = 32.3484, zoom = 5.8)
m %>% addTiles() %>%
  addRasterImage(Trawl_raster, opacity = 0.8)
addLegend(m, "bottomright", values = ~Trawl_raster$layer, title = "Trawl Density", opacity = 1, pal=pal)

#dev.off()
  
  
## Raster Map of Average Species Richness ##

#pdf('./figures/leafletmapraster.pdf')
pal <- colorNumeric(palette = "YlGnBu", domain = 0:100)
sp <- leaflet() %>% setView(lng = -79.2532, lat = 32.3484, zoom = 5.8)
sp %>% addTiles() %>%
    addRasterImage(SpeciesRich_raster, opacity = 0.8)
  addLegend("bottomright", values = Trawl_raster$layer, title = "Trawl Density", opacity = 1, pal=pal)
  
  #dev.off()
  

  
## Raster Map of Var in Biomass ## 

#pdf('./figures/leafletmapraster.pdf')
pal <- colorNumeric(palette = "YlGnBu", domain = 0:100)
bv <- leaflet() %>% setView(lng = -79.2532, lat = 32.3484, zoom = 5.8)
bv %>% addTiles() %>%
  addRasterImage(BiomassVar_raster, opacity = 0.8)
  addLegend("bottomright", values = Trawl_raster$layer, title = "Trawl Density", opacity = 1, pal=pal)
  
  #dev.off()


