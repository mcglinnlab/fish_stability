#Creating Rasterized Maps

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)
library(leaflet)

library(tmap)
library(lattice)
library(cartography)
library(maps)
library(ggplot2)
library(sp)
library(maptools)
library(tmaptools)



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



  
  #### plotting subset of rasters that match time bin requirement #####
  
  pal <- colorNumeric(palette = "YlGnBu", domain = 0:100)
  m <- leaflet()%>% setView(lng = -79.2532, lat = 32.3484, zoom = 5.8)
  m %>% 
    addTiles() %>%
    addRasterImage(oc_raster, opacity = 0.8)
  addLegend(m, "bottomright", values = ~Trawl_raster$layer, title = "Trawl Density", opacity = 1, pal=pal)
  
  
  
  
  #### plotting all raster regions #####
 
   tm_shape(all_raster) +
    tm_raster(title = "All Raster Regions") +
    tm_shape(continents) +
    tm_borders(lwd = 0.5) +
    tm_scale_bar(position = c("left", "bottom"))
  
#### plotting raster regions in all time bins ####
  tm_shape(timebin_raster) +
    tm_raster(title = "Subset Represented in All Time Bins") +
    tm_shape(continents) +
    tm_borders(lwd = 0.5) +
    tm_scale_bar(position = c("left", "bottom"))
  
  
  