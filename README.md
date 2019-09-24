# fish_stability
code for analysis of fish biodiversity and stability

This project serves as an initial analysis for my larger thesis work.

Objectives of my overall work: 

1: Investigate the relationships between species diversity and community biomass and stability dynamics using fishery independent monitoring data.

2: Explore whether stability is due to asynchronous interaction between species and if this mechanism exhibits spatial scale dependence using fishery independent monitoring data.

3: Test theoretical prediction of scale-dependence in diversity-stability relationship using fishery independent monitoring data.



Scripts included in the scripts folder are as follows:

data_processing_Baker
data_processing_Caughron
rasterizing_polygons_Caughron
Creating_rasterized_maps_Caughron
spread_function
DSR_relationship
Investigations
environmental_vars_rasters



data_processing_Baker

  reads in SEAMAP data, cleans and organizes, completed previously by Baker
  
  
data_processing_Caughron

  large portion derived from data_processing_Baker, however end of script restructures data into data frame     s_spread which removes unneeded columns and calculates new columns for species richness, summed biomass and   raster IDs for different resoultions. Species now in wide form with number of individuals in rows. Each row   in s_spread represents a unique event. 
  
  
rasterizing_polygons_Caughron

  Oceans raster is created that will be used as base raster for further analysis. This is where resolution of   raster can be adjusted. Rasters for trawl density, species richness, biomass var, and cell identity created.
  
  
Creating_rasterized_maps_Caughron
  
  Creating maps in leaflet of raster layers for trawl density, species richness, and biomass var
  
  
spread_function

  function called spread_with_multiple_values() which is used in data_processing_Caughron and can be accessed   at: https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R


DSR_relationship

  script will eventually be used to perform analysis on DSR relationship after data structing is complete. 
  
  
Investigations
  
  script to try things out.


environmental_vars_rasters
  
  some code to create rasters for environmental variables that was created earlier that is not needed now but   didn't want to get rid of it. 




Notes from cruise
  file summarizes some thoughts from the summer sampling season cruises
  
Fish species.csv
  csv listing species of fish used to remove inverts and other from data set

