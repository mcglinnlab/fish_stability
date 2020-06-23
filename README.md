# fish_stability
code for analysis of fish biodiversity and stability

This project serves as the analysis for my larger thesis work.

Objectives of my overall work: 

1: Investigate the relationships between species diversity and community biomass and stability dynamics using fishery independent monitoring data.

2: Test theoretical prediction of scale-dependence in diversity-stability relationship using fishery independent monitoring data.



Key Scripts included in the scripts folder are as follows:

DSR_analysis
Permutation
SpatialScalingDSR
creating_rasterized_maps
data_processing_Caughron
rasterizing_polygons_Caughron
Chap1
BEF analysis
SSR_analysis

DSR_analysis
  reads in resultsfullpoint2 and creates graphs of various relationships between species richness, biomass, variance and invariance. 

Permutation
  reads in s_rarefac and runs series of for loops to groups trawl events into raster IDs and recalculate adjusted values for species richness, biomass, variance, cv, and invariance. Then creates new dataframe and writes resultsfullpoint2. 
  
SpatialScalingDSR
  reads in resultsfullpoint2 and uses information to look at relationship between species richness and invar as it scales. 
  
creating_rasterized_maps
  creates maps of rasters that look at species richness, biomass, and trawl density througout sampling area.

data_processing_Baker

  reads in SEAMAP data, cleans and organizes, completed previously by Baker
  
  
data_processing_Caughron

  large portion derived from data_processing_Baker, however end of script restructures data into data frame     s_spread which removes unneeded columns and calculates new columns for species richness, summed biomass and   raster IDs for different resoultions. Species now in wide form with number of individuals in rows. Each row   in s_spread represents a unique event. 
  
rarefaction_curve
  Creating rarefaction curves that may be useful later in SpatialScalingDSR context. 
  
  
rasterizing_polygons_Caughron

  Oceans raster is created that will be used as base raster for further analysis. This is where resolution of   raster can be adjusted. Rasters for trawl density, species richness, biomass var, and cell identity created.
  
  
spread_function

  function called spread_with_multiple_values() which is used in data_processing_Caughron and can be accessed at: https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
  
  Chap1 
summary of BEF results summarizing relationships under objective 1

BEF analysis
diversity ecosystem function and stability relationships at the single raster level

SSR_analysis
Major analyses under the second objective. Testing scale dependence in diversity stability relationship and diversity biomass relationship. 


  




Notes from cruise

  file summarizes some thoughts from the summer sampling season cruises
  
Notes

  file acts as journal of thoughts and progress through project.
  
Fish species.csv

  csv listing species of fish used to remove inverts and other from data set

