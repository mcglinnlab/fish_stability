
#DSR Relationship

#load data needed: data organized into raster regions, varience in biomass through time for raster region and
#cumulative diversity for raster region

#sum biomass for all collections within each respective raster region for each year
#calculate variance through time for biomass within each raster region
#calculate average species number for each respective raster region

#create new data frame with columns: raster region ID, var in biomass, average species richness, possibly geographic description (ex "Raleigh Bay, SC, GA")
#plot var in biomass as a function of species richness 



#Determining how many trawls per raster region
TrawlRasterData <- data.frame(Trawl_raster@data@values)
  #removing NA values
#TrawlRasterData <- data.frame(TrawlRasterData[!is.na(TrawlRasterData)])

#Determining average species richness per raster region
SpeciesRichRasterData <- data.frame(SpeciesRich_raster@data@values)
  #removing NA values
#SpeciesRichRasterData <- data.frame(SpeciesRichRasterData[!is.na(SpeciesRichRasterData)])

#Determining var in total biomass per raster region
BiomassVarRasterData <- data.frame(BiomassVar_raster@data@values)
  #removing NA values
#BiomassVarRasterData <- data.frame(BiomassVarRasterData[!is.na(BiomassVarRasterData)])

#Binding trawl density, average species richness, var in biomass into one data frame
AvSPasfBioVar <- data.frame(cbind(TrawlRasterData$Trawl_raster.data.values, SpeciesRichRasterData$SpeciesRich_raster.data.values, BiomassVarRasterData$BiomassVar_raster.data.values))

#changing name of the columns from x1, x2, x3 to something useful
names(AvSPasfBioVar) <- c("TrawlDensity", "AverageSpecRich", "VarTotBiomass")

#plotting var in biomass as a function of average species richness. Removing NAs

plot(AvSPasfBioVar$VarTotBiomass~AvSPasfBioVar$AverageSpecRich, ylab= "Varience in Total Biomass", xlab= "Average Species Richness", main= "Resolution is 0.2")





