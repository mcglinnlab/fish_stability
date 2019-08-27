
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
TrawlRasterData <- data.frame(TrawlRasterData[!is.na(TrawlRasterData)])

#Summarizing Different Options
SummaryRes0.3 <- summary(TrawlRasterData)
length(TrawlRasterData$TrawlRasterData..is.na.TrawlRasterData..)

#Determining coordinates for each of the rasters

SummaryRes0.1
SummaryRes0.15
SummaryRes0.2
SummaryRes0.25
SummaryRes0.3

