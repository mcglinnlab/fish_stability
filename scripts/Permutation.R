#permutation of events within each raster region to account for unequal sampling effort

#series of three for loops accounting for year, grid ID, and then permutation of selecting events

## resolving sampling for 0.2 resolution ##
#s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)
raster_values0.2 <- read.csv("~./fish_stability/data/raster_values0.2.csv", header = T)
raster_values0.2 <- raster_values0.2[,2:3]

ID.df <- as.data.frame(cbind(s_rarefac$EVENTNAME,s_rarefac$year, raster_values0.2$layer, s_rarefac$S, s_rarefac$biomass))
colnames(ID.df) <- c("event","year", "point2resID", "S", "biomass")

yearpull <- NULL
eventsavail <- NULL
event <- NULL
averageS <- NULL
averagebio <- NULL
varbio <- NULL
averagevarbio <- NULL
NEW.df <- NULL

#pulling out events for specific years
for (i in 1989:2015) {
  yearpull <- ID.df[ID.df$year == "i",]
  #pulling out events from each raster region
  for (i in min(raster_values0.2):max(raster_values0.2)) {
    
    eventsavail <- yearpull$event[yearpull$pointresID == "i"]
    #permutation
    for (i in 1:1000) {
        #rasters with densities less then arbitrary threshold (for now)
      if(length(eventsavail) < 5) {
        
      averageS <- mean(yearpull$S[yearpull$event == "c(eventavail)"])
      averagebio <- mean(yearpull$biomass[yearpull$event == "c(eventavail)"])
      varbio <- var(yearpull$biomass[yearpull$event == "c(eventavail)"])
      #rasters that require subsampling
      } else{
        
      event <- sample(eventsavail, 5, replace = F)
      averageS <- mean(yearpull$S[yearpull$eventname == "c(event)"])
      averagebio <- mean(yearpull$biomass[yearpull$eventname == "c(event)"])
      varbio <- var(yearpull$biomass[yearpull$eventname == "c(event)"])
      }
    }
    #creating new data frame with new averages for S, biomass, var biomass, and raster ID
    ID <- i
    averageS <- mean (averageS)
    averagebio <- mean(averagebio)
    averagevarbio <- mean(varbiomass)
    NEW.df <- as.data.frame(cbind(pointresID, averageS, averagebio, averagevarbio))
 }
}
