#permutation of events within each raster region to account for unequal sampling effort

#series of three for loops accounting for year, grid ID, and then permutation of selecting events

## resolving sampling for 0.2 resolution ##
s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)
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
# make empty results matrix or data.frame, can be a matrix if only inputing numbers 
results <- matrix(NULL, ncol = ..., nrow = )
colnames(results) = 
for (i in 1989:2015) {
  yearpull <- subset(ID.df, ID.df$year == i)
  
  #pulling out events from each raster region
  for (j in min(raster_values0.2):max(raster_values0.2)) {
    eventsavail <- subset(yearpull, yearpull$point2resID == j)

    if(length(eventsavail$event) < 5) {
      #rasters with densities less then arbitrary threshold (for now just taken average and var)
      #instead = future: when equal to threshold vv but when less then write in to ignore. 
        
      averageS <- mean(eventsavail$S)
      averagebio <- mean(eventsavail$biomass)
      varbio <- var(eventsavail$biomass)
      tempresults = data.frame(averageS, averagebio, varbio)
      #rasters that require subsampling
    } else{
        #permutation
        for (k in 1:1000) {
        
          event <- eventsavail[sample(nrow(eventsavail), 5, replace = F), ]
          averageS[k] <- mean(event$S)
          averagebio[k] <- mean(event$biomass)
          varbio[k] <- var(event$biomass)
        }
        tempresults <- data.frame( S = mean(averageS), B = mean(averaebio), sigmaB = mean(varbio))
      
    }
    # row bind into results matrix while adding year and raster ids as columns
    results <- rbind(results, data.frame(id = i, yr = j, tempresults))
 }
}
