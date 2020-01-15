#permutation of events within each raster region to account for unequal sampling effort

#series of three for loops accounting for year, grid ID, and then permutation of selecting events

## resolving sampling for 0.2 resolution ##

library(dplyr)

#reading in data 

  #s_rarefac has basic data and community matrix with presence absence data for each species
s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)

  #raster_values0.2 has the raster sqaure ID of every trawl event
raster_values0.2 <- read.csv("~./fish_stability/data/raster_values0.2.csv", header = T)
raster_values0.2 <- raster_values0.2[,2:3]

#creating a new data frame with event name, year, ID, S, and biomass

ID.df <- as.data.frame(cbind(s_rarefac$EVENTNAME,s_rarefac$year, 
                             raster_values0.2$layer, s_rarefac$S, s_rarefac$biomass))
colnames(ID.df) <- c("event","year", "point2resID", "S", "biomass")

#creating null vectors that will eventually store data

yearpull <- NULL
eventsavail <- NULL
event <- NULL
averageS <- NULL
averagebio <- NULL
varbio <- NULL
cv <- NULL
invar <- NULL
averagevarbio <- NULL
tempresults <- NULL
tempresults1 <- NULL
results <- NULL
results1 <- NULL

#pulling out events for specific years
# make empty results matrix or data.frame, can be a matrix if only inputing numbers

#year for loop
for (i in 1989:2015) {
  
  #subsetting the IDs that were sampled in year i 
  yearpull <- subset(ID.df, ID.df$year == i)
  yearIDs <- unique(ID.df$point2resID[ID.df$year == i])
  
  #subsetting the trawl events that occurred in each ID 
  for (j in yearIDs) {
    
    #subsetting events avail for subsampling
    eventsavail <- subset(yearpull, yearpull$point2resID == j)
    
    #if raster density is equal or less than threshold; threshold = 5 for now
    if (length(eventsavail$event) == 5) {
      
      event <- eventsavail
      
      #calculate
      averageS <- mean(event$S)
      averagebio <- mean(event$biomass)
      varbio <- var(event$biomass)
      cv <- varbio/(averagebio ^ 2)
      invar <- 1/cv
      
      #store
      ID <- j
      year <- i
      tempresults1 <- data.frame(ID, year, averageS, averagebio, varbio, cv, invar)
      colnames(tempresults1) <- c("ID", 'year', 'averageS', 'averagebio', 'varbio', 'cv', 'invar')
    }
    
    #rasters that require subsampling
    if (length(eventsavail$event) > 5){
      
      #permutation
      for (k in 1:1000){
        
        #subsampling
        event <- eventsavail[sample(nrow(eventsavail), 5, replace = F), ]
        
        #calculating
        averageS[k] <- mean(event$S)
        averagebio[k] <- mean(event$biomass)
        varbio[k] <- var(event$biomass)
        cv[k] <- varbio[k]/(averagebio[k] ^ 2)
        invar[k] <- 1/(cv[k])
      }
      
      ID <- j
      year <- i
      tempresults <- data.frame(ID, year, mean(averageS), mean(averagebio), mean(varbio), mean(cv), mean(invar))
      colnames(tempresults) <- c('ID', 'year', 'averageS', 'averagebio', 'varbio', 'cv', 'invar')
      
    }
    # row bind into results matrix while adding year and raster ids as columns
    results <- rbind(results, tempresults)
    results1 <- rbind(results1, tempresults1)
  }
}


#removing duplicate rows that were added 
results <- results %>%
  distinct(ID, year, .keep_all = TRUE)

#removing raster regions that did not have any trawls for that year
results1 <- results1[complete.cases(results1),] 

#removing duplicate rows that were added
results1 <- results1 %>%
  distinct(ID, year, .keep_all = TRUE)

#binding data frames for rasters that required subsampling and those that did not
resultsfullpoint2 <- rbind(results, results1)

write.csv(resultsfullpoint2, "~./fish_stability/data/resultsfullpoint2.csv")


#check to make sure all raster regions were sampled for a given year #revisit this line
#setequal(yearIDs, resultsfull$ID)
