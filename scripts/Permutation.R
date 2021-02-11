#permutation of events within each raster region to account for unequal sampling effort

#series of three for loops accounting for year, grid ID, and then permutation of selecting events

## resolving sampling for 0.2 resolution ##

library(dplyr)

#reading in data 
    #best to read in data with yrcat category added outside in excel

s_environ <- read.csv("~./fish_stability/data/s_environ.csv", header = T)


#### PERMUTATION 1 YR BIN ####

#creating null vectors

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
  yearpull <- subset(s_environ, s_environ$year == i)
  yearIDs <- unique(s_environ$ID[s_environ$year == i])
  
  #subsetting the trawl events that occurred in each ID 
  for (j in yearIDs) {
    
    #subsetting events avail for subsampling
    eventsavail <- subset(yearpull, yearpull$ID == j)
    
    #if raster density is equal or less than threshold; threshold = 5 for now
    if (length(eventsavail$event) == 5) {
      
      event <- eventsavail
      
      #calculate
      averageS <- mean(event$S)
      averagebio <- mean(event$biomass)
      varbio <- var(event$biomass)
      
      
      #store
      ID <- j
      year <- i
      tempresults1 <- data.frame(ID, year, averageS, averagebio, varbio)
      colnames(tempresults1) <- c("ID", 'year', 'averageS', 'averagebio',
                                  'varbio')
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
        
      }
      
      ID <- j
      year <- i
      tempresults <- data.frame(ID, year, mean(averageS), mean(averagebio), 
                                mean(varbio))
      colnames(tempresults) <- c('ID', 'year', 'averageS', 'averagebio',
                                 'varbio')
      
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

#write results with year aggregating
write.csv(resultsfullpoint2, "~./fish_stability/data/yrag_resultsfullpoint2.csv")





#### PERUMATION 3 YR BIN ####

#creating null vectors

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

#year for loop
for (i in c("a","b","c","d","e","f","g","h","i")) {
  
  #subsetting the IDs that were sampled in year i 
  yearpull <- subset(s_environ, s_environ$yrcat == i)
  yearIDs <- unique(s_environ$ID[s_environ$yrcat == i])
  
#subsetting the trawl events that occurred in each ID 
  for (j in yearIDs) {
    
    #subsetting events avail for subsampling
    eventsavail <- subset(yearpull, yearpull$ID == j)
    
    #if raster density is equal or less than threshold; threshold = 5 for now
    if (length(eventsavail$event) == 5) {
      
      event <- eventsavail
      
      #calculate
      averageS <- mean(event$S)
      averagebio <- mean(event$biomass)
      varbio <- var(event$biomass)
     
      
      #store
      ID <- j
      year <- i
      tempresults1 <- data.frame(ID, year, averageS, averagebio, varbio)
      colnames(tempresults1) <- c("ID", 'year', 'averageS', 'averagebio',
                                  'varbio')
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
        
      }
      
      ID <- j
      year <- i
      tempresults <- data.frame(ID, year, mean(averageS), mean(averagebio), 
                                mean(varbio))
      colnames(tempresults) <- c('ID', 'year', 'averageS', 'averagebio',
                                 'varbio')
      
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
yrag_three <- rbind(results, results1)

#write results with year aggregating
write.csv(yrag_three, "~./fish_stability/data/yrag_three.csv")




#check to make sure all raster regions were sampled for a given year #revisit this line
#setequal(yearIDs, resultsfull$ID)






