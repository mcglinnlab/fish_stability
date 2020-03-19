
#### CALCULATING CUMULATIVE SPECIES RICHNESS ####

#Example of rarefy
data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

#uses data frame s_comm and s_environ


#subsetting events for each raster square for each raster time block
for (y in c("a","b","c","d","e","f","g","h","i")) {
      #have to figure out this subsetting, do i need to add ID column and yrcat
      #to s_comm 
  #subsetting the IDs that were sampled in year y 
  yearpull <- subset(s_comm, s_environ$yrcat == y)
  yearIDs <- unique(s_environ$ID[s_environ$yrcat == y])
  
  #subsetting the trawl events that occurred in each ID 
  for (r in yearIDs) {
    
    #subsetting events avail for rarefaction
    eventsavail <- subset(yearpull, yearpull$ID == r)
    
    #rarefaction of that raster region and yr cat 
    S <- specnumber()
    #store
    ID <- r
    yrcat <- y
    tempresults1 <- data.frame(ID, yrcat, )
    colnames(tempresults1) <- c("ID", 'yrcat',)
    
    
    
    


#### Spatial Scaling of DSR ####
# Idea for how to write spatial part
  #Make a loop that randomize the list of ID squares
  #Loop makes calcs of var of invar and average species richness with 1 square included, 2 square until length(ID)
  #Rerandomize list of IDs and repeat many many times to create curves
  #Average curves


#renaming dataframe so results dataframe stays intact

ssr_results <- resultsfullpoint2

#fake data set to try things on
#a <- c(1,2,3,4,5,6,7,8,9,10)
#b <- c(2,3,5,6,7,3,5,4,5,6)
#ab <- data.frame(cbind(a,b))



#examples of reordering data set
#rows <- sample(nrow(ab))
#abnew <- ab[rows, ]


for (p in 1:5) {
  rows <- sample(nrow(ssr_results))
  ssr_new <- ssr_results[rows, ]
  
  for (a in 1:length(ssr_results)){
    #selects 1 row then 2 then 3 until all row included
    sub_ssr <- ssr_new[1:a, ]
    
    #vector of cumulative average of biomass and species as more IDs added
    bio[a] <- average(sub_ssr$averagebio)
    S[a] <- average(sub_ssr$averageS)
    
    #creates vectors of just varbio and averagebio columns to be used to calc 
      #new cv and new invar for growing number of raster IDs
    varbio[a] <- sub_ssr$varbio
    averabiobio[a] <- sub_ssr$averagebio
    
    #calculating cv and invar each time a new raster ID is added
    cv[a] <- varbio[a]/(averagebio[a] ^ 2)
    invar[a] <- 1/(cv[a])
    
    
    #creating columns that tell me how many raster IDs are included (numID) and 
    # which reshuffling the row is associated with (run)
    numID <- a
    run <- p
    
    #creating data frame to save to
    tempcurve <- data.frame(cbind(numID, run, bio, S, CV, invar))
    tempcurve <- colnames("numID", "run", "bio", "S", "CV", "invar")
  }
  curve <- rbind(curve, tempcurve)  
}






