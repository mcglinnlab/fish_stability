
#### EXAMPLE OF CODE USING BCI DATA ####

#Example of rarefy
data(BCI)
S <- specnumber(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)


####CREATING RASTER SCALE COMMUNITY MATRIX ####
  #creating new col that merges col ID and yrcat. This makes unique ID for each raster through time.
s_rarefac$ID_yrcat <- paste(s_rarefac$ID, "_", s_rarefac$yrcat)

  #list of unique raster IDs through time
uniqueID <- unique(s_rarefac$ID_yrcat)


  ## for loop to calculate community matrix at the raster scale with presence/absence ##
rastercom_mat <- NULL


for(i in  uniqueID) {
    #pulling events from each unique ID through time
  IDpull <- subset(s_rarefac, s_rarefac$ID_yrcat == i)
  
    #removing environmental columns leaving only comm matrix #need to figure out if I need the first column (X)
  comm_mat <-IDpull[,20:212]
  
    #summing the columns in the community matrix. This tells us how many events observed a species in a raster
  rastercolsum <- colSums(comm_mat)
  
    #changing col sum to presence/absence
  row <- ifelse(rastercolsum > 0 , 1, 0)
  
    #adding each run with a unique ID to a matrix using rbind
  rastercom_mat <- rbind(rastercom_mat, row)
}

  #turning output into a data frame and adding column with IDs back
rastercom_mat <- as.data.frame(rastercom_mat)
rastercom_mat <- as.data.frame(cbind(uniqueID, rastercom_mat))

  #saving
#write.csv(rastercom_mat, "~/fish_stability/data/rastercom_mat.csv")



## for loop to calculate community matrix at the raster scale with abundance ##

rastercom_mat_abun <- NULL

for(i in  uniqueID) {
  #pulling events from each unique ID through time
  IDpull <- subset(s_rarefac, s_rarefac$ID_yrcat == i)
  
  #removing environmental columns leaving only comm matrix #need to figure out if I need the first column (X)
  comm_mat <-IDpull[,20:212]
  
  #summing the columns in the community matrix. This tells us how many events observed a species in a raster
  rastercolsum <- colSums(comm_mat)
  
  #adding each run with a unique ID to a matrix using rbind
  rastercom_mat_abun <- rbind(rastercom_mat_abun, rastercolsum)
}

  #turning output into a data frame and adding column with IDs back
rastercom_mat_abun <- as.data.frame(rastercom_mat_abun)
rastercom_mat_abun <- as.data.frame(cbind(uniqueID, rastercom_mat_abun))




#### trying to do rarefac

  ## using presence/absence matrix ##

S <- specnumber(rastercom_mat[,-1]) # observed number of species
(raremax <- min(rowSums(rastercom_mat[,-1])))
Srare <- rarefy(rastercom_mat[,-1], raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(rastercom_mat[,-1], step = 20, sample = raremax, col = "blue", cex = 0.6, label = F)


  ## using number of individuals matrix ##
S <- specnumber(rastercom_mat_abun[,-1]) # observed number of species
raremax <- min(rowSums(rastercom_mat_abun[,-1]))
Srare <- rarefy(rastercom_mat_abun[,-1], raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(rastercom_mat_abun[,-1], step = 20, sample = raremax, col = "blue", cex = 0.6, label = F)



####Practice#### 
  #pulling events for a year bin
yearpull <- subset(s_rarefac, s_rarefac$yrcat == "a")
  #pulling events in a specific raster ID
eventsavail <- subset(yearpull, yearpull$ID == "2314")
  #subset columns from s_rarefac data frame
comm_mat <- eventsavail[,c(1, 20:211)]
  #caclulating rarecurve on each event
#rarecurve(comm_mat)

  #need to calculate curve for whole raster region not just for each event
  #first need to sum down the col
sumofcol <- base::colSums(comm_mat)
  #determine a new presence absence for each raster region in each year cat
rastercomm_vec <- ifelse(sumofcol > 0, 1, 0)







####FOR LOOPS; MAY OR MAY NOT NEED ####
### NOTE: need to do some thinking about how I want the rarefac information to come out 
  # in a data frame. I have bare bones of for loops that will subset data to what I want. 
  # Data frame needs an index column 1:nrow.  

#uses data frame s_rarefac


#subsetting events for each raster square for each raster time block
for (y in c("a" #,"b","c","d","e","f","g","h","i"
            )) {
      #have to figure out this subsetting, do i need to add ID column and yrcat
      #to s_comm  
  #subsetting the IDs that were sampled in year y 
  yearpull <- subset(s_rarefac, s_rarefac$yrcat == y)
  yearIDs <- unique(s_rarefac$ID[s_rarefac$yrcat == y])
  
  #subsetting the trawl events that occurred in each ID 
  for (r in yearIDs) {
    
    #subsetting events avail for rarefaction
    eventsavail <- subset(yearpull, yearpull$ID == r)
    
    #removing columns that are not community matrix 
    comm_mat <- eventsavail[,c(1,19:211)]
    
    #rarefaction of that raster region and yr cat 
    S <- specnumber(comm_mat)
    
    raremax <- min(rowSums(comm_mat))
    Srare <- rarefy(comm_mat, raremax)
    
    #store
    ID <- r
    yrcat <- y
    tempresults1 <- data.frame(ID, yrcat, S, Srare )
    colnames(tempresults1) <- c("ID", 'yrcat', 'maxspecies', 'rarefy')
  }
}
    
    
    


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






