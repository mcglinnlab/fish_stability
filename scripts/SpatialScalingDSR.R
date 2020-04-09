library(dplyr)

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
  #load s_rarefac from datasets script


  #pulling out 36 predetermined good IDs where we know theres at least 5 events in each time bin
IDlist <- c(1496, 1554, 1610, 1786, 1842, 1844, 1846, 1902, 1906, 1960, 2020,
            2080, 2137, 2138, 2196, 2255, 2313, 2314, 2372, 2373, 2431, 2432,
            2550, 2609, 2610, 2669, 2729, 2788, 2789, 2909, 3029, 3089, 3150,
            3210, 3271, 3331)
s_rarefac_sub <- s_rarefac[s_rarefac$ID %in% IDlist,]

  #creating new col that merges col ID and yrcat. This makes unique ID for each raster through time.
s_rarefac_sub$ID_yrcat <- paste(s_rarefac_sub$ID, "_", s_rarefac_sub$yrcat)

  #list of unique raster IDs through time
uniqueID <- unique(s_rarefac_sub$ID_yrcat)

  ## for loop to calculate community matrix at the raster scale with presence/absence ##
rastercom_mat <- NULL


for(i in uniqueID) {
    #pulling events from each unique ID through time
  IDpull <- subset(s_rarefac_sub, s_rarefac_sub$ID_yrcat == i)
  
    #pulling 5 events from each raster
  event <- IDpull[sample(nrow(IDpull), 5, replace = F), ]
  
    #removing environmental columns leaving only comm matrix #need to figure out if I need the first column (X)
  comm_mat <-event[,19:211]
  
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
# uses s_spread where cells have abundance #
s_spread <- cbind.data.frame(s_rarefac$yrcat, s_spread)
names(s_spread)[names(s_spread) == "rastervals.layer"] <- "ID"
names(s_spread)[names(s_spread) == "s_rarefac$yrcat"] <- "yrcat"
s_spread$ID_yrcat <- paste(s_spread$ID, "_", s_spread$yrcat)
d[is.na(d)] <- 0
s_spread[is.na(s_spread)] <- 0



rastercom_mat_abun <- NULL

for(i in  uniqueID) {
  #pulling events from each unique ID through time
  IDpull <- subset(s_spread, s_spread$ID_yrcat == i)
  
  #removing environmental columns leaving only comm matrix #need to figure out if I need the first column (X)
  comm_mat <-IDpull[,19:218]
  
  #summing the columns in the community matrix. This tells us how many events observed a species in a raster
  rastercolsum <- colSums(comm_mat)
  
  #adding each run with a unique ID to a matrix using rbind
  rastercom_mat_abun <- rbind(rastercom_mat_abun, rastercolsum)
}

  #turning output into a data frame and adding column with IDs back
rastercom_mat_abun <- as.data.frame(rastercom_mat_abun)
rastercom_mat_abun <- as.data.frame(cbind(uniqueID, rastercom_mat_abun))


  #saving
#write.csv(rastercom_mat_abun, "~/fish_stability/data/rastercom_mat_abun.csv")




## for loop to calculate community matrix at the raster scale with biomass ##
  # # uses s_bio_comm community matrix where cells have biomass #
s_bio_comm <- arrange(s_bio_comm, EVENTNAME)
s_bio_comm <- cbind.data.frame(s_rarefac$yrcat, s_rarefac$ID, s_bio_comm)

names(s_bio_comm)[names(s_bio_comm) == "s_rarefac$ID"] <- "ID"
names(s_bio_comm)[names(s_bio_comm) == "s_rarefac$yrcat"] <- "yrcat"
s_bio_comm$ID_yrcat <- paste(s_bio_comm$ID, "_", s_bio_comm$yrcat)


rastercom_mat_bio <- NULL

for(i in  uniqueID) {
  #pulling events from each unique ID through time
  IDpull <- subset(s_bio_comm, s_bio_comm$ID_yrcat == i)
  
  #removing environmental columns leaving only comm matrix
  comm_mat <-IDpull[,4:197]
  
  #summing the columns in the community matrix. This tells us how many events observed a species in a raster
  rastercolsum <- colSums(comm_mat)
  
  #adding each run with a unique ID to a matrix using rbind
  rastercom_mat_bio <- rbind(rastercom_mat_bio, rastercolsum)
}

  #turning output into a data frame and adding column with IDs back
rastercom_mat_bio <- as.data.frame(rastercom_mat_bio)
rastercom_mat_bio <- as.data.frame(cbind(uniqueID, rastercom_mat_bio))

  #saving
write.csv(rastercom_mat_bio, "~/fish_stability/data/rastercom_mat_bio.csv")


#### RARECURVES WITH PRESENCE AND ABUNDANCE DATA ####

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
slope <- rareslope(rastercom_mat_abun[,-1], 100)









#### FOR LOOP CREATING ALL THREE RASTER LEVEL COMMUNITY MATRICES WITH A 5 EVENT PULL ####
  
  #prepping data sets
  # load s_rarefac, s_spread, s_bio_comm, and ID.df
    #s_rarefac for pres/ab
s_rarefac$ID <- ID.df$point2resID
s_rarefac$yrcat <- ID.df$yrcat
s_rarefac$ID_yrcat <- paste(s_rarefac$ID, "_", s_rarefac$yrcat)

    #s_spread for abundance
s_spread$ID <- ID.df$point2resID
s_spread$yrcat <- ID.df$yrcat
s_spread$ID_yrcat <- paste(s_spread$ID, "_", s_spread$yrcat)

s_spread[is.na(s_spread)] <- 0

    #s_bio_comm for biomass
s_bio_comm <- arrange(s_bio_comm, EVENTNAME)

s_bio_comm$ID <- ID.df$point2resID
s_bio_comm$yrcat <- ID.df$yrcat
s_bio_comm$ID_yrcat <- paste(s_bio_comm$ID, "_", s_bio_comm$yrcat)



  #pulling out 36 predetermined good IDs where we know theres at least 5 events in each time bin
IDlist <- c(1496, 1554, 1610, 1786, 1842, 1844, 1846, 1902, 1906, 1960, 2020,
            2080, 2137, 2138, 2196, 2255, 2313, 2314, 2372, 2373, 2431, 2432,
            2550, 2609, 2610, 2669, 2729, 2788, 2789, 2909, 3029, 3089, 3150,
            3210, 3271, 3331)

    #good ID for pres/ab data set s_rarefac
s_rarefac_sub <- s_rarefac[s_rarefac$ID %in% IDlist, ]

    #good ID for abundance data set
s_spread_sub <- s_spread[s_spread$ID %in% IDlist, ]

    #good ID for biomass data set
s_bio_comm_sub <- s_bio_comm[s_bio_comm$ID %in% IDlist, ]


#list of unique raster IDs through time #this works for all three matrices
uniqueID <- unique(s_rarefac_sub$ID_yrcat)

## for loop to calculate community matrix at the raster scale with presence/absence ##
rastercom_mat_pres <- NULL
rastercom_mat_abun <- NULL
rastercom_mat_bio <- NULL

for(i in uniqueID) {
  #pulling events from each unique ID through time
    #pres/ab
  IDpull_pres <- subset(s_rarefac_sub, s_rarefac_sub$ID_yrcat == i)
    #abundance
  IDpull_abun <- subset(s_spread_sub, s_spread_sub$ID_yrcat == i)
    #biomass
  IDpull_bio <- subset(s_bio_comm_sub, s_bio_comm_sub$ID_yrcat == i)
  
  #determining which events will be pulled
  samp <- sample(nrow(IDpull_pres), 5, replace = F)
  
  #pulling 5 events from each raster
    #pres/ab
  event_pres <- IDpull_pres[samp, ]
    #abundance
  event_abun <- IDpull_abun[samp, ]
    #biomass
  event_bio <- IDpull_bio[samp, ]
  
  #removing environmental columns leaving only comm matrix
  
  
    #pres/ab
   comm_mat_pres <- event_pres[,16:215]
   comm_mat_pres <- as.data.frame(sapply(comm_mat_pres, as.numeric))
    #abundance
   comm_mat_abun <- event_abun[,17:216]
   comm_mat_abun <- as.data.frame(sapply(comm_mat_abun, as.numeric))
    #biomass
   comm_mat_bio <- event_bio[,3:202]
   comm_mat_bio <- as.data.frame(sapply(comm_mat_bio, as.numeric))
    
  #summing the columns in the community matrix. This tells us how many events observed a species in a raster
    #pres/ab
   rastercolsum_pres <- colSums(comm_mat_pres)
    #abundance
   rastercolsum_abun <- colSums(comm_mat_abun)
    #biomass
   rastercolsum_bio <- colSums(comm_mat_bio)
  
  #changing col sum to presence/absence #only for pres
  row <- ifelse(rastercolsum_pres > 0 , 1, 0)
  
  #adding each run with a unique ID to a matrix using rbind
    #pres
  rastercom_mat_pres <- rbind(rastercom_mat_pres, row)
    #abundance
  rastercom_mat_abun <- rbind(rastercom_mat_abun, rastercolsum_abun)
    #biomass
  rastercom_mat_bio <- rbind(rastercom_mat_bio, rastercolsum_bio)
}

#turning output into a data frame and adding column with IDs back
  #pres
rastercom_mat_pres <- as.data.frame(rastercom_mat_pres)
rastercom_mat_pres <- as.data.frame(cbind(uniqueID, rastercom_mat_pres))

  #abundance
rastercom_mat_abun <- as.data.frame(rastercom_mat_abun)
rastercom_mat_abun <- as.data.frame(cbind(uniqueID, rastercom_mat_abun))

  #biomass
rastercom_mat_bio <- as.data.frame(rastercom_mat_bio)
rastercom_mat_bio <- as.data.frame(cbind(uniqueID, rastercom_mat_bio))







#### CALC NEW S FROM rastercom_mat and rastercom_mat_abun ####
  #asymptote from rastercom_mat_abun rarecurve() = row sum of rastercom_mat
  
  #calc new S
newS <- rowSums(rastercom_mat[,-1])
ID <- rastercom_mat$uniqueID
ID_newS <- cbind.data.frame(ID, newS)
#write.csv(ID_newS, "~/fish_stability/data/ID_newS.csv")


  #pull ID with all yrcat: goodID list from DSR_analysis script
IDlist <- c(1496, 1554, 1610, 1786, 1842, 1844, 1846, 1902, 1906, 1960, 2020,
            2080, 2137, 2138, 2196, 2255, 2313, 2314, 2372, 2373, 2431, 2432,
            2550, 2609, 2610, 2669, 2729, 2788, 2789, 2909, 3029, 3089, 3150,
            3210, 3271, 3331)

  #adding a column of just raster ID
ID_newS$raster <- substr(ID_newS$ID, start=1, stop=4)

  #pulling out rows with the above listed raster IDs
ID_newS_sub <- ID_newS[ID_newS$raster %in% IDlist,]

  #save
#write.csv(ID_newS_sub, "~/fish_stability/data/ID_newS_sub.csv")





    
    


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






