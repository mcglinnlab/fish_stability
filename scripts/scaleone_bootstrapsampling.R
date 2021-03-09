
library(mobr)

#Input: s_bio_sub, s_ind_sub s_shrimp_sub and s_flounder_sub from SpatialScalingDSR

##sampling loop ##
for (f in 1) {  
  
  ### for loop to calculate community matrix at the raster scale with 
  #bio 5 event pull 
  raster_bio <- NULL
  raster_ind <- NULL
  raster_shrimp <- NULL
  raster_flounder <- NULL
  
  for(a in uniqueID) {
  #subsetting each unique ID
    #biomass
    IDpull_bio <- subset(s_bio_sub, s_bio_sub$ID_yrcat == a)
    #number of individuals
    IDpull_ind <- subset(s_ind_sub, s_ind_sub$ID_yrcat == a)
    #shrimp biomass
    IDpull_bio_shrimp <- subset(s_bio_shrimp_sub, s_bio_shrimp_sub$ID_yrcat == a)
    #flounder biomass 
    IDpull_bio_flounder <- subset(s_bio_flounder_sub, s_bio_flounder_sub$ID_yrcat == a)
    
    #determining which events will be pulled
    samp <- sample(nrow(IDpull_bio), 5, replace = F)
    
    #pulling 5 events from each raster
    #biomass
    event_bio <- IDpull_bio[samp, ]
    #number of individuals
    event_ind <- IDpull_ind[samp, ]
    #shrimp
    event_shrimp <- IDpull_bio_shrimp[samp, ]
    #flounder
    event_flounder <- IDpull_bio_flounder[samp, ]
    
    #removing environmental columns leaving only comm matrix
    #biomass
    comm_bio <- event_bio[,15:211]
    comm_bio <- as.data.frame(sapply(comm_bio, as.numeric))
    #number of individuals
    comm_ind <- event_ind[,15:211]
    comm_ind <- as.data.frame(sapply(comm_ind, as.numeric))
    #shrimp
    comm_shrimp <- event_shrimp[, 15:17]
    comm_shrimp <- as.data.frame(sapply(comm_shrimp, as.numeric))
    #flounder
    comm_flounder <- event_flounder[, 15:17]
    comm_flounder <- as.data.frame(sapply(comm_flounder, as.numeric))
    
    
  #summing the columns in the community matrix. This tells us total biomass 
  #for each species at 5 event rarefaction 
    #biomass
    rastercolsum_bio <- colSums(comm_bio)
    #number of individuals
    rastercolsum_ind <- colSums(comm_ind)
    #shrimp
    rastercolsum_shrimp <- colSums(comm_shrimp)
    #flounder 
    rastercolsum_flounder <- colSums(comm_flounder)
  
  #adding each run with a unique ID to a matrix using rbind
    #biomass
    raster_bio <- rbind(raster_bio, rastercolsum_bio)
    #number of individuals
    raster_ind <- rbind(raster_ind, rastercolsum_ind)
    #shrimp
    raster_shrimp <- rbind(raster_shrimp, rastercolsum_shrimp)
    #flounder
    raster_flounder <- rbind(raster_flounder, rastercolsum_flounder)
  }
  
#turning output into a data frame and adding column with IDs back
  
  #logging the iteration number
  boot <- f
  
  #biomass
  raster_bio <- as.data.frame(raster_bio)
  raster_bio <- as.data.frame(cbind(uniqueID, boot, raster_bio))
  #number of individuals
  raster_ind <- as.data.frame(raster_ind)
  raster_ind <- as.data.frame(cbind(uniqueID, boot, raster_ind))
  #shrimp
  raster_shrimp <- as.data.frame(raster_shrimp)
  raster_shrimp <- as.data.frame(cbind(uniqueID, boot, raster_shrimp))
  #flounder
  raster_flounder <- as.data.frame(raster_flounder)
  raster_flounder <- as.data.frame(cbind(uniqueID, boot, raster_flounder))
}


## summary loop ##

temp <- NULL
summary_BEF <- NULL

  #for number of iterations
for (b in 1) {
  #subsetting based on each bootstrap iteration
  boot_bio <- subset(raster_bio, raster_bio$boot == b)
  boot_ind <- subset(raster_ind, raster_ind$boot == b)
  boot_shrimp <- subset(raster_shrimp, raster_shrimp$boot == b)
  boot_flounder <- subset(raster_flounder, raster_flounder$boot == b)
  
  for (c in uniqueID) {
    #pull row for each unique ID 
    raster_bio_sub <- subset(boot_bio, boot_bio$uniqueID == c)
    raster_ind_sub <- subset(boot_ind, boot_ind$uniqueID == c)
    raster_shrimp_sub <- subset(boot_shrimp, boot_shrimp$uniqueID == c)
    raster_flounder_sub <- subset(boot_flounder, boot_flounder$uniqueID == c)
    
    #boot 
    boot <- b
    
    #uniqueID 
    unique_ID <- c
    
    #calc biomass
    biomass <- rowSums(raster_bio_sub[, -(1:2)])
    
    #calc shrimp biomass
    shrimp_bio <- rowSums(raster_shrimp_sub[, -(1:2)])
    
    #calc flounder biomass 
    flounder_bio <- rowSums(raster_flounder_sub[, -(1:2)])
    
    #calcS 
    raster_pres_sub <- raster_bio_sub
    raster_pres_sub[raster_pres_sub > 0] <- 1
    S <- rowSums(raster_pres_sub[, -(1:2)])
    
    #calcSpie
    sPIE <- calc_PIE(raster_ind_sub[, -(1:2)], ENS = T)
    
    #rarefaction
    s_N <- rarefaction(raster_ind_sub[, -(1:2)], method = "IBR", effort = 100) 
    
    #number of individuals
    Nind <- rowSums(raster_ind_sub[, -(1:2)])
    #storage
      #temp
    temp <- data.frame(cbind(unique_ID, boot, S, sPIE, s_N, Nind, biomass, shrimp_bio, flounder_bio))
    summary_BEF <- rbind(summary_BEF, temp)
  }
}

summary_BEF[, 3:9] <- as.data.frame(sapply(summary_BEF[, 3:9], as.numeric))



#quick graphs
with(summary_BEF, plot(biomass ~ S))
with(summary_BEF, plot(flounder_bio ~ S))
with(summary_BEF, plot(shrimp_bio ~ S))

with(summary_BEF, plot(log(shrimp_bio) ~ log(S)))
shrimp_lm <- lm(log(summary_BEF$biomass) ~ summary_BEF$S)
with(summary_BEF, plot(log(flounder_bio) ~ log(S)))
with(summary_BEF, plot(log(biomass) ~ log(S)))

with(summary_BEF, plot(log(biomass) ~ log(sPIE)))
with(summary_BEF, plot(log(shrimp_bio) ~ sPIE))
with(summary_BEF, plot(log(flounder_bio) ~ sPIE))

with(summary_BEF, plot(log(biomass) ~ log(Nind)))

with(summary_BEF, plot(log(biomass) ~ log(s_N)))



summary(summary_BEF$Nind)




