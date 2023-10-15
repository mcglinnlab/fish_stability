
library(mobr)

#Input: s_bio_sub, s_ind_sub s_shrimp_sub and s_flounder_sub from SpatialScalingDSR



### for loop to calculate community matrix at the raster scale with 
#bio 5 event pull 
raster_bio <- NULL
raster_ind <- NULL
raster_shrimp <- NULL
raster_flounder <- NULL
raster_environ <- NULL

##sampling loop ##
for (f in 1:1000) {  
  
  raster_bio_boot <- NULL
  raster_ind_boot <- NULL
  raster_shrimp_boot <- NULL
  raster_flounder_boot <- NULL
  raster_environ_boot <- NULL
  
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
    #environment
    IDpull_environ <- subset(event_dat_sub, event_dat_sub$ID_yrcat == a)
    
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
    #environment
    event_environ <- IDpull_environ[samp, ]
    
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
    #evnironment
    dat_environ <- event_environ[, 11:14]
    dat_environ <- as.data.frame(sapply(dat_environ, as.numeric))
    
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
    #environment
    rastercolav_environment <- colMeans(dat_environ, na.rm = T)
  
  #adding each run with a unique ID to a matrix using rbind
    #biomass
    raster_bio_boot <- rbind(raster_bio_boot, rastercolsum_bio)
    #number of individuals
    raster_ind_boot <- rbind(raster_ind_boot, rastercolsum_ind)
    #shrimp
    raster_shrimp_boot <- rbind(raster_shrimp_boot, rastercolsum_shrimp)
    #flounder
    raster_flounder_boot <- rbind(raster_flounder_boot, rastercolsum_flounder)
    #environment
    raster_environ_boot <- rbind(raster_environ_boot, rastercolav_environment)
  
    }
  
#turning output into a data frame and adding column with IDs back
  
  #logging the iteration number
  boot <- f
  
  
  #prep data storage 
  #biomass
  raster_bio_boot <- as.data.frame(raster_bio_boot)
  raster_bio_boot <- as.data.frame(cbind(uniqueID, boot, raster_bio_boot))
  #number of individuals
  raster_ind_boot <- as.data.frame(raster_ind_boot)
  raster_ind_boot <- as.data.frame(cbind(uniqueID, boot, raster_ind_boot))
  #shrimp
  raster_shrimp_boot <- as.data.frame(raster_shrimp_boot)
  raster_shrimp_boot <- as.data.frame(cbind(uniqueID, boot, raster_shrimp_boot))
  #flounder
  raster_flounder_boot <- as.data.frame(raster_flounder_boot)
  raster_flounder_boot <- as.data.frame(cbind(uniqueID, boot, raster_flounder_boot))
  #environment 
  raster_environ_boot <- as.data.frame(raster_environ_boot)
  raster_environ_boot <- as.data.frame(cbind(uniqueID, boot, raster_environ_boot))

  #biomass
  raster_bio <- as.data.frame(rbind(raster_bio, raster_bio_boot))
  #number of individuals
  raster_ind <- as.data.frame(rbind(raster_ind, raster_ind_boot))
  #shrimp
  raster_shrimp <- as.data.frame(rbind(raster_shrimp, raster_shrimp_boot))
  #flounder
  raster_flounder <- as.data.frame(rbind(raster_flounder, raster_flounder_boot))
  #environment 
  raster_environ <- as.data.frame(rbind(raster_environ, raster_environ_boot))
  
}





## summary loop ##

temp <- NULL
summary_BEF <- NULL

  #for number of iterations
for (b in 1:50) {
  
  #subsetting based on each bootstrap iteration
  boot_bio <- subset(raster_bio, raster_bio$boot == b)
  boot_ind <- subset(raster_ind, raster_ind$boot == b)
  boot_shrimp <- subset(raster_shrimp, raster_shrimp$boot == b)
  boot_flounder <- subset(raster_flounder, raster_flounder$boot == b)
  boot_environ <- subset(raster_environ, raster_environ$boot == b)
  
  for (c in uniqueID) {
    #pull row for each unique ID 
    raster_bio_sub <- subset(boot_bio, boot_bio$uniqueID == c)
    raster_ind_sub <- subset(boot_ind, boot_ind$uniqueID == c)
    raster_shrimp_sub <- subset(boot_shrimp, boot_shrimp$uniqueID == c)
    raster_flounder_sub <- subset(boot_flounder, boot_flounder$uniqueID == c)
    raster_environ_sub <- subset(boot_environ, boot_environ$uniqueID == c)
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
    s_N_100 <- rarefaction(raster_ind_sub[, -(1:2)], method = "IBR", effort = 100) 
    s_N_500 <- rarefaction(raster_ind_sub[, -(1:2)], method = "IBR", effort = 500)
    s_N_1000 <- rarefaction(raster_ind_sub[, -(1:2)], method = "IBR", effort = 1000)
    s_N_2500 <- rarefaction(raster_ind_sub[, -(1:2)], method = "IBR", effort = 2500)
    
    #number of individuals
    Nind <- rowSums(raster_ind_sub[, -(1:2)])
    
    #surface temp
    tempS <- raster_environ_sub$tempS
    #bottom temp
    tempB <- raster_environ_sub$tempB
    #surface salinity
    salS <- raster_environ_sub$salS
    #bottom salinity
    salB <- raster_environ_sub$salB
   
     #storage
      #temp
    temp <- data.frame(cbind(unique_ID, boot, S, sPIE, s_N_100, s_N_500, s_N_1000, 
        s_N_2500, Nind, biomass, shrimp_bio, flounder_bio, tempS, tempB, salS, salB))
    summary_BEF <- rbind(summary_BEF, temp)
  }
}

summary_BEF[, 3:16] <- as.data.frame(sapply(summary_BEF[, 3:16], as.numeric))

#10/15/23 - stopped loop for time sake andtruncate at 50 boot iterations temporarily 


#writing new summary_BEF with different N iterations included. Boot iterations = 250.
write.csv(summary_BEF, "./data/BEF/summary_BEF_sNiterations.csv")

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



#take a look at different s_N graphs
  #N=100
with(summary_BEF, plot(log(biomass) ~ log(s_N_100)))
with(summary_BEF, lines(lowess(log(s_N_100), log(biomass)), col = "red"))
     
  #N=500
with(summary_BEF, plot(log(biomass) ~ log(s_N_500)))
with(summary_BEF, lines(lowess(log(s_N_500), log(biomass)), col = "red"))

  #N=1000
with(summary_BEF, plot(log(biomass) ~ log(s_N_1000)))
with(summary_BEF, lines(lowess(log(s_N_1000), log(biomass)), col = "red"))

  #N=2500
with(summary_BEF, plot(log(biomass) ~ log(s_N_2500)))
with(summary_BEF, lines(lowess(log(s_N_2500), log(biomass)), col = "red"))


#averaging across bootstraps
b_av <- with(summary_BEF, tapply(biomass, list(unique_ID), mean))
sN100_av <- with(summary_BEF, tapply(s_N_100, list(unique_ID), mean))
sN500_av <- with(summary_BEF, tapply(s_N_500, list(unique_ID), mean))
sN1000_av <- with(summary_BEF, tapply(s_N_1000, list(unique_ID), mean))
sN2500_av <- with(summary_BEF, tapply(s_N_2500, list(unique_ID), mean))

#plots averaged across bootstraps
plot(log(b_av) ~ log(sN100_av))
lines(lowess(log(sN100_av), log(b_av)), col = "red")

plot(log(b_av) ~ log(sN500_av))
lines(lowess(log(sN500_av), log(b_av)), col = "red")

plot(log(b_av) ~ log(sN1000_av))
lines(lowess(log(sN1000_av), log(b_av)), col = "red")

plot(log(b_av) ~ log(sN2500_av))
lines(lowess(log(sN2500_av), log(b_av)), col = "red")



