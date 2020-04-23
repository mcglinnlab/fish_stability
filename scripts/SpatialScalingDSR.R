library(dplyr)
library(tidyr)
library(vegan)
library(raster)
library(sp)
library(reshape2)

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
  # load s_rarefac, s_spreadd, s_bio_comm, and ID.df
    #s_rarefac for pres/ab
s_rarefac$ID <- ID.df$point2resID
s_rarefac$yrcat <- ID.df$yrcat
s_rarefac$ID_yrcat <- paste(s_rarefac$ID, "_", s_rarefac$yrcat)

    #s_spread for abundance
s_spreadd$ID <- ID.df$point2resID
s_spreadd$yrcat <- ID.df$yrcat
s_spreadd$ID_yrcat <- paste(s_spreadd$ID, "_", s_spread$yrcat)

s_spreadd[is.na(s_spreadd)] <- 0

    #s_bio_comm for biomass
s_bio_comm <- arrange(s_bio_comm, EVENTNAME)

s_bio_comm$ID <- ID.df$point2resID
s_bio_comm$yrcat <- ID.df$yrcat
s_bio_comm$ID_yrcat <- paste(s_bio_comm$ID, "_", s_bio_comm$yrcat)



  #pulling out 36 predetermined good IDs where we know theres at least 5 events in each time bin
IDlist <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
            1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
            2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
            2670, 2721, 2771)

    #good ID for pres/ab data set s_rarefac
s_rarefac_sub <- s_rarefac[s_rarefac$ID %in% IDlist, ]

    #good ID for abundance data set
s_spread_sub <- s_spreadd[s_spreadd$ID %in% IDlist, ]

    #good ID for biomass data set
s_bio_comm_sub <- s_bio_comm[s_bio_comm$ID %in% IDlist, ]


#list of unique raster IDs through time #this works for all three matrices
uniqueID <- unique(s_bio_comm_sub$ID_yrcat)

## for loop to calculate community matrix at the raster scale with presence/absence ##
rastercom_mat_pres <- NULL
rastercom_mat_abun <- NULL
rastercom_mat_bio <- NULL

for(i in uniqueID) {
  #pulling events from each unique ID through time
    #pres/ab
 # IDpull_pres <- subset(s_rarefac_sub, s_rarefac_sub$ID_yrcat == i)
    #abundance
  #IDpull_abun <- subset(s_spread_sub, s_spread_sub$ID_yrcat == i)
    #biomass
  IDpull_bio <- subset(s_bio_comm_sub, s_bio_comm_sub$ID_yrcat == i)
  
  #determining which events will be pulled
  samp <- sample(nrow(IDpull_pres), 5, replace = F)
  
  #pulling 5 events from each raster
    #pres/ab
  #event_pres <- IDpull_pres[samp, ]
    #abundance
  #event_abun <- IDpull_abun[samp, ]
    #biomass
  event_bio <- IDpull_bio[samp, ]
  
  #removing environmental columns leaving only comm matrix
  
  
    #pres/ab
   #comm_mat_pres <- event_pres[,16:215]
   #comm_mat_pres <- as.data.frame(sapply(comm_mat_pres, as.numeric))
    #abundance
   #comm_mat_abun <- event_abun[,17:216]
   #comm_mat_abun <- as.data.frame(sapply(comm_mat_abun, as.numeric))
    #biomass
   comm_mat_bio <- event_bio[,3:202]
   comm_mat_bio <- as.data.frame(sapply(comm_mat_bio, as.numeric))
    
  #summing the columns in the community matrix. This tells us how many events observed a species in a raster
    #pres/ab
   #rastercolsum_pres <- colSums(comm_mat_pres)
    #abundance
  # rastercolsum_abun <- colSums(comm_mat_abun)
    #biomass
   rastercolsum_bio <- colSums(comm_mat_bio)
  
  #changing col sum to presence/absence #only for pres
  #row <- ifelse(rastercolsum_pres > 0 , 1, 0)
  
  #adding each run with a unique ID to a matrix using rbind
    #pres
  #rastercom_mat_pres <- rbind(rastercom_mat_pres, row)
    #abundance
  #rastercom_mat_abun <- rbind(rastercom_mat_abun, rastercolsum_abun)
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
IDlist <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
            1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
            2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
            2670, 2721, 2771)

  #adding a column of just raster ID
ID_newS$raster <- substr(ID_newS$ID, start=1, stop=4)

  #pulling out rows with the above listed raster IDs
ID_newS_sub <- ID_newS[ID_newS$raster %in% IDlist,]

  #save
#write.csv(ID_newS_sub, "~/fish_stability/data/ID_newS_sub.csv")




#### SCALING ####

  #input files 
    #comm matrix of species pres/ab, biomass, and abundance where each row is raster and time bin
    #file names rastercom_mat_pres, rastercom_mat_bio, rastercom_mat_abun
      #load from dataset file and then run following transformation. Do not resave changes. 

#an ID order will be created each run. ID list sequence will determine the order in which 
  #rasters are added to geographic extent


#ORDER OF RASTER IDS
  #coordinates of raster cell centroid
coordcenter <-as.matrix(xyFromCell(oc_raster, IDlist))

  #pairwise distances between each raster cell
raster_dist <- as.data.frame(spDists(coordcenter, coordcenter))
colnames(raster_dist) <- IDlist
rownames(raster_dist) <- IDlist



#using converting lat long to distances function
sphere_dist = function(coords){
  long = coords[ , 1]
  lat = coords[ , 2]
  # Convert degrees to radians
  deg2rad = function(deg) return(deg * pi / 180)
  delta_long = as.matrix(dist(as.matrix(deg2rad(long))))
  delta_lat = as.matrix(dist(as.matrix(deg2rad(lat))))
  hav = sin(delta_lat / 2)^2 + cos(lat) %*% t(cos(lat)) * sin(delta_long / 2)^2
  dist = 2 * asin(sqrt(abs(hav)))
  return(dist)
}


# Compute distance on sphere if xy are longitudes and latitudes
# Assume x is longitude and y is latitude
  pair_dist = sphere_dist(coordcenter)
  
  #each run of loop creates new order IDs should be pooled by nearest neighbor dist. 
  n = 36    
for (i in 1:n) {
  dist_to_site = pair_dist[i, ]
# Shuffle plots, so that tied grouping is not biased by original order.
  new_order = sample(1:n)  
  dist_new = dist_to_site[new_order]
  new_order = new_order[order(dist_new)]
# Move focal site to the front
  new_order = c(i, new_order[new_order != i])
      }



  
# GEOGRAPHIC MERGE: OUTPUT = COMM MAT PRES, BIO AT SCALE AND TIME BIN #

  #only imput bio but pres created within loop  
#seperating unique ID col into ID and yr_cat
  #bio
  rastercom_mat_bio <- rastercom_mat_bio %>%
    separate(uniqueID, c("ID", "yr_cat"))
  
  
#null objects
  bio_pull <- NULL
  bio_sub <- NULL
  ID <- NULL
  yr_cat <- NULL
  scale_pres <- NULL
  temp_pres <- NULL
  scalecom_mat_pres <- NULL
  scale_bio <- NULL
  temp_bio <- NULL
  scalecom_mat_bio <- NULL
 
  #loop that adds data from one raster at a time
for (i in IDlist) { 
#subsetting rows
  #pulling ID from bio
    bio_pull <- rastercom_mat_bio[rastercom_mat_bio$ID == i, ]
      #adding new ID pull rows to new df
    bio_sub <- rbind(bio_sub, bio_pull)
    
#geographic merge. merge by same time bin. 
  for (z in c("a", "b", "c", "d", "e", "f", "g", "h", "i")) {    #letters[1:9]
    
     #columns for ID and yrcat
    ID <- i
    yr_cat <- z
    
    #merging S
      #calc
    scale_pres <- bio_sub[bio_sub$yr_cat == z, ]
    scale_press <- t(colSums(scale_pres[, 3:202])) #-(1:2)
    scale_presss <- ifelse(scale_press > 0, 1, 0)
      #store
    temp_pres <- data.frame(cbind(ID, yr_cat, scale_presss))
    scalecom_mat_pres <- rbind(scalecom_mat_pres, temp_pres)
    
    #merging biomass
      #calc
    scale_bio <- bio_sub[bio_sub$yr_cat == z, ] 
    scale_bio <- t(colSums(scale_bio[, 3:202]))
      #store
    temp_bio <- data.frame(cbind(ID, yr_cat, scale_bio))
    scalecom_mat_bio <- rbind(scalecom_mat_bio, temp_bio)
    
  }
}

#creating scale vector   
scale <- rep(1:36, each=9)


## GEOGRAPHIC MERGE OUTPUT ##
#merging scale vector and loop output
scalecom_mat_pres <- as.data.frame(cbind(scale, scalecom_mat_pres))
scalecom_mat_bio <- as.data.frame(cbind(scale, scalecom_mat_bio))

write.csv(scalecom_mat_pres, "~/fish_stability/data/scalecom_mat_pres.csv")
write.csv(scalecom_mat_bio, "~/fish_stability/data/scalecom_mat_bio.csv")


# TEMPORAL MERGE: OUTPUT = df WHERE EACH ROW IS SCALE WITH COL FOR S, BIO,
  #ABUN, VAR S, VAR BIO #  
  #calcs made from geographic merge output

#null objects
  scale_sub_pres <- NULL
  scale_sub_bio <- NULL
  S <- NULL
  varS <- NULL
  bio <- NULL
  varbio <- NULL
  scale <- NULL
  tempscale_output <- NULL
  scale_output <- NULL
  
  #1:36 bc 36 raster so highest scale
for (i in 1:36) {
  
#subsetting rows for each scale
  #pres
  scale_sub_pres <- scalecom_mat_pres[scalecom_mat_pres$scale == i, 4:203]
  scale_sub_pres <- data.frame(sapply(scale_sub_pres, function(x) as.numeric(as.character(x))))
  #bio
  scale_sub_bio <- scalecom_mat_bio[scalecom_mat_bio$scale == i, 4:203]
  scale_sub_bio <- data.frame(sapply(scale_sub_bio, function(x) as.numeric(as.character(x))))

  
#calculations
  #pres
  S <- mean(rowSums(scale_sub_pres))
  varS <- sd(rowSums(scale_sub_pres))
  #bio
  bio <- mean(rowSums(scale_sub_bio))
  varbio <- var(rowSums(scale_sub_bio)) / (mean(rowSums(scale_sub_bio)) ^ 2)

#storage
  scale <- i 
  tempscale_output <- data.frame(scale, S, varS, bio, varbio) 
  colnames(tempscale_output) <- c("scale", "S", "varS", "bio", "varbio")
  scale_output <- rbind(scale_output, tempscale_output)
}


scale_output <- as.data.frame(scale_output)

write.csv(scale_output, "~/fish_stability/data/scale_output.csv")  #. instead ~/fish_stability
  

#adding column for stability
#scale_output$stability <- 1/scale_output$varbio
  
#a few quick graphs with scale_output 
  #bio ~ S 
with(scale_output, plot(bio ~ S))
  #S ~ scale
with(scale_output, plot(S ~ scale))
  #varbio ~ bio
with(scale_output, plot(varbio ~ bio))
  #varbio ~ S
with(scale_output, plot(varbio ~ S))
  #varbio ~ scale
with(scale_output, plot(varbio ~ scale))
  #varbio ~ varS 
with(scale_output, plot(varbio ~ varS))
  #stability ~ S
with(scale_output, plot(stability ~ S))
  #stability ~ scale
with(scale_output, plot(stability ~ scale))


  
##random
  

#examples of reordering data set
#rows <- sample(nrow(ab))
#abnew <- ab[rows, ]



  
  #### from mobr ####
  x = rastercom_mat_bio
  rownames(x) <- c(1:324)
  
  #making it a 
  x = (x > 0) * 1             
  # all sites are counted as samples even empty ones
  n = nrow(x) 
  x = colSums(x)
  
  explicit_loop = matrix(0, n, n)
  
  # Compute distance on sphere if xy are longitudes and latitudes
  # Assume x is longitude and y is latitude
  pair_dist = sphere_dist(coordcenter)
  
  n = 36    
  
  for (i in 1:n) {
    dist_to_site = pair_dist[i, ]
    # Shuffle plots, so that tied grouping is not biased by original order.
    new_order = sample(1:n)  
    dist_new = dist_to_site[new_order]
    new_order = new_order[order(dist_new)]
    # Move focal site to the front
    new_order = c(i, new_order[new_order != i])
  }
  
  ## to use, needs to be one row per site but we have nine rows per site. 
  #if bio com mat is sorted by rasters then each distance needs to be repeated 
  #in a sequence 9 times to get 324 rows and complete next step. 
  comm_ordered = x[new_order, ]
  # 1 for absence, 0 for presence
  comm_bool = as.data.frame((comm_ordered == 0) * 1) 
  rich = cumprod(comm_bool)
  explicit_loop[ , i] = as.numeric(ncol(x) - rowSums(rich))
  
  out = apply(explicit_loop, 1, mean)[effort]
  
