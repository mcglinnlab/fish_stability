library(dplyr)
library(tidyr)
library(vegan)
library(raster)
library(sp)
library(reshape2)



####### PREP DATA SET
  #LOAD DATA #    
s_bio_comm <- read.csv("~./fish_stability/data/s_bio_comm.csv", header =  T)
s_bio_comm <- s_bio_comm[ , -1]

ID.df <- read.csv("~./fish_stability/data/ID.df.csv", header =  T)
ID.df <- ID.df[, -1]

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

    #good ID for biomass data set
s_bio_comm_sub <- s_bio_comm[s_bio_comm$ID %in% IDlist, ]


#list of unique raster IDs through time #this works for all three matrices
uniqueID <- unique(s_bio_comm_sub$ID_yrcat)



##### CREATE ORDER OF RASTER IDS
#coordinates of raster cell centroid
coordcenter <-as.matrix(xyFromCell(oc_raster, IDlist))

#using converting lat long to distances function (from mobr)
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

IDorder <- NULL
n = 36    
for (i in 1:n) {
  dist_to_site = pair_dist[i, ]
  # Shuffle plots, so that tied grouping is not biased by original order.
  new_order = sample(1:n, replace = F)  
  dist_new = dist_to_site[new_order]
  new_order = new_order[order(dist_new)]
  # Move focal site to the front
  new_order = c(i, new_order[new_order != i])
  IDorder <- rbind(IDorder, new_order)
}

IDorder <- data.frame(IDorder)


#so far did it by hand but this for loop should work too
for (i in rownames(IDorder)) { 
IDorder <- IDorder %>%
  mutate(X36 = recode(X36, '1' = '1246', '2' = '1294', '3' = '1340', '4' = '1486', '5' = '1532',
                '6' = '1534', '7' = '1536', '8' = '1582', '9' = '1586', '10' = '1630',
                '11' = '1680', '12' = '1730', '13' = '1777', '14' = '1778',
                '15' = '1826','16' = '1875', '17' = '1923', '18' = '1924', 
                '19' = '1972', '20' = '1973','21' = '2021','22' = '2022',
                '23' = '2120', '24' = '2169', '25' = '2170','26' = '2219',
                '27' = '2269', '28' = '2318', '29' = '2319', '30' = '2419',
                '31' = '2519', '32' = '2569','33' = '2620', '34' = '2670', 
                '35' = '2721', '36' = '2771'))
}



####### BEGIN PERMUTATION FOR LOOP ########

### for loop to calculate community matrix at the raster scale with bio 5 event pull ##
rastercom_mat_bio <- NULL

for(a in uniqueID) {
    #biomass
  IDpull_bio <- subset(s_bio_comm_sub, s_bio_comm_sub$ID_yrcat == a)
  
  #determining which events will be pulled
  samp <- sample(nrow(IDpull_bio), 5, replace = F)
  
  #pulling 5 events from each raster
    #biomass
  event_bio <- IDpull_bio[samp, ]
  
  #removing environmental columns leaving only comm matrix
    #biomass
   comm_mat_bio <- event_bio[,3:202]
   comm_mat_bio <- as.data.frame(sapply(comm_mat_bio, as.numeric))
    
  #summing the columns in the community matrix. This tells us how many events observed a species in a raster
    #biomass
   rastercolsum_bio <- colSums(comm_mat_bio)
  
  #adding each run with a unique ID to a matrix using rbind

    #biomass
  rastercom_mat_bio <- rbind(rastercom_mat_bio, rastercolsum_bio)
}

#turning output into a data frame and adding column with IDs back

  #biomass
rastercom_mat_bio <- as.data.frame(rastercom_mat_bio)
rastercom_mat_bio <- as.data.frame(cbind(uniqueID, rastercom_mat_bio))


#### SCALING ####

  #input files 
    #comm matrix of biomass where each row is raster and time bin
    #file name rastercom_mat_bio
      #load from dataset file and then run following transformation. Do not resave changes. 

#an ID order will be created each run. ID list sequence will determine the order in which 
  #rasters are added to geographic extent

  
# GEOGRAPHIC MERGE: OUTPUT = COMM MAT PRES, BIO AT SCALE AND TIME BIN #

  #only input bio but pres created within loop  
#seperating unique ID col into ID and yr_cat
  #bio
  rastercom_mat_bio <- rastercom_mat_bio %>%
    separate(uniqueID, c("ID", "yr_cat"))
  
  
#null objects
  #bio_pull <- NULL
  #bio_sub <- NULL
  ID <- NULL
  yr_cat <- NULL
  scale_pres <- NULL
  temp_pres <- NULL
  scalecom_mat_pres <- NULL
  scale_bio <- NULL
  temp_bio <- NULL
  scalecom_mat_bio <- NULL
 
  
for (x in 1:36) {
  #pulling one row from IDorder to use as seq for next loop 
  IDseq <- as.numeric(as.vector(IDorder[x, ]))
  
  #adding null objects for next inner loop
  bio_pull <- NULL
  bio_sub <- NULL
  
  #loop that adds data from one raster at a time
  for (y in IDseq) { 
    #subsetting rows
      #pulling ID from bio
    bio_pull <- rastercom_mat_bio[rastercom_mat_bio$ID == y, ]
      #adding new ID pull rows to new df
    bio_sub <- rbind(bio_sub, bio_pull)
   
   
#geographic merge. merge by same time bin. 
    for (z in letters[1:9]) {
    
     #columns for ID, yrcat and start ID
      ID <- y
      yr_cat <- z
      startID <- IDseq[1]
      scale <- length(unique(bio_sub$ID))
    
    #merging S
      #calc
      scale_pres <- bio_sub[bio_sub$yr_cat == z, ]
      scale_press <- t(colSums(scale_pres[, 3:202])) #-(1:2)
      scale_presss <- ifelse(scale_press > 0, 1, 0)
      #store
      temp_pres <- data.frame(cbind(startID, scale, ID, yr_cat, scale_presss))
      scalecom_mat_pres <- rbind(scalecom_mat_pres, temp_pres)
    
    #merging biomass
      #calc
      scale_bio <- bio_sub[bio_sub$yr_cat == z, ] 
      scale_bio <- t(colSums(scale_bio[, 3:202]))
      #store
      temp_bio <- data.frame(cbind(startID, scale, ID, yr_cat, scale_bio))
      scalecom_mat_bio <- rbind(scalecom_mat_bio, temp_bio)
    
    }
  }
}

## GEOGRAPHIC MERGE OUTPUT ##

  #36 start ID with 36 scales within each and 9 time bins per scale = 11,664 rows

  #write.csv(scalecom_mat_pres, "~/fish_stability/data/scalecom_mat_pres.csv")
  #write.csv(scalecom_mat_bio, "~/fish_stability/data/scalecom_mat_bio.csv")


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

  ###when permutation is added another loop needed here to subset per permutation. 
    #permutation for loop only wrapped around top half. Second half only merging. 
  
#loop that subsets by start ID when merging temporally  
for (g in IDlist) {
  scale_sub_pres <- scalecom_mat_pres[scalecom_mat_pres$startID == g, ]
  scale_sub_bio <- scalecom_mat_bio[scalecom_mat_bio$startID == g, ]
  
  #1:36 bc 36 raster so highest scale
  for (i in 1:36) {
  
  #subsetting rows for each scale
    #pres
    scale_sub_pres2 <- scale_sub_pres[scale_sub_pres$scale == i, -c(1:4)]
    scale_sub_pres2 <- data.frame(sapply(scale_sub_pres2, function(x) as.numeric(as.character(x))))
    #bio
    scale_sub_bio2 <- scale_sub_bio[scale_sub_bio$scale == i, -c(1:4)]
    scale_sub_bio2 <- data.frame(sapply(scale_sub_bio2, function(x) as.numeric(as.character(x))))

  
  #calculations
    #pres
    S <- mean(rowSums(scale_sub_pres2))
    varS <- sd(rowSums(scale_sub_pres2))
    #bio
    bio <- mean(rowSums(scale_sub_bio2))
    varbio <- var(rowSums(scale_sub_bio2)) / (mean(rowSums(scale_sub_bio2)) ^ 2)

  #storage
    scale <- i 
    startID <- g
    tempscale_output <- data.frame(startID, scale, S, varS, bio, varbio) 
    colnames(tempscale_output) <- c("startID", "scale", "S", "varS", "bio", "varbio")
    scale_output <- rbind(scale_output, tempscale_output)
  }
}
  
#write.csv(scale_output, "~/fish_stability/data/scale_output.csv")  #. instead ~/fish_stability
  
  








####QUICK GRAPHS####
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


  #varbio ~ scale. fill = startID
ggplot(data = scale_output, aes(x = scale, y = varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Var Biomass") +
  theme_bw()

  #invarbio ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Invar Biomass") +
  theme_bw()

  #varbio ~ S fill = scale
ggplot(data = scale_output, aes(x = S, y = varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("S") +
  ylab("Biomass Var") +
  theme_bw()

  #invarbio ~ S fill = startID
ggplot(data = scale_output, aes(x = S, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("S") +
  ylab("Invar Biomass") +
  theme_bw()

  #bio ~ S fill = scale
ggplot(data = scale_output, aes(x = S, y = bio, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

  #bio ~ S fill = startID
ggplot(data = scale_output, aes(x = S, y = bio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

#S ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = S, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("S") +
  theme_bw()


  #bio ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = bio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Biomass") +
  theme_bw()

  #invar ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("Scale") +
  ylab(" Invar Biomass") +
  theme_bw()

  #invar ~ S fill = startID
ggplot(data = scale_output, aes(x = scale, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("S") +
  ylab("Invar Biomass") +
  theme_bw()







b <- with(scale_output, tapply(bio, list(scale), mean))
s <- with(scale_output, tapply(S, list(scale), mean))
ssd <- with(scale_output, tapply(S, list(scale), sd))
bvar <- with(scale_output, tapply(varbio, list(scale),mean))
bsd <- with(scale_output, tapply(bio, list(scale), sd))
binvar<- 1/bvar

scale <- c(1:36)

plot(b ~ s, xlab = "S", ylab = "biomass", cex = 2, lwd = 2)
plot(bvar ~ s, xlab = "S", ylab = "biomass var", cex = 2, lwd = 2)
plot(binvar ~ s, xlab = "S", ylab = "biomass invar", cex = 2, lwd = 2)

plot(b ~ scale, xlab = "scale", ylab = "biomass", cex = 2, lwd = 2)
plot(bvar ~ scale, xlab = "scale", ylab = "biomass var", cex = 2, lwd = 2 )

plot(binvar[binvar < 200] ~ scale[binvar < 200], xlab = "scale", 
     ylab = "biomass invar", cex = 2, lwd = 2)
plot(binvar[binvar < 1000] ~ scale[binvar < 1000], xlab = "scale", 
     ylab = "biomass invar", cex = 2, lwd = 2)
plot(binvar ~ scale, xlab = "scale", ylab = "biomass invar", cex = 2, lwd = 2)
plot(s ~ scale, xlab = "scale", ylab = "S", cex = 2, lwd = 2)


dat <- as.data.frame(cbind(b, s, bvar, binvar, scale))

ggplot(data = dat, aes(x = s, y = b, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

ggplot(data = dat, aes(x = s, y = binvar, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

ggplot(data = dat, aes(x = scale, y = binvar, fill = s)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Invar Biomass") +
  theme_bw()

  
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
  

  #### RARECURVES WITH PRESENCE AND ABUNDANCE DATA (ignore) ####
  
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