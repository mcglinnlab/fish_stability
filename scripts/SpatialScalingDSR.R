library(dplyr)
library(tidyr)
library(vegan)
library(raster)
library(sp)
library(reshape2)



####### PREP DATA SET
  #LOAD DATA #    
s_bio_comm <- read.csv(./fish_stability/data/s_bio_comm.csv, header =  T)
s_bio_comm <- s_bio_comm[ , -1]

ID.df <- read.csv(./fish_stability/data/ID.df.csv, header =  T)
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
  new_order = sample(1:n)  
  dist_new = dist_to_site[new_order]
  new_order = new_order[order(dist_new)]
  # Move focal site to the front
  new_order = c(i, new_order[new_order != i])
  IDorder <- rbind(IDorder, new_order)
}

IDorder <- data.frame(IDorder)


####### BEGIN PERMUTATION FOR LOOP ########

### for loop to calculate community matrix at the raster scale with bio 5 event pull ##
rastercom_mat_bio <- NULL

for(a in uniqueID) {
    #biomass
  IDpull_bio <- subset(s_bio_comm_sub, s_bio_comm_sub$ID_yrcat == a)
  
  #determining which events will be pulled
  samp <- sample(nrow(IDpull_pres), 5, replace = F)
  
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
  scalecom_pres_out <- NULL
  scalecom_bio_out <- NULL
  
for (x in 1:36) {
  #pulling one row from IDorder to use as seq for next loop 
  IDseq <- IDorder[x, ]
  
  #loop that adds data from one raster at a time
  for (y in IDseq) { 
    #subsetting rows
      #pulling ID from bio
    bio_pull <- rastercom_mat_bio[rastercom_mat_bio$ID == y, ]
      #adding new ID pull rows to new df
    bio_sub <- rbind(bio_sub, bio_pull)
    
#geographic merge. merge by same time bin. 
    for (z in letters[1:9]) {
    
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
  startID <- rep.int(IDseq[1], times = 324)
  
  temp_b <- data.frame(cbind(startID, scalecom_mat_bio))
  scalecom_bio_out <- rbind(scalecom_bio_out, temp_b)
  
  temp_p <- data.frame(cbind(startID, scalecom_mat_bio))
  scalecom_pres_out <- rbind(scalecom_pres_out, temp_p)
}

## GEOGRAPHIC MERGE OUTPUT ##
  #creating scale vector   
scale <- rep(1:36, each=324)

  #merging scale vector and loop output
scalecom_pres_out <- as.data.frame(cbind(scale, scalecom_mat_pres))
scalecom_bio_out <- as.data.frame(cbind(scale, scalecom_mat_bio))

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

#add loop that subsets by start ID when merging temporally  
for (g in IDlist) {
  scale_sub_pres <- scalecom_pres_out[scalecom_pres_out$startID == g, ]
  scale_sub_bio <- scalecom_bio_out[scalecom_bio_out$startID == g, ]
  
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
  startID <- rep.int(g, times = 36)
  out.temp <- data.frame(startID, scale_output)
  out <- rbind(out, out.temp)
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