library(dplyr)
library(tidyr)
library(vegan)
library(raster)
library(sp)
library(reshape2)
library(svMisc)



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
oc_raster <- oceans_raster
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


#so far did it by hand but this for loop should work too, this working funky
#just loaded data frame from files for now
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



#start bootstrap loop - pulls new comm matrix of 5 random trawls and completes
  #geographic merge before closing. 

#null objects
scalecom_mat_pres <- NULL
scalecom_mat_bio <- NULL


for (f in 56:59) {  
  progress(f)
  
  ### for loop to calculate community matrix at the raster scale with 
    #bio 5 event pull 
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
    ID <- NULL
    yr_cat <- NULL
    scale_pres <- NULL
    temp_pres <- NULL
    scale_bio <- NULL
    temp_bio <- NULL
 
  
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
        boot <- f
      #merging S
        #calc
        scale_pres <- bio_sub[bio_sub$yr_cat == z, ]
        scale_press <- t(colSums(scale_pres[, 3:202])) #-(1:2)
        scale_presss <- ifelse(scale_press > 0, 1, 0)
        #store
        temp_pres <- data.frame(cbind(boot, startID, scale, ID, yr_cat,
                                      scale_presss))
        scalecom_mat_pres <- rbind(scalecom_mat_pres, temp_pres)
    
      #merging biomass
        #calc
        scale_bio <- bio_sub[bio_sub$yr_cat == z, ] 
        scale_bio <- t(colSums(scale_bio[, 3:202]))
        #store
        temp_bio <- data.frame(cbind(boot, startID, scale, ID, yr_cat, scale_bio))
        scalecom_mat_bio <- rbind(scalecom_mat_bio, temp_bio)
    
      }
    }
  }
}
  ## GEOGRAPHIC MERGE OUTPUT ##

    #36 start ID with 36 scales within each and 9 time bins per scale = 11,664 rows

    #write.csv(scalecom_mat_pres, "~/fish_stability/data/scalecom_mat_pres.csv")
    #write.csv(scalecom_mat_bio, "~/fish_stability/data/scalecom_mat_bio.csv")



# TEMPORAL MERGE: OUTPUT = df WHERE EACH ROW IS SCALE WITH COL FOR S, BIO,
#ABUN, VAR S, VAR BIO #  

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

#subsetting for bootstrap iteration
for (h in 1:20) {
  progress(h)
  
  boot_sub_pres <- scalecom_mat_pres[scalecom_mat_pres$boot == h, ]
  boot_sub_bio <- scalecom_mat_bio[scalecom_mat_bio$boot == h, ]
  
  #loop that subsets by start ID when merging temporally  
  for (g in IDlist) {
    scale_sub_pres <- boot_sub_pres[boot_sub_pres$startID == g, ]
    scale_sub_bio <- boot_sub_bio[boot_sub_bio$startID == g, ]
  
    #1:36 bc 36 raster so highest scale
    for (i in 1:36) {
  
    #subsetting rows for each scale
      #pres
      scale_sub_pres2 <- scale_sub_pres[scale_sub_pres$scale == i, -c(1:5)]
      scale_sub_pres2 <- data.frame(sapply(scale_sub_pres2, function(x) as.numeric(as.character(x))))
      #bio
      scale_sub_bio2 <- scale_sub_bio[scale_sub_bio$scale == i, -c(1:5)]
      scale_sub_bio2 <- data.frame(sapply(scale_sub_bio2, function(x) as.numeric(as.character(x))))

  
    #calculations
      #pres
      S <- mean(rowSums(scale_sub_pres2))
      varS <- sd(rowSums(scale_sub_pres2))
      #bio
      bio <- mean(rowSums(scale_sub_bio2))
      varbio <- var(rowSums(scale_sub_bio2)) / (mean(rowSums(scale_sub_bio2)) ^ 2)
      stability <- 1/varbio

    #storage
      scale <- i 
      startID <- g
      boot <- h
      tempscale_output <- data.frame(boot, startID, scale, S, varS, bio, varbio) 
      colnames(tempscale_output) <- c("boot", "startID", "scale", "S", "varS", 
                                      "bio", "varbio")
      scale_output <- rbind(scale_output, tempscale_output)
    }
  }
}
  
#write.csv(scale_output, "~/fish_stability/data/scale_output.csv")  #. instead ~/fish_stability
  