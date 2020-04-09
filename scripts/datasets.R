#List of Data Sets and what they mean

#SEAMAP_sub 
  #full data set with unneeded columns removed
SEAMAP_sub <- read.csv("~./fish_stability/data/SEAMAP_sub.csv", header = T)
SEAMAP_sub <- SEAMAP_sub[,-1]

#SEAMAP_inner
  #only includes inner depth strata
SEAMAP_inner <- read.csv("~./fish_stability/data/SEAMAP_inner.csv", header = T)
SEAMAP_inner <- SEAMAP_inner[,-1]





#ID.df
 #data frame with the raster ID and yrcat for each event in the inner depth zone
ID.df <- read.csv("~/fish_stability/data/ID.df.csv", header = T)
ID.df <- ID.df[,-1]





#s_rarefac
  #has environmental matrix and community matrix with presence/ absence
s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)
s_rarefac <- s_rarefac[,-1]

#s_rarefac_sub
  #same as s_rarefac but subset for 36 good raster IDs
s_rarefac_sub <- read.csv("~/fish_stability/data/s_rarefac_sub.csv", header = T)
s_rarefac_sub <- s_rarefac_sub[,-1]

#s_comm
  #community matrix where rows are events and columns are species with presence
    #absence
s_comm <- read.csv("~./fish_stability/data/s_comm.csv", header = T)
s_comm <- s_comm[,-1]

#s_environ
  #environmental matrix where rows are events and columns are parameters
s_environ <- read.csv("~./fish_stability/data/s_environ.csv", header = T)
s_environ <- s_environ[,-1]

#s_spread
  #community matrix with number of individuals in cells and environmental included
s_spread <- read.csv("~./fish_stability/data/s_spread.csv", header = T)
s_spread <- s_spread[,-1]

#s_bio_comm 
  #community matrix with biomass in cells and no environmental included
s_bio_comm <- read.csv("~/fish_stability/data/s_bio_comm.csv", header = T)
s_bio_comm <- s_bio_comm[,-1]




#resultsfullpoint2
  #after permutation. each row is raster region in a single year
resultsfullpoint2 <- read.csv("~./fish_stability/data/resultsfullpoint2.csv",
         header = T)
resultsfullpoint2 <- resultsfullpoint2[,-1]

#yrag_resultsfull
  # after permutation. each row is raster region over three years
yrag_resultsfull <- read.csv("~./fish_stability/data/yrag_resultsfullpoint2.csv", 
                             header = T)
yrag_resultsfull <- yrag_resultsfull[,-1]

#yrag_sub
  #after permuation. each row is raster region over three years when region has
  # at least five trawls over every three year bin
yrag_sub <- read.csv("~./fish_stability/data/yrag_sub.csv", header = T)
yrag_sub <- yrag_sub[,-1]





#rastercom_mat
  #community matrix at the raster level. each row is cumulative species in a
  #raster region in a time bin
rastercom_mat <- read.csv("~/fish_stability/data/rastermat_com.csv", header = T)
rastercom_mat <- rastercom_mat[,-1]

#rastercom_mat_abun 
  #community matrix at the raster level. each row is abundance in a raster region
rastercom_mat_abun <- read.csv("~/fish_stability/data/rastercom_mat_abun.csv", 
                               header = T)
rastercom_mat_abun <- rastercom_mat_abun[,-1]

#rastercom_mat_bio
  #community matrix at the raster level. each row is biomass in a raster region
rastercom_mat_bio <- read.csv("~/fish_stability/data/rastercom_mat_bio.csv",
                              header = T)
rastercom_mat_bio <- rastercom_mat_bio[,-1]





#ID_newS
  #two column data frame. includes unique raster ID (raster_yrcat) and new S
  #calculated by finding asymptote of rarefaction curve for each unique ID or
  #total number of unique species found in each raster sqaure for that yr cat
ID_newS <- read.csv("~/fish_stability/data/ID_newS.csv", header = T)
ID_newS <- ID_newS[,-1]


#ID_newS_sub 
  #ID_newS subsetting for good IDs used to create yrag_sub data frame
ID_newS_sub <- read.csv("~/fish_stability/data/ID_newS_sub.csv", header = T)
ID_newS_sub <- ID_newS_sub[,-1]





 
