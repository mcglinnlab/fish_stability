#List of Data Sets and what they mean

#SEAMAP_sub 
  #full data set with unneeded columns removed
SEAMAP_sub <- read.csv("~./fish_stability/data/SEAMAP_sub.csv", header = T)
SEAMAP_sub <- SEAMAP_sub[,-1]

#SEAMAP_inner
  #only includes inner depth strata
SEAMAP_inner <- read.csv("~./fish_stability/data/SEAMAP_inner.csv", header = T)
SEAMAP_inner <- SEAMAP_inner[,-1]

#s_rarefac
  #has environmental matrix and community matrix with presence/ absence
s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)
s_rarefac <- s_rarefac[,-1]

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

#resultsfullpoint2
  #after permutation. each row is raster region in a single year
resultsfullpoint2 <- read.csv("~./fish_stability/data/resultsfullpoint2.csv",
         header = T)
resultsfullpoint2 <- resultsfullpoint2[,-1]

#yrag_resultsfull
  # after permutation. each row is raster region over three years
yrag_resultsfull <- read.csv("~./fish_stability/data/yrag_resultsfull.csv", 
                             header = T)
yrag_resultsfull <- yrag_resultsfull[,-1]

#yrag_sub
  #after permuation. each row is raster region over three years when region has
  # at least five trawls over every three year bin
yrag_sub <- read.csv("~./fish_stability/data/yrag_sub.csv", header = T)
yrag_sub <- yrag_sub[,-1]
