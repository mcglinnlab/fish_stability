
#### Initial Data Processing ####

#import fish species csv

fish_species = read.csv('./fish_species.csv', header=FALSE, colClasses='character')
names(fish_species) = 'species'

#import SEAMAP data

SEAMAP<-read.csv("~./fish_stability/data/SEAMAPData.csv", header = T)



##determining number of unique nets (collection number) for each trawl (event name)
n = NULL
uni_event = unique(SEAMAP$EVENTNAME)
for (i in 1:length(uni_event)) {
  SEAMAP_sub = subset(SEAMAP, EVENTNAME == uni_event[i])
  n[i] = length(unique(SEAMAP_sub$COLLECTIONNUMBER))
}
n
sum(n)
table(n)
uni_event[which(n == 0)]

# run check that only one date applies to each event name
out = NULL
for (i in 1:length(SEAMAP$EVENTNAME)){
  uni_dates = unique(SEAMAP$DATE[SEAMAP$EVENTNAME == SEAMAP$EVENTNAME[i]])
  out[i] = length(uni_dates)
}

sum(out!=2)


##THIS PART REMOVES THE =\" FROM ENTRY NAMES"
SEAMAP = as.data.frame(SEAMAP)
for (i in 1:ncol(SEAMAP)) {
  SEAMAP[ , i] = sub('=', '', SEAMAP[ , i])
}


#finding unique species names
uni_sp = unique(SEAMAP$SPECIESCOMMONNAME)
# spcies in the sp list
gd_common_names = uni_sp[uni_sp %in% fish_species$species]
# species not in the sp list
uni_sp[!(uni_sp %in% fish_species$species)]

gd_sci_names = unique(SEAMAP$SPECIESSCIENTIFICNAME[SEAMAP$SPECIESCOMMONNAME
                                                   %in% gd_common_names])

#write.csv(gd_sci_names, file='./gd_sci_names.csv', row.names=F)

#output names that we have filtered out of the dataset
#uni_sci_sp = unique(SEAMAP$SPECIESSCIENTIFICNAME)
#write.csv(uni_sci_sp[!(uni_sci_sp %in% gd_sci_names)],
  #'./sp_names_filtered_out.csv', row.names=F)


#manually enter in small subset of species that lack common names
gd_sci_names = read.csv('./gd_sci_names.csv')
names(gd_sci_names) = 'species'

SEAMAP_sub = subset(SEAMAP, SEAMAP$SPECIESSCIENTIFICNAME
                    %in% gd_sci_names$species)


#Merge species names that were inconsistently used scientific names
anchoa_rows_sci = grep('ANCHOA', SEAMAP_sub$SPECIESSCIENTIFICNAME)
SEAMAP_sub$SPECIESSCIENTIFICNAME[anchoa_rows_sci] = 'ANCHOA'

#Merge species names that were inconsistently used common names
anchoa_rows_com = grep('ANCHOVY', SEAMAP_sub$SPECIESCOMMONNAME)
SEAMAP_sub$SPECIESCOMMONNAME[anchoa_rows_com] = 'ANCHOA SPP'
anchoa_rows_com = grep('ANCHOVIES', SEAMAP_sub$SPECIESCOMMONNAME)
SEAMAP_sub$SPECIESCOMMONNAME[anchoa_rows_com] = 'ANCHOA SPP'



#find and remove rows with nonlogical coordinates
apply(SEAMAP_sub[,c("LONGITUDESTART","LATITUDESTART", "COLLECTIONNUMBER")],2, summary)
SEAMAP_sub <- subset(SEAMAP_sub, LONGITUDESTART < -90 & LATITUDESTART < 90)


#Export SEAMAP_sub to csv
#write.csv(SEAMAP_sub, file = "./data/SEAMAP_sub.csv")


#### REMOVING DEEP SITE, ADDING RASTER ID, CREATING COMMUNITY MATRICES ####
#read in SEAMAP_sub
SEAMAP_sub <-read.csv("~./fish_stability/data/SEAMAP_sub.csv", header = T)




#subsetting for inner depth zone only, removing samples from 30-60 ft
#SEAMAP_inner <- subset(SEAMAP_sub, DEPTHZONE == "INNER")

#EACH ROW EVENTNAME AND WIDE FORM ##
## creating new data frame where row is unique event and total number of each 
  #species are in wide form ##

library(dplyr)
library(tidyr)
library(tidyselect)
library(purrr)

#selecting only needed columns 

SEAMAP_invest <- SEAMAP_sub[,c("DATE", "Year", "LONGITUDESTART",
                                 "LATITUDESTART", "COLLECTIONNUMBER", 
                                 "EVENTNAME", "SPECIESSCIENTIFICNAME", 
                                 "SPECIESCOMMONNAME", "NUMBERTOTAL", 
                                 "SPECIESTOTALWEIGHT", "LOCATION", "REGION", 
                                 "TEMPSURFACE", "TEMPBOTTOM", "SALINITYSURFACE",
                                 "SALINITYBOTTOM", "DEPTHZONE")]




## creating working data frame with summed biomass, unique event names species 
  #richness column and species in wide form

#creating community matrix with number of individuals and creating environmental matrix
# adding columns that sums number of species and total biomass grouped by eventname

dat <- SEAMAP_invest %>%
  group_by(EVENTNAME) %>%
  summarize(S = length(unique(SPECIESSCIENTIFICNAME)),
            biomass = sum(as.numeric(SPECIESTOTALWEIGHT), na.rm = T),
            date = unique(DATE),
            lat = unique(LATITUDESTART),
            long = unique(LONGITUDESTART),
            year = unique(Year),
            numtotal = sum(as.numeric(NUMBERTOTAL)),
            location = unique(LOCATION),
            region = unique(REGION),
            tempS = unique(as.numeric(TEMPSURFACE)),
            tempB = unique(as.numeric(TEMPBOTTOM)),
            salS = unique(as.numeric(SALINITYSURFACE)),
            salB = unique(as.numeric(SALINITYBOTTOM)),
            depthzone = unique(DEPTHZONE)
            )




dat$EVENTNAME <-as.character(dat$EVENTNAME)
dat["2673", "long"] <- -78.1
dat["6160", "long"] <- -80.1

dat <- cbind(dat, rastervals$layer)
dat <- subset(dat, depthzone == "INNER")
s_environ <- dat
colnames(s_environ)[colnames(s_environ) == "rastervals$layer"] <- "ID"
colnames(s_environ)[colnames(s_environ) == "EVENTNAME"] <- "event"
#changing species total numbers from long form to wide form 

s_wide <- SEAMAP_invest[,c("EVENTNAME","SPECIESCOMMONNAME","NUMBERTOTAL", "DEPTHZONE")]

  #function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
  #run function found in script spread_function

s_wide$EVENTNAME <-as.character(s_wide$EVENTNAME)
s_wide$SPECIESCOMMONNAME <-as.character(s_wide$SPECIESCOMMONNAME)
s_wide$NUMBERTOTAL <-as.numeric(s_wide$NUMBERTOTAL)

s_group <- spread_with_multiple_values(s_wide, SPECIESCOMMONNAME,NUMBERTOTAL, 
                                       aggfunc = sum)

s_group <- subset(s_group, DEPTHZONE == "INNER")

#Export s_group to csv
#write.csv(s_group, file = "./data/s_group.csv")


# Wide form including individual number

s_spread <- data.frame(left_join(dat, s_group, by='EVENTNAME'))

#Export s_spread to csv
#write.csv(s_spread, file = "./data/s_spread.csv")


#Creating presence/absence wide form for rarefaction 
s_rarefac <- data.frame(s_group[,3:202])
s_rarefac[is.na(s_rarefac)] <- 0
s_rarefac[s_rarefac >0] <- 1
s_rarefac <- cbind(s_group$EVENTNAME, s_rarefac)
colnames(s_rarefac)[colnames(s_rarefac)== 's_group$EVENTNAME'] <- "EVENTNAME"
s_rarefac <- data.frame(left_join(dat, s_rarefac, by='EVENTNAME'))

#Export s_rarefac to csv
#write.csv(s_rarefac, file = "./data/s_rarefac.csv")

####BIOMASS COMMUNITY MATRIX ####

#creating community matrix with total biomass in cells and merging with environmental matrix
##did not rerun this as of 3/9/2020##
s_bio <- SEAMAP_invest[,c("EVENTNAME","SPECIESCOMMONNAME","SPECIESTOTALWEIGHT", "DEPTHZONE")]

#function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
#run function found in script spread_function

s_bio$EVENTNAME <-as.character(s_bio$EVENTNAME)
s_bio$SPECIESCOMMONNAME <-as.character(s_bio$SPECIESCOMMONNAME)
s_bio$SPECIESTOTALWEIGHT <-as.numeric(s_bio$SPECIESTOTALWEIGHT)

s_bio_comm <- spread_with_multiple_values(s_bio, SPECIESCOMMONNAME,SPECIESTOTALWEIGHT, 
                                       aggfunc = sum)
s_bio_comm[is.na(s_bio_comm)] <-0


s_bio_comm <- subset(s_bio_comm, DEPTHZONE == "INNER")

#Export s_group to csv
#write.csv(s_bio_comm, file = "./data/s_bio_comm.csv")








