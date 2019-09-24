
#Data Processing

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

gd_sci_names = unique(SEAMAP$SPECIESSCIENTIFICNAME[SEAMAP$SPECIESCOMMONNAME %in% gd_common_names])

#write.csv(gd_sci_names, file='./gd_sci_names.csv', row.names=F)

#output names that we have filtered out of the dataset
#uni_sci_sp = unique(SEAMAP$SPECIESSCIENTIFICNAME)
#write.csv(uni_sci_sp[!(uni_sci_sp %in% gd_sci_names)], './sp_names_filtered_out.csv', row.names=F)


#manually enter in small subset of species that lack common names
gd_sci_names = read.csv('./gd_sci_names.csv')
names(gd_sci_names) = 'species'

SEAMAP_sub = subset(SEAMAP, SEAMAP$SPECIESSCIENTIFICNAME %in% gd_sci_names$species)


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
write.csv(SEAMAP_sub, file = "./data/SEAMAP_sub.csv")

#read in SEAMAP_sub
SEAMAP_sub <-read.csv("~./fish_stability/data/SEAMAP_sub.csv", header = T)


## creating new data frame where row is unique event and total number of each species are in wide form ##

library(dplyr)
library(tidyr)
library(tidyselect)
library(purrr)

#selecting only needed columns 

SEAMAP_invest <- SEAMAP_sub[,c("DATE", "Year", "LONGITUDESTART", "LATITUDESTART", "COLLECTIONNUMBER", "EVENTNAME", "SPECIESSCIENTIFICNAME", "SPECIESCOMMONNAME", "NUMBERTOTAL", "SPECIESTOTALWEIGHT")]



## creating working data frame with summed biomass, unique event names species richness column and species in wide form

#columns to be added
    #raster ids for different resolutions


# adding columns that sums number of species and total biomass grouped by eventname

dat <- SEAMAP_invest %>%
  group_by(EVENTNAME) %>%
  summarize(S = length(unique(SPECIESSCIENTIFICNAME)),
            biomass = sum(as.numeric(SPECIESTOTALWEIGHT)),
            date = unique(DATE),
            lat = unique(LATITUDESTART),
            long = unique((LONGITUDESTART)))

#changing species total numbers from long form to wide form 

s_wide <- SEAMAP_invest[,c("EVENTNAME","SPECIESCOMMONNAME","NUMBERTOTAL")]

#function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R

s_wide$EVENTNAME <-as.character(s_wide$EVENTNAME)
s_wide$SPECIESCOMMONNAME <-as.character(s_wide$SPECIESCOMMONNAME)
s_wide$NUMBERTOTAL <-as.numeric(s_wide$NUMBERTOTAL)

s_group <- spread_with_multiple_values(s_wide, SPECIESCOMMONNAME,NUMBERTOTAL, aggfunc = sum)

s_spread <- data.frame(left_join(dat, s_group, by='EVENTNAME'))

#Export s_spread to csv
#write.csv(s_spread, file = "./data/s_spread.csv")


