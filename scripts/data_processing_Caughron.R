
#libraries
library(dplyr)
library(tidyr)
library(tidyselect)
library(purrr)


#### Initial Data Processing ####

#import fish species csv
fish_species = read.csv('./fish_species.csv', header=FALSE, colClasses='character')
names(fish_species) = 'species'

#import SEAMAP data
SEAMAP<-read.csv("~./fish_stability/data/SEAMAPData.csv", header = T)

#determining number of unique nets (collection number) for each trawl (event name)
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

#run check that only one date applies to each event name
out = NULL
for (i in 1:length(SEAMAP$EVENTNAME)){
  uni_dates = unique(SEAMAP$DATE[SEAMAP$EVENTNAME == SEAMAP$EVENTNAME[i]])
  out[i] = length(uni_dates)
}

sum(out!=2)


#this part removes the =\" from entry names"
SEAMAP = as.data.frame(SEAMAP)
for (i in 1:ncol(SEAMAP)) {
  SEAMAP[ , i] = sub('=', '', SEAMAP[ , i])
}


#finding unique species names
uni_sp = unique(SEAMAP$SPECIESCOMMONNAME)
#species in the sp list
gd_common_names = uni_sp[uni_sp %in% fish_species$species]
#species not in the sp list
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

#removing deep strata collection events
SEAMAP_sub <- subset(SEAMAP_sub, DEPTHZONE == "INNER")

#selecting only needed columns 
SEAMAP_sub <- SEAMAP_sub[,c("DATE", "Year", "LONGITUDESTART",
                                 "LATITUDESTART", "COLLECTIONNUMBER", 
                                 "EVENTNAME", "SPECIESSCIENTIFICNAME", 
                                 "SPECIESCOMMONNAME", "NUMBERTOTAL", 
                                 "SPECIESTOTALWEIGHT", "LOCATION", "REGION", 
                                 "TEMPSURFACE", "TEMPBOTTOM", "SALINITYSURFACE",
                                 "SALINITYBOTTOM")]



#### CREATING COMMUNITY MATRICES ####


#creating environmental matrix
event_dat <- SEAMAP_sub %>%
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
            salB = unique(as.numeric(SALINITYBOTTOM))
            )


event_dat$EVENTNAME <-as.character(event_dat$EVENTNAME)
event_dat$long <- as.numeric(event_dat$long)

#replacing incorrect longtitudes
event_dat["2673", "long"] <- -78.1
event_dat["6160", "long"] <- -80.1



##creating number of individuals community matrix##

## changing species total numbers from long form to wide form ##
s_wide <- SEAMAP_sub[,c("EVENTNAME","SPECIESCOMMONNAME","NUMBERTOTAL")]

#changing variable classes
s_wide$EVENTNAME <-as.character(s_wide$EVENTNAME)
s_wide$SPECIESCOMMONNAME <-as.character(s_wide$SPECIESCOMMONNAME)
s_wide$NUMBERTOTAL <-as.numeric(s_wide$NUMBERTOTAL)

  #function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
  #run function found in script spread_function

s_wide <- spread_with_multiple_values(s_wide, SPECIESCOMMONNAME,NUMBERTOTAL, 
                                       aggfunc = sum)

# Wide form including individual number

s_spread <- data.frame(left_join(event_dat, s_wide, by='EVENTNAME'))

#Export s_spread to csv
#write.csv(s_spread, file = "./data/s_spread.csv")






##Creating presence/absence community matrix ## 
s_pres <- data.frame(s_wide[,2:195])
s_pres[is.na(s_pres)] <- 0
s_pres[s_pres > 0] <- 1
s_pres <- cbind(s_wide$EVENTNAME, s_pres)
colnames(s_pres)[colnames(s_pres)== 's_wide$EVENTNAME'] <- "EVENTNAME"
s_pres <- data.frame(left_join(event_dat, s_pres, by='EVENTNAME'))

#Export s_pres to csv
#write.csv(s_pres, file = "./data/s_pres.csv")





##creating biomass community matrix 

s_bio <- SEAMAP_sub[,c("EVENTNAME","SPECIESCOMMONNAME","SPECIESTOTALWEIGHT")]

#function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
#run function found in script spread_function

s_bio$EVENTNAME <-as.character(s_bio$EVENTNAME)
s_bio$SPECIESCOMMONNAME <-as.character(s_bio$SPECIESCOMMONNAME)
s_bio$SPECIESTOTALWEIGHT <-as.numeric(s_bio$SPECIESTOTALWEIGHT)

s_bio <- spread_with_multiple_values(s_bio, SPECIESCOMMONNAME,SPECIESTOTALWEIGHT, 
                                       aggfunc = sum)
s_bio[is.na(s_bio)] <-0

s_bio <- data.frame(left_join(event_dat, s_bio, by='EVENTNAME'))

#Export s_bio to csv
#write.csv(s_bio, file = "./data/s_bio.csv")








