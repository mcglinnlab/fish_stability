
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

write.csv(gd_sci_names, './good_sci_names.csv', row.names=F)

#output names that we have filtered out of the dataset
#uni_sci_sp = unique(SEAMAP$SPECIESSCIENTIFICNAME)
#write.csv(uni_sci_sp[!(uni_sci_sp %in% gd_sci_names)],
  #'./sp_names_filtered_out.csv', row.names=F)


#manually enter in small subset of species that lack common names
gd_sci_names = read.csv('./good_sci_names.csv')
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

SEAMAP_sub$LONGITUDESTART[SEAMAP_sub$EVENTNAME == "2010233"] <- -80.1
SEAMAP_sub$LONGITUDESTART[SEAMAP_sub$EVENTNAME == "1998467"] <- -78.1

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



##creating number of individuals community matrix##

## changing species total numbers from long form to wide form ##
s_wide <- SEAMAP_sub[,c("EVENTNAME","SPECIESCOMMONNAME","NUMBERTOTAL")]

#changing variable classes
s_wide$EVENTNAME <-as.character(s_wide$EVENTNAME)
s_wide$SPECIESCOMMONNAME <-as.character(s_wide$SPECIESCOMMONNAME)
s_wide$NUMBERTOTAL <-as.numeric(s_wide$NUMBERTOTAL)

s_wide <- s_wide %>%
  pivot_wider(id_cols = EVENTNAME, names_from = SPECIESCOMMONNAME, values_from = NUMBERTOTAL, values_fn = sum)


# Wide form including individual number

s_spread <- data.frame(left_join(event_dat, s_wide, by='EVENTNAME'))

#Export s_spread to csv
#write.csv(s_spread, file = "./data/s_spread.csv")






##Creating presence/absence community matrix ## 
s_pres <- data.frame(s_wide[,2:198])
s_pres[is.na(s_pres)] <- 0
s_pres[s_pres > 0] <- 1
s_pres <- cbind(s_wide$EVENTNAME, s_pres)
colnames(s_pres)[colnames(s_pres)== 's_wide$EVENTNAME'] <- "EVENTNAME"
s_pres <- data.frame(left_join(event_dat, s_pres, by='EVENTNAME'))

#Export s_pres to csv
#write.csv(s_pres, file = "./data/s_pres.csv")





##creating biomass community matrix 

s_bio <- SEAMAP_sub[,c("EVENTNAME","SPECIESCOMMONNAME","SPECIESTOTALWEIGHT")]


s_bio$EVENTNAME <-as.character(s_bio$EVENTNAME)
s_bio$SPECIESCOMMONNAME <-as.character(s_bio$SPECIESCOMMONNAME)
s_bio$SPECIESTOTALWEIGHT <-as.numeric(s_bio$SPECIESTOTALWEIGHT)

s_bio <- s_bio %>%
  pivot_wider(id_cols = EVENTNAME, names_from = SPECIESCOMMONNAME, values_from = NUMBERTOTAL, values_fn = sum)


s_bio[is.na(s_bio)] <-0

s_bio <- data.frame(left_join(event_dat, s_bio, by='EVENTNAME'))

#Export s_bio to csv
#write.csv(s_bio, file = "./data/s_bio.csv")



#### CREATING SHRIMP AND FLOUNDER DATA SETS ####

#importing shrimp and flounder species lists
shrimp_sp <- read.csv("./shrimp_sp.csv", header = T)
flounder_sp <- read.csv("./flounder_sp.csv", header = T)

#subset shrimp and flounder from SEAMAP
  #shrimp
SEAMAP_shrimp = subset(SEAMAP, SEAMAP$SPECIESCOMMONNAME
                    %in% shrimp_sp$species)
  #flounder
SEAMAP_flounder = subset(SEAMAP, SEAMAP$SPECIESCOMMONNAME
                    %in% flounder_sp$species)

#find and remove rows with nonlogical coordinates
  #shrimp
apply(SEAMAP_shrimp[,c("LONGITUDESTART","LATITUDESTART", "COLLECTIONNUMBER")],2, summary)
SEAMAP_shrimp <- subset(SEAMAP_shrimp, LONGITUDESTART < -90 & LATITUDESTART < 90)
  #flounder
apply(SEAMAP_flounder[,c("LONGITUDESTART","LATITUDESTART", "COLLECTIONNUMBER")],2, summary)
SEAMAP_flounder <- subset(SEAMAP_flounder, LONGITUDESTART < -90 & LATITUDESTART < 90)

#removing deep strata collection events
  #shrimp
SEAMAP_shrimp <- subset(SEAMAP_shrimp, DEPTHZONE == "INNER")
  #flounder
SEAMAP_flounder <- subset(SEAMAP_flounder, DEPTHZONE == "INNER")

#selecting only needed columns
  #shrimp
SEAMAP_shrimp <- SEAMAP_shrimp[,c("DATE", "Year", "LONGITUDESTART",
                            "LATITUDESTART", "COLLECTIONNUMBER", 
                            "EVENTNAME", "SPECIESSCIENTIFICNAME", 
                            "SPECIESCOMMONNAME", "NUMBERTOTAL", 
                            "SPECIESTOTALWEIGHT", "LOCATION", "REGION", 
                            "TEMPSURFACE", "TEMPBOTTOM", "SALINITYSURFACE",
                            "SALINITYBOTTOM")]
  #flounder
SEAMAP_flounder <- SEAMAP_flounder[,c("DATE", "Year", "LONGITUDESTART",
                            "LATITUDESTART", "COLLECTIONNUMBER", 
                            "EVENTNAME", "SPECIESSCIENTIFICNAME", 
                            "SPECIESCOMMONNAME", "NUMBERTOTAL", 
                            "SPECIESTOTALWEIGHT", "LOCATION", "REGION", 
                            "TEMPSURFACE", "TEMPBOTTOM", "SALINITYSURFACE",
                            "SALINITYBOTTOM")]

##creating biomass community matrix for shrimp and flounder subset
  #shrimp
s_bio_shrimp <- SEAMAP_shrimp[,c("EVENTNAME","SPECIESCOMMONNAME","SPECIESTOTALWEIGHT")]

#function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
#run function found in script spread_function

s_bio_shrimp$EVENTNAME <-as.character(s_bio_shrimp$EVENTNAME)
s_bio_shrimp$SPECIESCOMMONNAME <-as.character(s_bio_shrimp$SPECIESCOMMONNAME)
s_bio_shrimp$SPECIESTOTALWEIGHT <-as.numeric(s_bio_shrimp$SPECIESTOTALWEIGHT)

s_bio_shrimp <- spread_with_multiple_values(s_bio_shrimp, SPECIESCOMMONNAME,SPECIESTOTALWEIGHT, 
                                     aggfunc = sum)

s_bio_shrimp <- data.frame(merge(event_dat, s_bio_shrimp, all = T))

s_bio_shrimp[is.na(s_bio_shrimp)] <-0
#Export s_bio_shrimp to csv
#write.csv(s_bio_shrimp, file = "./data/s_bio_shrimp.csv")

  #flounder
s_bio_flounder <- SEAMAP_flounder[,c("EVENTNAME","SPECIESCOMMONNAME","SPECIESTOTALWEIGHT")]

#function found at https://rdrr.io/github/trias-project/trias/src/R/spread_with_multiple_values.R
#run function found in script spread_function

s_bio_flounder$EVENTNAME <-as.character(s_bio_flounder$EVENTNAME)
s_bio_flounder$SPECIESCOMMONNAME <-as.character(s_bio_flounder$SPECIESCOMMONNAME)
s_bio_flounder$SPECIESTOTALWEIGHT <-as.numeric(s_bio_flounder$SPECIESTOTALWEIGHT)

s_bio_flounder <- spread_with_multiple_values(s_bio_flounder, SPECIESCOMMONNAME,SPECIESTOTALWEIGHT, 
                                            aggfunc = sum)

s_bio_flounder <- data.frame(merge(event_dat, s_bio_flounder, all = T))

s_bio_flounder[is.na(s_bio_flounder)] <-0
#Export s_bio_flounder to csv
#write.csv(s_bio_flounder, file = "./data/s_bio_flounder.csv")




