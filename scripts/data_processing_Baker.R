#title: "Data processing of SEAMAP dataset"
#author: "Nathan Baker"
#date: "November 10, 2016"
#output: html_document

# Make 2 datasets (one with fish and abundance, the other with environmental factors)
# Find out number of nets per trawl
# Subset data to only include fish
# Run simple analyses of 1990 vs 2015 data
# SAD distribution


# Database Read-in and Examination ----------------------------------------


#import data
dat = read.csv('./data/bakernj.Coastal Survey.ABUNDANCEBIOMASS.2016-11-03T11.33.55.csv')
##determining number of unique nets (collection number) for each trawl (event name)
n = NULL
uni_event = unique(dat$EVENTNAME)
for (i in 1:length(uni_event)) {
  dat_sub = subset(dat, EVENTNAME == uni_event[i])
  n[i] = length(unique(dat_sub$COLLECTIONNUMBER))
}
n
sum(n)
table(n)

# run check that only one date applies to each event name
out = NULL
for (i in 1:length(dat$EVENTNAME)){
  uni_dates = unique(dat$DATE[dat$EVENTNAME == dat$EVENTNAME[i]])
  out[i] = length(uni_dates)
}

sum(out!=2)

fish_species = read.csv('./data/fish_species.csv', header=FALSE, colClasses='character')
names(fish_species) = 'species'


#library(rfishbase)
#?validate_names
##ignore fishbase for now
##THIS PART REMOVES THE =\" FROM ENTRY NAMES"
dat = as.data.frame(dat)
for (i in 1:ncol(dat)) {
  dat[ , i] = sub('=', '', dat[ , i])
}


uni_sp = unique(dat$SPECIESCOMMONNAME)
# spcies in the sp list
gd_common_names = uni_sp[uni_sp %in% fish_species$species]
# species not in the sp list
uni_sp[!(uni_sp %in% fish_species$species)]

gd_sci_names = unique(dat$SPECIESSCIENTIFICNAME[dat$SPECIESCOMMONNAME %in% gd_common_names])


#write.csv(gd_sci_names, file='./data/gd_sci_names.csv', row.names=F)

#output names that we have filtered out of the dataset
#uni_sci_sp = unique(dat$SPECIESSCIENTIFICNAME)
##write.csv(uni_sci_sp[!(uni_sci_sp %in% gd_sci_names$species)], './data/sp_names_filtered_out.csv', row.names=F)


# Matrix Formation --------------------------------------------------------


#manually enter in small subset of species that lack common names
gd_sci_names = read.csv('./data/gd_sci_names.csv')
names(gd_sci_names) = 'species'

dat_sub = subset(dat, dat$SPECIESSCIENTIFICNAME %in% gd_sci_names$species)

#Merge species names that were inconsistently used
anchoa_rows = grep('ANCHOA', dat_sub$SPECIESSCIENTIFICNAME)
dat_sub$SPECIESSCIENTIFICNAME[anchoa_rows] = 'ANCHOA'


#Add coarse region variable
dat_sub$REGION2 = ifelse(dat_sub$REGION %in% c('SOUTH CAROLINA', 'GEORGIA', 'FLORIDA'), 
                         'South', 'North')

# subset fish columns
fish_cols = c('EVENTNAME','COLLECTIONNUMBER','SPECIESSCIENTIFICNAME',
              'SPECIESCOMMONNAME','NUMBERTOTAL','EFFORT','LOCATION',
              'REGION','DEPTHZONE','STATIONCODE', 'REGION2', 'DEPTHZONE')
dat_sub[1:5 , fish_cols]


# create site x sp matrix (sites as rows and species as columns)
dat_sub2 = subset(dat_sub, dat_sub$DEPTHZONE == 'INNER')
dat_sub2$NUMBERTOTAL = as.integer(dat_sub2$NUMBERTOTAL)

sitexsp = tapply(dat_sub2$NUMBERTOTAL,
                 list(dat_sub2$EVENTNAME, 
                      dat_sub2$SPECIESSCIENTIFICNAME), 
                 sum)
sitexsp = ifelse(is.na(sitexsp), 0, sitexsp)
##write.csv(sitexsp,file = "./data/sitexsp_eventnames.csv", row.names=TRUE)
sitexsp = read.csv('./data/sitexsp_eventnames.csv', row.names = 1)

env = dat_sub2[match(row.names(sitexsp), dat_sub2$EVENTNAME), 
               c('EVENTNAME', 'REGION', 'REGION2', 'LONGITUDESTART', 'LATITUDESTART', 'DATE')]
names(env) = c('EVENTNAME', 'REGION', 'REGION2', 'x', 'y','DATE')
env$YEAR = sapply(strsplit(as.character(env$DATE), '-', fixed=T), function(x) x[3])
env$MONTH = sapply(strsplit(as.character(env$DATE), '-', fixed=T), function(x) x[1])
env$period = ifelse(env$EVENTNAME %in% 1990001:1995563,
                    'historic', ifelse(env$EVENTNAME %in% 2010001:2015657,
                                       'modern', 'other'))
#write.csv(env, file="./data/env.csv", row.names=FALSE)
env = read.csv('./data/env.csv')