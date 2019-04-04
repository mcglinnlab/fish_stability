
#Data Processing

#import fish species csv

fish_species = read.csv('./fish_species.csv', header=FALSE, colClasses='character')
names(fish_species) = 'species'

#import SEAMAP data

SEAMAP<-read.csv("~./fish_stability/SEAMAPData.csv", header = T)



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


