#Biomass as a function of species richness post unequal sampling correction

#results when data is not aggregated into years
resultsfullpoint2 <- read.csv( "~./fish_stability/data/resultsfullpoint2.csv",
                               header = T)
#results when data is aggregated into three year stints
yrag_resultsfull <- read.csv("~./fish_stability/data/yrag_resultsfullpoint2.csv",
                             header = T)
#results when yrag data is subset for IDs that are complete over the years
yrag_sub <- read.csv("~./fish_stability/data/yrag_sub.csv", header = T)


#plot of average biomass per raster region per year as a function of average
#species richness per year

for (i in 1989:2015) {
  plot(resultsfullpoint2$averagebio[resultsfullpoint2$year == i] ~
         resultsfullpoint2$averageS[resultsfullpoint2$year == i],
       xlab = "Average Species Richness", ylab = "Average Biomass", main = i)
}

# plot of spatial invar in biomass per raster as a function of year per raster
# var bio shows variance in trawls within a raster region when resampled 
for (j in unique(resultsfullpoint2$ID)) {
  plot(resultsfullpoint2$invar[resultsfullpoint2$ID == j] ~ 
       resultsfullpoint2$year[resultsfullpoint2$ID == j], 
       ylab = "Invar in Biomass", xlab = "Year", main = j)
}

#plot of spatial invar in biomass per raster as a function of average species richness per raster 

for (k in unique(resultsfullpoint2$ID)) {
  
   plot(resultsfullpoint2$invar[resultsfullpoint2$ID == k] ~ 
        resultsfullpoint2$averageS[resultsfullpoint2$ID == k],
        ylab = "Invar in Biomass", 
        xlab = "Average Species Richness", main = k)
}

# each point on this plot is one raster region

plot(resultsfullpoint2$averagebio ~ resultsfullpoint2$year)

#spatial invar
plot(resultsfullpoint2$invar ~ resultsfullpoint2$year)

# biomass as a function of species richness for each raster region
for (k in unique(resultsfullpoint2$ID)) {
  
  plot(resultsfullpoint2$averagebio[resultsfullpoint2$ID == k] ~ 
         resultsfullpoint2$averageS[resultsfullpoint2$ID == k],
       ylim = c(0,1000), xlim = c(0,40), ylab = "Biomass", 
       xlab = "Average Species Richness", main = k)
}



#Just looking at raw graphs 
with(resultsfullpoint2, plot(averagebio ~ averageS))
with(resultsfullpoint2, plot(averagebio ~ year))

devtools::install_github('mobiodiv/mobr', ref = 'dev')

data(inv_comm)



#ANALYSIS when using resultsfullpoint2 data frame. This is when the deep strata
  #has been removed but the years have not been aggregated


## these three are biomass sd, species richness average, and invar average for IDs over all years 

bsd = with(resultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(resultsfullpoint2, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(resultsfullpoint2, tapply(averagebio, list(ID), mean, na.rm=T))
var = with(resultsfullpoint2, tapply(averagebio, list(ID), var, na.rm = T))
cv = var / (bm ^ 2)
invar = 1/ cv

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)
plot(invar ~ sm)
plot(invar[invar < 20] ~ sm[invar < 20])

#plot of invar as a function of species rich ## 


plot(invar ~ sm)
abline(coef(lm(invar ~ sm)))
summary(lm(invar ~ sm))
lminvarsm <- glm(invar ~ sm)
plot(lminvarsm)



#ANALYSIS when using yrag_resultsfull data frame. This is when the deep strata
#has been removed and the years have been aggregated into three year chunks. 



## these three are biomass sd, species richness average, and invar average for IDs over 3 yr bins

bsd = with(yrag_resultsfull, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(yrag_resultsfull, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(yrag_resultsfull, tapply(averagebio, list(ID), mean, na.rm=T))
var = with(yrag_resultsfull, tapply(averagebio, list(ID), var, na.rm = T))
cv = var / (bm ^ 2)
invar = 1/ cv

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)
plot(invar ~ sm)
plot(invar ~ sm, ylim = c(0, 30))


#plot of invar as a function of species rich ## outliers were primarily removed 
  #by fixing sampling threshold in perm code. A few came back after removing outer
    #depth range. 

plot(invar ~ sm)
abline(coef(lm(invar ~ sm)))
summary(lm(invar ~ sm))
lminvarsm <- lm(invar ~ sm)
plot(lminvarsm)

#looking at relationship with outliers removed 

plot(invar[invar < 50] ~ sm[invar < 50])
abline(coef(lm(invar[invar < 50] ~ sm[invar < 50])))
summary(lm(invar[invar < 50] ~ sm[invar < 50]))




#ANALYSIS when using yrag_sub data frame. This is when the deep strata
#has been removed and the years have been aggregated into three year chunks. A 
  #list of 'good' IDs is pulled out where these IDs have representation over
  #all year chunks a-i



#subsetting raster IDs with at least 5 trawls across 3 yr time frame 
  #list of IDs with representation across years
IDlist <- c(1246,1294,1340,1388,1486,1532,1534,1536,1582,
            1586,1630,1680,1730,1777,1778,1779,1826,1875,1923,1924,1972,1973,2021,
            2022,2120,2169,2170,2219,2269,2318,2319,2419,2519,2569,2620,2670,
            2721,2771,2772)
#pulling out rows with the above listed IDs
yrag_sub <- yrag_resultsfull[yrag_resultsfull$ID %in% IDlist,]

#doing summaries for biomass, species, invar values

bsd = with(yrag_sub, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(yrag_sub, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(yrag_sub, tapply(averagebio, list(ID), mean, na.rm=T))
var = with(yrag_sub, tapply(averagebio, list(ID), var, na.rm = T))
cv = var / (bm ^ 2)
invar = 1/ cv

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)
plot(invar ~ sm)
plot(log(invar) ~ log(sm))


#plot and lm 
plot(invar ~ sm)
abline(coef(lm(invar ~ sm)))
summary(lm(invar ~ sm))
lminvarsm <- lm(invar ~ sm)
plot(lminvarsm)

plot(invar ~ sm)
lines(x, y)
abline(coef(lm(invar ~ sm + bsd)))
summary(lm(invar ~ sm + bsd))
lminvarsmbsd <- lm(invar ~ sm + bsd)
plot(lminvarsmbsd)



