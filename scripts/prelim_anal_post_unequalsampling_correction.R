#Biomass as a function of species richness post unequal sampling correction


#in permutation is it still ignoring grids with only one trawl? 
#those rasters with only one site do not have a variation. Complete cases removes
#these raster regions from the results data frame

resultsfullpoint2 <- read.csv( "~./fish_stability/data/resultsfullpoint2.csv",
                               header = T)
completeresultsfullpoint2 <- resultsfullpoint2[complete.cases(resultsfullpoint2),]

#plot of average biomass per raster region per year as a function of average
#species richness per year

for (i in 1989:2015) {
  plot(completeresultsfullpoint2$averagebio[resultsfullpoint2$year == i] ~
         completeresultsfullpoint2$averageS[resultsfullpoint2$year == i],
       xlab = "Average Species Richness", ylab = "Average Biomass", main = i)
}

# plot of var in biomass per raster as a function of year per raster
# var bio shows variance in trawls within a raster region when resampled 
for (j in unique(resultsfullpoint2$ID)) {
  plot(completeresultsfullpoint2$varbio[completeresultsfullpoint2$ID == j] ~ 
       completeresultsfullpoint2$year[completeresultsfullpoint2$ID == j], 
       ylim = c(0,2000000), ylab = "Variance in Biomass", xlab = "Year", main = j)
}

#plot of var in biomass per raster as a function of average species richness per raster 

for (k in unique(resultsfullpoint2$ID)) {
  
   plot(completeresultsfullpoint2$varbio[completeresultsfullpoint2$ID == k] ~ 
          completeresultsfullpoint2$averageS[completeresultsfullpoint2$ID == k],
        ylim = c(0,1000), xlim = c(0,40), ylab = "Var in Biomass", 
        xlab = "Average Species Richness", main = k)
}

# this each point on this plot is one raster region

plot(completeresultsfullpoint2$averagebio ~ completeresultsfullpoint2$year)

# biomass as a function of species richness for each raster region
for (k in unique(resultsfullpoint2$ID)) {
  
  plot(completeresultsfullpoint2$averagebio[completeresultsfullpoint2$ID == k] ~ 
         completeresultsfullpoint2$averageS[completeresultsfullpoint2$ID == k],
       ylim = c(0,1000), xlim = c(0,40), ylab = "Biomass", 
       xlab = "Average Species Richness", main = k)
}

#collapsing graph above to create one point for each raster region of var and
#average species richness through time

averagevarbiovector <- NULL
averagespeciesrichvector <- NULL
avvarspeciesresults <- NULL
ID <- NULL
vec <- unique(resultsfullpoint2$ID)

for (m in vec) {
  averagevarbiovector <- c(averagevarbiovector, 
                           var(completeresultsfullpoint2$averagebio[completeresultsfullpoint2$ID == m]))
  averagespeciesrichvector <- c(averagespeciesrichvector,
                                mean(completeresultsfullpoint2$averageS[completeresultsfullpoint2$ID == m]))
  ID <- c(ID, m)
  avvarspeciesresults <- data.frame(ID, averagevarbiovector, averagespeciesrichvector)
}

#each point is a raster region averaged through time

varspecieslm <- lm(avvarspeciesresults$averagevarbiovector ~ 
                     avvarspeciesresults$averagespeciesrichvector)
plot(avvarspeciesresults$averagevarbiovector ~ 
     avvarspeciesresults$averagespeciesrichvector, 
     xlab = "Average Species Richness Through Time", 
     ylab = "Variance in Biomass Through Time")
abline(varspecieslm$coefficients)

plot((1/avvarspeciesresults$averagevarbiovector) ~ 
       avvarspeciesresults$averagespeciesrichvector,
     xlab = "Average Species Richness Through Time", 
     ylab = "Invariance Through Time")

summary(varspecieslm)




#notes from Nov 21 meeting 

with(completeresultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
with(completeresultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
with(completeresultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
bsd = with(completeresultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(completeresultsfullpoint2, tapply(averageS, list(ID), mean, na.rm=T))
plot(bsd ~ sm)
bm = with(completeresultsfullpoint2, tapply(averagebio, list(ID), mean, na.rm=T))
plot(bm, bsd)
plot(sm, bm)
# since biomass is driving standard deviation so much the negative relationship of species richness is lost in plot.
summary(lm(bsd ~ sm + bm))
plot(bsd ~ sm)
