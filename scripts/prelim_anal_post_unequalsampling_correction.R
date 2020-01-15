#Biomass as a function of species richness post unequal sampling correction


resultsfullpoint2 <- read.csv( "~./fish_stability/data/resultsfullpoint2.csv",
                               header = T)



#plot of average biomass per raster region per year as a function of average
#species richness per year

for (i in 1989:2015) {
  plot(resultsfullpoint2$averagebio[resultsfullpoint2$year == i] ~
         resultsfullpoint2$averageS[resultsfullpoint2$year == i],
       xlab = "Average Species Richness", ylab = "Average Biomass", main = i)
}

# plot of invar in biomass per raster as a function of year per raster
# var bio shows variance in trawls within a raster region when resampled 
for (j in unique(resultsfullpoint2$ID)) {
  plot(resultsfullpoint2$invar[resultsfullpoint2$ID == j] ~ 
       resultsfullpoint2$year[resultsfullpoint2$ID == j], 
       ylab = "Invar in Biomass", xlab = "Year", main = j)
}

#plot of invar in biomass per raster as a function of average species richness per raster 

for (k in unique(resultsfullpoint2$ID)) {
  
   plot(resultsfullpoint2$invar[resultsfullpoint2$ID == k] ~ 
        resultsfullpoint2$averageS[resultsfullpoint2$ID == k],
        ylab = "Invar in Biomass", 
        xlab = "Average Species Richness", main = k)
}

# each point on this plot is one raster region

plot(resultsfullpoint2$averagebio ~ resultsfullpoint2$year)
plot(resultsfullpoint2$invar ~ resultsfullpoint2$year)

# biomass as a function of species richness for each raster region
for (k in unique(resultsfullpoint2$ID)) {
  
  plot(resultsfullpoint2$averagebio[resultsfullpoint2$ID == k] ~ 
         resultsfullpoint2$averageS[resultsfullpoint2$ID == k],
       ylim = c(0,1000), xlim = c(0,40), ylab = "Biomass", 
       xlab = "Average Species Richness", main = k)
}



#Update to notes from Nov 21 meeting 


## these three are biomass sd, species richness average, and invar average for IDs over all years 

bsd = with(resultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(resultsfullpoint2, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(resultsfullpoint2, tapply(averagebio, list(ID), mean, na.rm=T))
invar = with(resultsfullpoint2, tapply(invar, list(ID), mean, na.rm = T))
cv = with(resultsfullpoint2, tapply(cv, list(ID), mean, na.rm = T))

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)



#plot of invar as a function of species rich ## outliers were primarily removed by fixing sampling threshold in perm code. 
plot(invar ~ sm, ylim = c(0, 10))
abline(coef(lm(invar ~ sm)))

summary(lm(invar ~ sm))
summary(lm(bsd ~ sm + bm))
summary(lm(invar ~ sm + bm))




#Just looking at raw graphs 
with(resultsfullpoint2, plot(invar ~ averageS))
with(resultsfullpoint2, plot(averagebio ~ averageS))

LMinvarS <- lm(invar ~ averageS, data = resultsfullpoint2)
summary(lm(invar ~ averageS + averagebio, data = resultsfullpoint2))


with(resultsfullpoint2, plot(invar ~ averageS))
abline(LMinvarS$coefficients)

