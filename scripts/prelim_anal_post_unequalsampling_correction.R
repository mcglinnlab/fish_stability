#Biomass as a function of species richness post unequal sampling correction

resultsfullpoint2 <- read.csv( "~./fish_stability/data/resultsfullpoint2.csv", header = T)

#plot of average biomass per raster region per year as a function of average species richness per year

for (i in 1989:2015) {
  plot(resultsfullpoint2$averagebio[resultsfullpoint2$year == i] ~ resultsfullpoint2$averageS[resultsfullpoint2$year == i], xlab = "Average Species Richness", ylab = "Average Biomass", main = i)
}

# plot of var in biomass per raster as a function of year per raster 

for (i in unique(resultsfullpoint2$ID)) {
  plot(resultsfullpoint2$varbio[resultsfullpoint2$ID == i] ~ resultsfullpoint2$year[resultsfullpoint2$ID == i], ylim = c(0,2000000), ylab = "Variance in Biomass", xlab = "Year", main = i)
}

#plot of var in biomass per raster as a function of average species richness per raster 

for (j in unique(resultsfullpoint2$ID)) {
  
   plot(resultsfullpoint2$varbio[resultsfullpoint2$ID == j] ~ resultsfullpoint2$averageS[resultsfullpoint2$ID == j], ylim = c(0,1000), ylab = "Var in Biomass", xlab = "Average Species Richness", main = i)
}

#collapsing graph above to create one point for each raster region of var and average species richness through time
for (j in unique(resultsfullpoint2$ID)) {
  averagevarbiovector[j] <- average(resultsfullpoint2$varbio[resultsfullpoint2$ID == j])
  averagespeciesrichvector[j] <- average(resultsfullpoint2$averageS[resultsfullpoint2 == j])
  ID <- j
  avvarspeciesresults <- data.frame(ID, averagevarbiovector, averagespeciesrichvector)
}


