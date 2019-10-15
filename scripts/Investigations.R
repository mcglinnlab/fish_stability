#Trying some things 

library(tmap)


#average species richness map 0.2 for poster

tm_shape(SpeciesRich_raster) +
  tm_raster(title = "Average Species Richness") +
tm_shape(continents) +
  tm_borders(lwd = 0.5)


#species richness versus biomass graph for poster

xyplot(s_rarefac$biomass ~ s_rarefac$S|s_rarefac$year , relation = 'free')
xyplot(s_rarefac$biomass ~ s_rarefac$S, groups = s_rarefac$year)
       

lm1995 <- lm(s_rarefac$biomass[s_rarefac$year == '1995'] ~ s_rarefac$S[s_rarefac$year == '1995'])
plot(s_rarefac$biomass[s_rarefac$year == '1995'] ~ s_rarefac$S[s_rarefac$year == '1995'])
abline(lm1995$coefficients)
summary(lm1995)


