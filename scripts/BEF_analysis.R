# BEF Script for Conference and First Chapter
# will draw on s_rarefac and resultsfullpoint2 data sets

#I want biomass per species richness, temp, lat, long, location, region, salinity, 


#working with s_rarefac -> before permutation. each row is trawl event. 

 #load s_rarefac
model1 <- lm(s_rarefac$biomass ~ s_rarefac$S)

plot(s_rarefac$biomass ~ s_rarefac$S)
abline(coefficients(model1))
summary(model1)
plot(model1)



model2 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S)

plot(log(s_rarefac$biomass) ~ s_rarefac$S)
abline(coefficients(model2))
summary(model2)
plot(model2)


model3 <- lm(log(s_rarefac$biomass) ~ log(s_rarefac$S))

plot(log(s_rarefac$biomass) ~ log(s_rarefac$S))
abline(coefficients(model3))
summary(model3)
plot(model3)


model4 <- lm(log(biomass) ~ s_rarefac$S + s_rarefac$lat + s_rarefac$region +
               s_rarefac$tempB + s_rarefac$tempS)
summary(model4)
plot(model4)



# collapse to average biomass by species richness
model5 <- lm(bss ~ sss)
summary(model5)

bss= with(s_rarefac, tapply(biomass, list(S), mean, na.rm=T))
bss
sss = c(1:48, 50, 54)

plot(bss ~ sss, xlab = "Species Richness (S)", 
     ylab = "Average Biomass per Trawl Event (kg)")
abline(coefficients(model5))
plot(model5)


#runCCA and PCA
