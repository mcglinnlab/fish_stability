# BEF Script for Conference and First Chapter
# will draw on s_rarefac and resultsfullpoint2 data sets

#I want biomass per species richness, temp, lat, long, location, region, salinity, 


#working with s_rarefac -> before permutation. each row is trawl event. 

s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)

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


model4 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat + s_rarefac$region +
               s_rarefac$tempB + s_rarefac$tempS)
summary(model4)
plot(model4)



# collapse to average biomass by species richness
bss= with(s_rarefac, tapply(biomass, list(S), mean, na.rm=T))
bss
sss = c(1:48, 50, 54)



model5 <- lm(bss ~ sss)
summary(model5)


plot(bss ~ sss, xlab = "Species Richness (S)", 
     ylab = "Average Biomass per Trawl Event (kg)", pch = 1)
abline(coefficients(model5), lwd = 2)
plot(model5)

  #model the other way 
plot(sss ~ bss)


  #calculating sPIE
#averageing biomass based on how many species are in the event
s_spread <- s_spread[,15:208]
s_spread[is.na(s_spread)] <-0
sPIE <- calc_PIE(s_spread, ENS = T)
plotpiedata <- as.data.frame(cbind(s_rarefac$S, s_rarefac$biomass, sPIE))
sssP <- with(plotpiedata, tapply(sPIE, list(V1), mean, na.rm = T))

modelPI <- lm(sssP ~ sss)
summary(modelPI)
plot(sssP ~ sss)
abline(modelPI$coefficients)
plot(bss ~ sssP)
  #as PIE is increasing, biomass is decreasing. I think this is due to few species 
    #taking up a large portion of the biomass (Croaker, spot, whiting)

plotpiedata$sPIEr <- signif(plotpiedata$sPIE, digits = 2)

#averaging biomass based on how even trawls were
bssPI <- with(plotpiedata, tapply(V2, list(sPIEr), mean, na.rm = T))
bssPI
sssPIE <- c(seq(1,10,0.1), 11:15, 17,18,20,23)
plot(bssPI ~ sssPIE)



  #checking for spatial dependence
    #calculate dist of community matrix with species richness inside
s_comm <- read.csv("~./fish_stability/data/s_comm.csv", header = T)
s_environ <- read.csv("~./fish_stability/data/s_environ.csv", header = T)

sr = apply(s_comm, 1, function(x) sum(x > 0))
hist(sr)

sr_dist = dist(sr)

latlong <- s_environ[,6:7]

xy_dist = dist(latlong)

max_dist = max(xy_dist) / 2
# plot result
plot(xy_dist, sr_dist)
abline(lm(sr_dist ~ xy_dist), lwd=3, col='red')
lines(lowess(xy_dist, sr_dist), lwd=3, col='pink')
abline(v = max_dist, col='red', lwd=3, lty=2)

mantel(xy_dist, sr_dist)
#output of test is correlation is zero and not significantly different from zero. 



# removing point 47 outlier

model6 <- lm(bss[bss < 300] ~ sss[bss < 300])
summary(model5)


plot(bss[bss < 300] ~ sss[bss < 300], xlab = "Species Richness (S)", 
     ylab = "Average Biomass per Trawl Event (kg)")
abline(coefficients(model6))
plot(model6)


#how important is S 
model7 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat + s_rarefac$year + 
               s_rarefac$region + s_rarefac$tempB + s_rarefac$salB)
summary(model7)
plot(model7)

plot(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat + s_rarefac$year + 
       s_rarefac$region + s_rarefac$tempB + s_rarefac$salB)

## make this into table for poster 
model7aov <- aov(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat 
                 + s_rarefac$year + s_rarefac$region + s_rarefac$tempB 
                 + s_rarefac$salB + s_rarefac$month)
summary(model7aov)
plot(model7aov)
AIC(model7aov)

#adding date to look at season
dates <- s_rarefac$date
dates <- as.Date(dates, "%m/%d/%Y")
month <- strftime(dates, "%m")
s_rarefac <- cbind(month, s_rarefac)







model8 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$year)
summary(model8)
plot(model8)

model8aov <- aov(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$year)
summary(model8aov)
AIC(model8aov)

model9 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S)
summary(model9)

model9aov <- aov(log(s_rarefac$biomass) ~ s_rarefac$S)
summary(model9aov)
AIC(model9aov)
summary(anova(model8aov, model9aov))

#runCCA and PCA
  #PCA and CCA with presence/absence as community matrix
library(vegan)
s_comm <- s_rarefac[,16:208]

write.csv(s_comm, "~./fish_stability/data/s_comm.csv")

s_environ <- s_rarefac[,1:15]

write.csv(s_environ, "~./fish_stability/data/s_environ.csv")

#indirect
s_pca <- rda(s_comm)
s_pca
plot(s_pca)

#direct
s_cca <- cca(s_comm ~ S + biomass + lat + year + region + tempB + salB,
             data = s_environ, na.action = na.exclude)
s_cca
plot(s_cca)

  #run PCA and CCA with biomass in community matrix
  #indirect
sb_pca <- rda(s_bio_comm)
sb_pca
plot(sb_pca)

  #direct
sb_cca <- cca(s_comm~ S + lat + year + region + tempB + salB + month,
              data = s_environ, na.action = na.exclude)
sb_cca
plot(sb_cca)





#after rasterizing work 
  #rounded S to two significant digits

resultsfullpoint2$roundS <- signif(resultsfullpoint2$averageS, digits = 2)
bss_raster <- with(resultsfullpoint2, tapply(averagebio, list(roundS),mean, na.rm= T))
bss_raster
sss_raster <- c(7.1, 9.8, 11:33)
plot(bss_raster ~ sss_raster)

model10 <- lm(bss_raster ~ sss_raster)
summary(model10)
plot(model10)

plot(bss_raster ~ sss_raster, ylab = "Average Biomass per Raster Region (kg)", xlab = "Species Richness (S)")
abline(model10$coefficients, lwd = 2)



    # what does species richness over time look like 
S_ID <- with(yrag_sub, tapply(averageS, list(ID), mean, na.rm = T))
S_ID
S_ID <- signif(S_ID, digits = 2)
S_ID
B_VAR_ID <- with(yrag_sub, tapply(averagebio,list(ID),sd, na.rm = T))
plot(B_VAR_ID ~ S_ID)
