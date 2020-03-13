# BEF Script for Conference and First Chapter

library(vegan)


####BEFORE RASTERIZATION####
#working with s_rarefac -> before permutation. each row is trawl event. 

s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)

#MODELING BIOMASS AS FUNCTION OF OTHER VARIABLES

 #model 1 biomass ~ species
model1 <- lm(s_rarefac$biomass ~ s_rarefac$S)
plot(s_rarefac$biomass ~ s_rarefac$S)
abline(coefficients(model1))
summary(model1)
plot(model1)


  #model 2 ln biomass ~ species
model2 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S)
plot(log(s_rarefac$biomass) ~ s_rarefac$S, xlab = "Species Richness", 
     ylab = "Ln Biomass")
abline(coefficients(model2), lwd = 4, col = "red")
summary(model2)
plot(model2)

plot(log(s_rarefac$biomass) ~ s_rarefac$S, xlab = "Species Richness", 
     ylab = "Ln Biomass")
lines(lowess(log(s_rarefac$biomass) ~ s_rarefac$S), col = "red", lwd = 4)


  #model 3 ln biomass ~ ln species
model3 <- lm(log(s_rarefac$biomass) ~ log(s_rarefac$S))
plot(log(s_rarefac$biomass) ~ log(s_rarefac$S))
abline(coefficients(model3))
summary(model3)
plot(model3)


  #model 4 ln biomass ~ s , lat, region, tempB, tempS
model4 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat + s_rarefac$region +
               s_rarefac$tempB + s_rarefac$tempS)
summary(model4)
plot(model4)

  
  #model 5
    #collapse to average biomass by species richness
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
summary(lm(sss ~ bss))
plot(sss ~ bss)


  #model 6
    #removing point 47 outlier
model6 <- lm(bss[bss < 300] ~ sss[bss < 300])
summary(model5)

plot(bss[bss < 300] ~ sss[bss < 300], xlab = "Species Richness (S)", 
     ylab = "Average Biomass per Trawl Event (kg)")
abline(coefficients(model6))
plot(model6)

  #model 7
    #how important is S 

      #adding date to look at season
dates <- s_rarefac$date
dates <- as.Date(dates, "%m/%d/%Y")
month <- strftime(dates, "%m")
s_rarefac <- cbind(month, s_rarefac)


model7 <- lm(s_rarefac$biomass ~ s_rarefac$S + s_rarefac$lat + s_rarefac$year + 
               s_rarefac$region + s_rarefac$tempB + s_rarefac$salB + s_rarefac$month)
summary(model7)
plot(model7)

plot(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat + s_rarefac$year + 
       s_rarefac$region + s_rarefac$tempB + s_rarefac$salB + s_rarefac$month)
 

 #model 7 AOV
    ## make this into table for poster 
model7aov <- aov(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$lat 
                 + s_rarefac$year + s_rarefac$region + s_rarefac$tempS 
                 + s_rarefac$salS + s_rarefac$month)
summary(model7aov)
plot(model7aov)
AIC(model7aov)

omega_sq(model7aov)


  #model 8
model8 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$year)
summary(model8)
plot(model8)

model8aov <- aov(log(s_rarefac$biomass) ~ s_rarefac$S + s_rarefac$year)
summary(model8aov)
AIC(model8aov)


  #model 9
model9 <- lm(log(s_rarefac$biomass) ~ s_rarefac$S)
summary(model9)

model9aov <- aov(log(s_rarefac$biomass) ~ s_rarefac$S)
summary(model9aov)
AIC(model9aov)
summary(anova(model8aov, model9aov))

  #model 10 
model10 <- lm(log(s_rarefac$biomass) ~ s_rarefac$lat + s_rarefac$year + 
                s_rarefac$region + s_rarefac$tempS + s_rarefac$salS + s_rarefac$month)
summary(model10)

model10AOV <- aov(log(s_rarefac$biomass) ~ s_rarefac$lat + s_rarefac$year + 
                    s_rarefac$region + s_rarefac$tempS + s_rarefac$salS + s_rarefac$month)
summary(model10AOV)



####CALCULATING sPIE####
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




####CHECKING FOR SPATIAL DEPENDENCE####
    #calculate dist of community matrix with species richness inside
s_comm <- read.csv("~./fish_stability/data/s_comm.csv", header = T)
s_environ <- read.csv("~./fish_stability/data/s_environ.csv", header = T)

sr = apply(s_comm, 1, function(x) sum(x > 0))
hist(sr)

sr_dist = dist(sr)
latlong <- s_environ[,6:7]
xy_dist = dist(latlong)
max_dist = max(xy_dist) / 2

  #plot result
plot(xy_dist, sr_dist)
abline(lm(sr_dist ~ xy_dist), lwd=3, col='red')
lines(lowess(xy_dist, sr_dist), lwd=3, col='pink')
abline(v = max_dist, col='red', lwd=3, lty=2)

mantel(xy_dist, sr_dist)
#output of test is correlation is zero and not significantly different from zero. 






####AFTER RASTERIZING - ONE YEAR BIN#### 
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


####AFTER RASTERIZING- THREE YEAR BIN####

    # var biomass vs average species richness per raster region
S_ID <- with(yrag_sub, tapply(averageS, list(ID), mean, na.rm = T))
S_ID
S_ID <- signif(S_ID, digits = 2)
S_ID
B_VAR_ID <- with(yrag_sub, tapply(averagebio,list(ID),sd, na.rm = T))
plot(B_VAR_ID ~ S_ID)


    # Biomass SD vs Average Species Richness with three year bin ### on poster ###
yrag_sub$roundS <- signif(yrag_sub$averageS, digits = 2)
yrag_bss_raster <- with(yrag_sub, tapply(averagebio, list(roundS),mean, na.rm= T))
yrag_bss_raster
yrag_sss_raster <- c(10, 12:32, 35)
plot(yrag_bss_raster ~ yrag_sss_raster)

model10yr <- lm(yrag_bss_raster ~ yrag_sss_raster)
summary(model10yr)
plot(model10yr)

plot(yrag_bss_raster ~ yrag_sss_raster, ylab = "Average Biomass per Raster Region (kg)",
     xlab = "Species Richness (S)", cex = 1.5)
abline(model10yr$coefficients, lwd = 2.5)

  # Biomass SD as a function of Species Richness SD 
    
S_SD <- with(yrag_sub, tapply(averageS, list(ID), sd, na.rm = T))
S_SD
S_SD <- signif(S_SD, digits = 2)
S_SD
B_VAR_ID

      #areas where species richness is more variable, biomass is more variable
model11 <- lm(B_VAR_ID ~ S_SD)
summary(model11)
plot(model11)

plot(B_VAR_ID ~ S_SD, xlab = "SD of Species Richness", ylab = "SD Biomass", 
     cex = 1.5, col = S_AV)
abline(model11$coefficients, lwd = 2.5)

  #making plot above species rich var by biomass var with ggplot2

data <- as.data.frame(cbind(S_SD, S_AV, B_VAR_ID))
dat <- as.data.frame(cbind(S_SD, B_VAR_ID))

ggplot(data = data, aes(x = S_SD, y = B_VAR_ID, fill = S_AV)) +
  geom_point(size = 8, shape = 21) +
  geom_smooth(data = dat, method = "lm" ) +
  xlab("Species Richness SD") +
  ylab("Biomass SD") +
  theme_bw()


