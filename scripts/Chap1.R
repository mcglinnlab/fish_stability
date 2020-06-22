library(ggplot2)
library(dplyr)
library(tidyr)
library(lmodel2)
library(sjstats)

#Organizing BEF analysis into single script with graphs and analysis

  #### PRE RASTERIZATION - AT TRAWL EVENT LEVEL ####
#data sets
  #DATA SET ONE
  #raw data before aggregation by eventname
  SEAMAP_inner
  SEAMAP_inner <- SEAMAP_inner[,-1]
  
  
  #DATA SET TWO
#7,412 events. data aggregated by event 
s_environ <- read.csv("~/fish_stability/data/s_environ.csv", header = T)
s_environ <- s_environ[,-1]
  
#adding month column
  dates <- s_environ$date
  dates <- as.Date(dates, "%m/%d/%Y")
  month <- strftime(dates, "%m")
  s_environ <- cbind(month, s_environ)

#plots
  
  plot(s_environ$biomass ~ s_environ$S, xlab = "S", ylab = "biomass (kg)")
  
  plot(s_environ$biomass ~ s_environ$year, xlab = "year", ylab = "biomass (kg)")
  
  plot(s_environ$biomass ~ s_environ$yrcat, xlab = "yr cat", ylab = "biomass (kg)")
  
  
#models and plots
  #all parameters included
  model7 <- with(s_environ, lm(log(biomass) ~ S + as.numeric(lat)
               + as.numeric(year) +  region + tempB + salB + month))
  summary(model7)
  plot(model7)
  
  with(s_environ, plot(log(biomass) ~ S + lat + year + region + tempB + salB
                       + month))
  
  
  #model 7 AOV
  ## used for AFS poster
  model7aov <- with(s_environ, aov(log(biomass) ~ S + lat + year + region + tempS 
                   + salS + month))
  summary(model7aov)
  plot(model7aov)
  AIC(model7aov)
  
  omega_sq(model7aov)
  
  
  #model 8 - isolating top two parameters (S and year)
  model8 <- with(s_environ, lm(log(biomass) ~ S + year))
  summary(model8)
  plot(model8)
  
  model8aov <- with(s_environ, aov(log(biomass) ~ S + year))
  summary(model8aov)
  AIC(model8aov)
  
  
  #model 9 - just S 
  model9 <- with(s_environ, lmodel2(log(biomass) ~ S))
  model9
  summary(model9)
  
  model9aov <- with(s_environ, aov(log(biomass) ~ S))
  summary(model9aov)
  AIC(model9aov)
  summary(anova(model8aov, model9aov))
  
  
  #model 10 - removing just S  
  model10 <- with(s_environ, lm(log(biomass) ~ lat + year + region + tempS + salS
                                + month))
  summary(model10)
  
  model10AOV <- with(s_environ, aov(log(biomass) ~ lat + year + region + tempS + 
                                      salS + month))
  summary(model10AOV)
  
  
  
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

  
  
  #### SINGLE TRAWL AVERAGE RASTER LEVEL - THREE YEAR TIME BIN ####
  
  #DATA SET ONE
#before removing rasters that don't meet 5 events for all time bins ('good IDs')
  yrag_resultsfull
  
  #DATA SET TWO
#only 36 'good' IDs - where at least 5 events exist in each time bin 
yrag_sub <- read.csv("~/fish_stability/data/yrag_sub.csv", header = T)
yrag_sub <- yrag_sub[, -1]

s_environ
environ_dat <- s_environ %>%
  group_by(ID, yrcat) %>%
  summarize(tempS = mean(tempS),
            salS = mean(salS),
            salB = mean(salB),
            numtotsum = sum(numtotal),
            numtotmean = mean(numtotal)
  )

IDlist <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
            1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
            2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
            2670, 2721, 2771)

#good ID for environ data set
environ_dat <- environ_dat[environ_dat$ID %in% IDlist, ]  


#biologic  
  B_AV <- with(yrag_sub, tapply(averagebio, list(ID), mean))
  S_AV <- with(yrag_sub, tapply(averageS, list(ID), mean))
  S_SD <- with(yrag_sub, tapply(averageS, list(ID), sd))
  B_VAR <- with(yrag_sub, tapply(averagebio, list(ID), function(x)(var(x)/ (mean(x)^2))))
  B_SD <- with(yrag_sub, tapply(averagebio, list(ID), sd))
  B_invar <- 1/B_VAR

#environmental 
  TEMP <- with(environ_dat, tapply(tempS, list(ID), mean, na.rm = T))
  SALB <- with(environ_dat, tapply(salB, list(ID), mean, na.rm =T))
  SALS <- with(environ_dat, tapply(salS, list(ID), mean, na.rm = T))
  NUMTOTS <- with(environ_dat, tapply(numtotsum, list(ID), mean, na.rm =T))
  NUMTOTM <- with(environ_dat, tapply(numtotmean, list(ID), mean, na.rm = T))
  centercoord #lat long of centroid of 36 regions
  LAT <- centercoords$y
  
##BIOLOGICAL COMPARISONS ####  
  
  
#B_VAR ~ S_AV
  model12 <- lmodel2(B_VAR ~ S_AV, nperm = 100)
  model12
  plot(model12, "SMA")
  
  plot(B_VAR ~ S_AV, xlab = "Average Species Richness per Raster Region (S)", 
       ylab = "Biomass Var per Raster Region (kg)", cex = 1.5)
  abline(a = model12$regression.results$Intercept[3],
         b = model12$regression.results$Slope[3], lwd = 2.5)
  cor(B_VAR, S_AV)
  
#B_invar ~ S_AV
  model15 <- lmodel2(B_invar ~ S_AV, nperm = 100)
  model15
  plot(model15, "SMA")
  
  plot(B_invar ~ S_AV, xlab = "Average Species Richness(S)", 
       ylab = "Biomass invar (kg)", cex = 1.5)
  abline(a = model15$regression.results$Intercept[3],
         b = model15$regression.results$Slope[3], lwd = 2.5)
  cor(B_invar, S_AV)
  
#S_SD ~ S_A
  model13 <- lmodel2(S_SD ~ S_AV, nperm = 100)
  model13
  plot(model13, "SMA")
  
  plot(S_SD ~ S_AV, xlab = "Average Species Richness per Raster Region (S)"
       , ylab = "Species Richness SD per Raster Region (S)", cex = 1.5)
  abline(a = model13$regression.results$Intercept[3],
         b = model13$regression.results$Slope[3], lwd = 2.5)
  cor(S_SD, S_AV)
  
#B_AV ~ S_AV
  model14 <- lmodel2(B_AV ~ S_AV, nperm = 100)
  model14
  plot(model14, "SMA")
  
  plot(B_AV ~ S_AV, xlab = "Average Species Richness", ylab = "Average Biomass (kg)",
       cex = 1.5)
  abline(a = model14$regression.results$Intercept[3],
         b = model14$regression.results$Slope[3], lwd = 2.5)
  cor(B_AV, S_AV)
  
#B_VAR ~ S_SD with fill S_AV
  data <- as.data.frame(cbind(S_SD, S_AV, B_VAR))
  dat <- as.data.frame(cbind(S_SD, B_VAR))
  
  ggplot(data = data, aes(x = S_SD, y = B_VAR, fill = S_AV)) +
    geom_point(size = 8, shape = 21) +
    geom_smooth(data = dat, method = "" ) +
    xlab("Species Richness SD") +
    ylab("Biomass Var") +
    theme_bw()
  
#B_SD ~ S_SD fill S_AV
  data <- as.data.frame(cbind(S_SD, S_AV, B_SD))
  dat <- as.data.frame(cbind(S_SD, B_SD))
  
  ggplot(data = data, aes(x = S_SD, y = B_SD, fill = S_AV)) +
    geom_point(size = 8, shape = 21) +
    geom_smooth(data = dat, method = "lm" ) +
    xlab("Species Richness SD") +
    ylab("Biomass SD") +
    theme_bw()
  
  
#how to plot RMA with ggplot
  reg <- mod$regression.results
  names(reg) <- c("method", "intercept", "slope", "angle", "p-value")
  ggplot(dat) + 
    geom_point(aes(b, a)) +
    geom_abline(data = reg, 
                aes(intercept = intercept, slope = slope, colour = method))
  
  
### ENVIRONMENTAL COMPARISONS ####
  
  #B_AV ~ LAT 
  model16 <- lm(B_AV ~ LAT)
  summary(model16)
  plot(model16)
  
  plot(B_AV ~ LAT, xlab = "Raster Centroid Latitude", ylab = "Average Biomass (kg)",
       cex = 1.5)
  lines(lowess(LAT, B_AV), col = 2, lwd = 3)
  abline(model16$coefficients, lwd = 2.5)  
  cor(B_AV, LAT)

  #S_AV ~ LAT 
  model17 <- lm(S_AV ~ LAT)
  summary(model17)
  plot(model17)
  
  plot(S_AV ~ LAT, xlab = "Raster Centroid Latitude", 
       ylab = "Average Species Richness", cex = 1.5)
  lines(lowess(LAT, S_AV), col = 2, lwd = 3)
  abline(model17$coefficients, lwd = 2.5)
  cor(S_AV, LAT)
  
  #B_invar ~ LAT
  model18 <- lm(B_invar ~ LAT)
  summary(model18)
  plot(model18)
  
  plot(B_invar ~ LAT, xlab = "Raster Centroid Latitude", 
       ylab = "Stabiltiy (1/var)", cex = 1.5)
  lines(lowess(LAT, B_invar), col = 2, lwd = 3)
  abline(model18$coefficients, lwd = 2.5)  
  cor(B_invar, LAT)
  
  #B_invar ~ tempS
  model19 <- lmodel2(B_invar ~ TEMP)
  model19
  plot(model19)
  
  plot(B_invar ~ TEMP, xlab = "Average Surface Temp", 
       ylab = "Stabiltiy (1/var)", cex = 1.5)
  lines(lowess(TEMP, B_invar), col = 2, lwd = 3)
  abline(model19$coefficients, lwd = 2.5)
  cor(B_invar, TEMP)
  
  #B_AV ~ tempS
  model20 <- lm(B_AV ~ TEMP)
  summary(model20)
  plot(model20)
  
  plot(B_AV ~ TEMP, xlab = "Average Surface Temp", 
       ylab = "Biomass (kg)", cex = 1.5)
  lines(lowess(TEMP, B_AV), col = 2, lwd = 3)
  abline(model20$coefficients, lwd = 2.5)
  cor(B_AV, TEMP)
  
plot(TEMP ~ LAT, xlab = "Raster Centroid Latitude", ylab = "Surface Temp",
     cex = 1.5)
lines(lowess(LAT, TEMP), col = 2, lwd = 3)
cor(TEMP, LAT)

plot(S_AV ~ NUMTOTM, xlab = "Mean Total Number Individuals per Raster Subsample ",
     ylab = "average S", cex = 1.5)
lines(lowess(NUMTOTM, S_AV), col = 2, lwd = 3)
cor(S_AV, NUMTOTM)

plot(S_AV ~ NUMTOTS, xlab = "Total Number Individuals per Reaster", 
     ylab = "average S", cex = 1.5)
lines(lowess(NUMTOTS, S_AV), col = 2, lwd = 3)
cor(S_AV, NUMTOTS)

plot(B_AV ~ NUMTOTS, xlab = "Total Number of Individuals per Raster", 
     ylab = "Average Biomass", cex = 1.5)
lines(lowess(NUMTOTS, B_AV), col = 2, lwd = 3)
cor(B_AV, NUMTOTS)

plot(B_AV ~ NUMTOTM, xlab = "Mean Total Number Individuals", 
     ylab = "Average Biomass (kg)", cex = 1.5)
lines(lowess(NUMTOTM, B_AV), col = 2, lwd = 3)
cor(B_AV, NUMTOTM)

plot(B_invar ~ NUMTOTM, xlab = "Mean Total Number Individuals", 
     ylab = "Stability (1/var)", cex = 1.5)
lines(lowess(NUMTOTM, B_invar), col = 2, lwd = 3)
cor(B_invar, NUMTOTM)

plot(B_invar ~ NUMTOTS, xlab = "Total Number of Individuals per Raster", 
     ylab = "Stability (1/var)", cex = 1.5)
lines(lowess(NUMTOTS, B_invar), col = 2, lwd = 3)
cor(B_invar, NUMTOTS)

plot(NUMTOTM ~ LAT, xlab = "Raster Centroid Latitude", 
     ylab = "Mean Total Number Individuals", cex = 1.5)
lines(lowess(LAT, NUMTOTM), col = 2, lwd = 3)
cor(NUMTOTM, LAT)  

  #### COMPLETE RASTER ESTIMATE LEVEL - THREE YEAR TIME BIN ####
#community matrix to calc biomass values
  rastercom_mat_bio
  
#calc new S
  Smatrix <- rastercom_mat_bio[, -1]
  Smatrix[Smatrix >0] <- 1
  newS <- rowSums(Smatrix)
#calc new B
  newB <- rowSums(rastercom_mat_bio[,-1])
#storage
  ID <- rastercom_mat_bio$uniqueID
  full_raster_est_SB <- cbind.data.frame(ID, newS, newB)
  full_raster_est_SB <- full_raster_est_SB %>%
    separate(ID, c("ID", "yr_cat"))
  
#calcs by ID
  B_AV2 <- with(full_raster_est_SB, tapply(newB, list(ID), mean))
  S_AV2 <- with(full_raster_est_SB, tapply(newS, list(ID), mean))
  S_SD2 <- with(full_raster_est_SB, tapply(newS, list(ID), sd))
  B_VAR2 <- with(full_raster_est_SB, tapply(newB, list(ID), function(x)(var(x)/ (mean(x)^2))))
  B_SD2 <- with(full_raster_est_SB, tapply(newB, list(ID), sd))
  B_invar2 <- 1/B_VAR2
  
#graphs and models
  #B_VAR ~ S_AV
  model122 <- lmodel2(B_VAR2 ~ S_AV2, nperm = 100)
  model122
  plot(model122)
  
  plot(B_VAR2 ~ S_AV2, xlab = "Average Species Richness per Raster Region (S)", 
       ylab = "Biomass Var per Raster Region (kg)", cex = 1.5)
  abline(model122$coefficients, lwd = 2.5)
  
  #B_invar ~ S_AV
  model152 <- lmodel2(B_invar2 ~ S_AV2, nperm = 100)
  model152
  plot(model152)
  
  plot(B_invar2 ~ S_AV2, xlab = "Average Species Richness(S)", 
       ylab = "Biomass invar (kg)", cex = 1.5)
  abline(model152$coefficients, lwd = 2.5)
  
  #S_SD ~ S_AV
  model13 <- lmodel2(S_SD2 ~ S_AV2, nperm = 100)
  model132
  plot(model132)
  
  plot(S_SD2 ~ S_AV2, xlab = "Average Species Richness per Raster Region (S)"
       , ylab = "Species Richness SD per Raster Region (S)", cex = 1.5)
  abline(model132$coefficients, lwd = 2.5)
  
  #B_AV ~ S_AV
  model142 <- lmodel2(B_AV2 ~ S_AV2, nperm = 100)
  model142
  plot(model142)
  
  plot(B_AV2 ~ S_AV2, xlab = "Average Species Richness", ylab = "Average Biomass (kg)",
       cex = 1.5)
  abline(model142$coefficients, lwd = 2.5)
  
  #B_VAR ~ S_SD with fill S_AV
  data <- as.data.frame(cbind(S_SD2, S_AV2, B_VAR2))
  dat <- as.data.frame(cbind(S_SD2, B_VAR2))
  
  ggplot(data = data, aes(x = S_SD2, y = B_VAR2, fill = S_AV2)) +
    geom_point(size = 8, shape = 21) +
    geom_smooth(data = dat, method = "lm" ) +
    xlab("Species Richness SD") +
    ylab("Biomass Var") +
    theme_bw()
  
  #B_SD ~ S_SD fill S_AV
  data <- as.data.frame(cbind(S_SD2, S_AV2, B_SD2))
  dat <- as.data.frame(cbind(S_SD2, B_SD2))
  
  ggplot(data = data, aes(x = S_SD2, y = B_SD2, fill = S_AV2)) +
    geom_point(size = 8, shape = 21) +
    geom_smooth(data = dat, method = "lm" ) +
    xlab("Species Richness SD") +
    ylab("Biomass SD") +
    theme_bw()
  
  
  
  
  ####TAKE 2 ####
  
  #sub only scale 1
  Smatrix <- presrun1_20[presrun1_20$scale== "1" , ]
  Bmatrix <- biorun1_20[biorun1_20$scale == "1", ]
  
  #calculating new S and bio
  identifier <- Smatrix[, 1:5]
  Smatrix <- Smatrix[, 6:205]
  newS <- rowSums(Smatrix)
  
  identifier <- Bmatrix[, 1:5]
  Bmatrix <- Bmatrix[, 6:205]
  newbio <- rowSums(Bmatrix)
  
  BEF2 <- cbind(identifier, newS, newbio)
  
#averaging across boot iterations  
  BEF3 <- BEF2 %>%
    group_by(startID, yr_cat) %>%
  summarize(S = mean(newS),
            bio = mean(newbio),
            Ssd = sd(newS),
            biosd = sd(newbio))
  
#getting surface temp and surface salinity
  environ <- s_environ %>%
    group_by(ID, yrcat) %>%
    summarize(tempS = mean(tempS, na.rm = T),
              salS = mean(salS, na.rm =T))
  
  environ <- environ[environ$ID %in% IDlist, ]  
  environ <- environ[, 3:4]
  
 #creating data frame for biomass ~ S + others model
BEF <- as.data.frame(cbind(BEF2$startID, BEF3$yr_cat, BEF3$S, BEF3$bio, BEF3$Ssd,
                           BEF3$biosd, environ$tempS, environ$salS, rasterlat))
names(BEF) <- c("ID", "yr_cat", "S", "bio", "Sbootsd", "biobootsd", "tempS",
                "salS", "rasterlat")
  
#plots
plot(log2(BEF$bio) ~ log2(BEF$S))  


  
#collapsing over time bins

tempvar <- function(x) {
  var(x)/ (mean(x)^2)
}

BEFovTime <- BEF %>%
  group_by(ID) %>%
  summarize(Srich = mean(S),
            Bio = mean(bio),
            StimeVar = sd(S),
            biotimeVar = var(bio)/ (mean(bio) ^2),
            stability = 1/biotimeVar,
            stab = (mean(bio) ^2)/ var(bio), 
            tempS = mean(tempS),
            salS = mean(salS),
            rasterlat = unique(rasterlat)
            )
    
  #plots
with(BEFovTime, plot(log2(Bio) ~ log2(Srich)))
bioModel <- with(BEFovTime, lm(log2(Bio) ~ log2(Srich) + rasterlat + tempS + salS))
summary(bioModel)

with(BEFovTime, plot((stability) ~ (Srich)))
stabModel <- with(BEFovTime, lm((stability) ~ (Srich) + rasterlat + tempS + salS))
summary(stabModel)

summary(lm(stability ~ Srich, data = BEFovTime))


tempresults <- NULL
output_res <- NULL

for (i in 1:20) {
  step1 <- BEF2[BEF2$boot == i, ]
  step2 <- step1 %>%
    group_by(startID) %>%
    summarize(boot = unique(boot),
              S = mean(newS),
              bio = mean(newbio),
              varbio = var(newbio)/(mean(newbio) ^2),
              stabbio = (mean(newbio)^2) / var(newbio),
              varS = var(newS)
              )
  tempresults <- step2
  output_res <- rbind(output_res, tempresults)
}


final_output <- output_res %>% 
  group_by(startID) %>%
  summarize(Srich = mean(S),
            Bio = mean(bio),
            avbootvarbio = mean(varbio),
            avbootstabbio = mean(stabbio),
            avvarS = mean(varS))

final_output <- cbind(final_output, BEFovTime$rasterlat, BEFovTime$tempS, BEFovTime$salS)
names(final_output) <- c("startID", "Srich", "Bio", "avbootvarbio", "avbootstabbio",
                         "avvarS", "rasterlat", "tempS", "salS")


stabModel <- with(final_output, lm(log2(stability) ~ log2(Srich) + rasterlat + tempS + salS))
summary(stabModel)
stabModel <- with(final_output, lm(log2(avbootstabbio) ~ log2(Srich)))
summary(stabModel)

bioModel <- with(final_output, lm(log2(Bio) ~ log2(Srich) + rasterlat + tempS + salS))
summary(bioModel)
bioModel <- with(final_output, lm(log2(Bio) ~ log2(Srich)))
summary(bioModel)


with(final_output, plot(Bio ~ rasterlat))
with(final_output, plot(avbootstabbio ~ rasterlat))
with(final_output, plot(avbootstabbio ~ Srich))
with(final_output, lines(lowess(avbootstabbio ~ Srich)))
with(final_output, plot(Bio ~ tempS))
with(final_output, plot(avbootstabbio ~ tempS))


#### FIGURE 1 -  4 Panel maps  ####
library(tmap)
library(tmaptools)

#a) number of trawls per pixel - uses ID.df
ID.df$identity <- 1

trawl_num <- ID.df %>%
  group_by(point2resID) %>%
  summarize(trawlnum = sum(identity))

trawl_num <- trawl_num[trawl_num$point2resID %in% IDlist, ]
trawl_num <- cbind(trawl_num, raster_cord$x, raster_cord$y)
names(trawl_num) <- c("ID", "trawlnum", "long", "lat")

Trawl_raster <- rasterize(trawl_num[,3:4], oc_raster, trawl_num$trawlnum)
res(Trawl_raster)
plot(Trawl_raster)

Trawl_raster <- crop(x = Trawl_raster, y = new.extent)

tm_shape(Trawl_raster) +
  tm_raster(title = "") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()


#b) average rarefied richness to 5 trawls
Species_Raster <- rasterize(trawl_num[, 3:4], oc_raster, final_output$Srich)
res(Species_Raster)
plot(Species_Raster)

Species_Raster <- crop( x = Species_Raster, y = new.extent)

tm_shape(Species_Raster) +
  tm_raster(title = "", palette = "Blues") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#c) average total biomass 5 trawls
Biomass_Raster <- rasterize(trawl_num[, 3:4], oc_raster, final_output$Bio)
res(Biomass_Raster)
plot(Biomass_Raster)

Biomass_Raster <- crop(x = Biomass_Raster, y = new.extent)

tm_shape(Biomass_Raster) +
  tm_raster(title = "", palette = "Greens") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#d) average stability biomass at 5 trawls
Stability_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                              final_output$avbootstabbio)
res(Stability_Raster)
plot(Stability_Raster)

Stability_Raster <- crop(x = Stability_Raster, y = new.extent)

tm_shape(Stability_Raster) +
  tm_raster(title = "", palette = "PuRd") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()




#FIGURE 2 - Multiple Regression Results

  #biomass
bioModel <- lm(log2(Bio) ~ log2(Srich) + rasterlat + tempS +
                                    salS, data = final_output)
summary(bioModel)
#standardized regression coefficients
lm.beta(bioModel)
plot(bioModel)

  #stability
stabModel <- lm(log2(avbootstabbio) ~ log2(Srich) + rasterlat +
                                     tempS + salS, data = final_output)
summary(stabModel)
#standardized regression coefficients
lm.beta(stabModel)
plot(stabModel)



#FIGURE 3 - Partial Regression and Lowess smoother - 6 panel

#a) biomass ~ richness
termplot(bioModel, terms = "log2(Srich)", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-2, 2))
axis(side=1, cex.axis=2, at = seq(35, 55, 1))
axis(side=2, cex.axis=2)
res = residuals(bioModel, 'partial')
res = res[ , 'log2(Srich)', drop=FALSE]
lines(lowess((final_output$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(stabModel, terms = "log2(Srich)", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-2, 2))
axis(side=1, cex.axis=2, at = seq(35, 55, 1))
axis(side=2, cex.axis=2)
res = residuals(stabModel, 'partial')
res = res[ , 'log2(Srich)', drop=FALSE]
lines(lowess((final_output$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ latitude
termplot(bioModel, terms = "rasterlat", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-1, 1))
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2)
res = residuals(bioModel, 'partial')
res = res[ , 'rasterlat', drop=FALSE]
lines(lowess(final_output$rasterlat, res), col='red', lty=2, lwd=5)

#d) stability ~ latitude
termplot(stabModel, terms = "rasterlat", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-2, 2))
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2)
res = residuals(stabModel, 'partial')
res = res[ , 'rasterlat', drop=FALSE]
lines(lowess(final_output$rasterlat, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-1, 1))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2)
res = residuals(bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(final_output$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-2, 2))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2)
res = residuals(stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(final_output$tempS, res), col='red', lty=2, lwd=5)


