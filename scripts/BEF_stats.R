library(dplyr)
library(tidyr)
library(raster)
library(tmap)
library(tmaptools)
library(rgdal)
library(QuantPsyc)
library(USAboundaries)

#### BEF SCALE ONE ANALYSIS ####

#DATA SETS
  #SUMMARY_BEF - 324,000  rows, 1,000 iterations, 36 rasters, 9 time bins
summary_BEF <- read.csv("~/fish_stability/data/BEF/summary_BEF.csv", header = T)
summary_BEF <- summary_BEF[, -1]

  #ID.df - Information about each raster IDs
ID.df <- read.csv("~/fish_stability/data/ID.df.csv", header = T)
ID.df <- ID.df[, -1]

  #ID list - list of good raster IDs
IDlist <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
            1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
            2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
            2670, 2721, 2771)
  #rasterID_coords - coordinate of center of raster region
rasterID_coord <- read.csv("~/fish_stability/data/rasterID_coord.csv", header = T)
rasterID_coord <- rasterID_coord[, -1]


  #Ocean and Continents Shape File
  # read in the ocean
oceans <- readOGR(dsn = "./shapefiles/ocean_raster", layer = "ne_10m_ocean")
  # create a global raster layer
oceans <- spTransform(oceans, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))
oceans_raster <- raster(oceans)
  #Setting raster resolution
  # res in units of decimal degrees ----- 0.1 decimal degrees ~ 11.132 km
res(oceans_raster) <- .2
  #crop extent of the oceans raster
extent_oc <- extent(-85,-75,25,40)
oc_raster <- crop(oceans_raster, extent_oc)
  # making continents polygon  
continents <- shapefile('./shapefiles/continent/continent/continent.shp')
continents <- spTransform(continents, CRS("+proj=longlat +lat_0=32.4 +lon_0=-79.6"))
  #states
states <- us_states()

#### Averaging across bootstrap iterations ####

#biologic  
  #average over bootstraps
b_av <- with(summary_BEF, tapply(biomass, list(unique_ID), mean))
S_av <- with(summary_BEF, tapply(S, list(unique_ID), mean))
Spie_av <- with(summary_BEF, tapply(sPIE, list(unique_ID), mean))
sN_av <- with(summary_BEF, tapply(s_N, list(unique_ID), mean))
Nind_av <- with(summary_BEF, tapply(Nind, list(unique_ID), mean))
b_shrimp_av <- with(summary_BEF, tapply(shrimp_bio, list(unique_ID), mean))
b_flounder_av <- with(summary_BEF, tapply(flounder_bio, list(unique_ID), mean))


  #sd over bootstraps
S_bootsd <- with(summary_BEF, tapply(S, list(unique_ID), sd))
B_bootsd <- with(summary_BEF, tapply(biomass, list(unique_ID), sd))
Spie_bootsd <- with(summary_BEF, tapply(sPIE, list(unique_ID), sd))
sN_bootsd <- with(summary_BEF, tapply(s_N, list(unique_ID), sd))
Nind_bootsd <- with(summary_BEF, tapply(Nind, list(unique_ID), sd))
b_shrimp_bootsd <- with(summary_BEF, tapply(shrimp_bio, list(unique_ID), sd))
b_flounder_bootsd <- with(summary_BEF, tapply(flounder_bio, list(unique_ID), sd))




#environmental
  #average over bootstraps
tempS <- with(summary_BEF, tapply(tempS, list(unique_ID), mean, na.rm = T))
tempB <- with(summary_BEF, tapply(tempB, list(unique_ID), mean, na.rm = T))
salB <- with(summary_BEF, tapply(salB, list(unique_ID), mean, na.rm =T))
salS <- with(summary_BEF, tapply(salS, list(unique_ID), mean, na.rm = T))

  #sd over bootstraps
tempSsd <- with(summary_BEF, tapply(tempS, list(unique_ID), sd, na.rm = T))
tempBsd <- with(summary_BEF, tapply(tempB, list(unique_ID), sd, na.rm = T))
salBsd <- with(summary_BEF, tapply(salB, list(unique_ID), sd, na.rm =T))
salSsd <- with(summary_BEF, tapply(salS, list(unique_ID), sd, na.rm = T))




BEF <- as.data.frame(cbind(b_av, S_av, Spie_av, sN_av, Nind_av, b_shrimp_av,
                           b_flounder_av, B_bootsd, S_bootsd, Spie_bootsd, 
                           sN_bootsd, Nind_bootsd, b_shrimp_bootsd, b_flounder_bootsd, 
                           tempS, tempB, salB, salS, tempSsd, tempBsd, salBsd, salSsd))
BEF$unique_ID <- rownames(BEF)

BEF <- BEF %>%
  separate(unique_ID, c("ID", "yr_cat"))

#### Averaging across time bins ####
#biologic  
  #average over bootstraps
b_yrav <- with(BEF, tapply(b_av, list(ID), mean))
S_yrav <- with(BEF, tapply(S_av, list(ID), mean))
Spie_yrav <- with(BEF, tapply(Spie_av, list(ID), mean))
sN_yrav <- with(BEF, tapply(sN_av, list(ID), mean))
Nind_yrav <- with(BEF, tapply(Nind_av, list(ID), mean))
b_shrimp_yrav <- with(BEF, tapply(b_shrimp_av, list(ID), mean))
b_flounder_yrav <- with(BEF, tapply(b_flounder_av, list(ID), mean))


  #sd over bootstraps
S_yrsd <- with(BEF, tapply(S_av, list(ID), sd))
B_yrsd <- with(BEF, tapply(b_av, list(ID), sd))
B_yrvar <- with(BEF, tapply(b_av, list(ID), function(x)(var(x)/ (mean(x)^2))))
B_yrstab <- 1/ B_yrvar
Spie_yrsd <- with(BEF, tapply(Spie_av, list(ID), sd))
sN_yrsd <- with(BEF, tapply(sN_av, list(ID), sd))
Nind_yrsd <- with(BEF, tapply(Nind_av, list(ID), sd))
b_shrimp_yrsd <- with(BEF, tapply(b_shrimp_av, list(ID), sd))
b_shrimp_yrvar <- with(BEF, tapply(b_shrimp_av, list(ID), function(x)(var(x)/ (mean(x)^2))))
b_shrimp_yrstab <- 1/ b_shrimp_yrvar
b_flounder_yrsd <- with(BEF, tapply(b_flounder_av, list(ID), sd))
b_flounder_yrvar <- with(BEF, tapply(b_flounder_av, list(ID), function(x)(var(x)/ (mean(x)^2))))
b_flounder_yrstab <- 1/ b_flounder_yrvar



#environmental
  #average over bootstraps
tempS_yr <- with(BEF, tapply(tempS, list(ID), mean, na.rm = T))
tempB_yr <- with(BEF, tapply(tempB, list(ID), mean, na.rm = T))
salB_yr <- with(BEF, tapply(salB, list(ID), mean, na.rm =T))
salS_yr <- with(BEF, tapply(salS, list(ID), mean, na.rm = T))

  #sd over bootstraps
tempS_yrsd <- with(BEF, tapply(tempS, list(ID), sd, na.rm = T))
tempB_yrsd <- with(BEF, tapply(tempB, list(ID), sd, na.rm = T))
salB_yrsd <- with(BEF, tapply(salB, list(ID), sd, na.rm =T))
salS_yrsd <- with(BEF, tapply(salS, list(ID), sd, na.rm = T))



BEF_yr <- as.data.frame(cbind(b_yrav, B_yrvar, B_yrstab, S_yrav, Spie_yrav, sN_yrav, 
                           Nind_yrav, b_shrimp_yrav, b_shrimp_yrstab, b_flounder_yrav,
                           b_flounder_yrstab, B_yrsd, S_yrsd, Spie_yrsd, sN_yrsd,
                           Nind_yrsd, b_shrimp_yrsd,b_flounder_yrsd, tempS_yr,
                           tempB_yr, salB_yr, salS_yr, tempS_yrsd, tempB_yrsd, 
                           salB_yrsd, salS_yrsd))
BEF_yr$ID <- rownames(BEF_yr)







#### SIMPLE GRAPHS ####
with(BEF_yr, plot(b_yrav ~ S_yrav))
with(BEF_yr, plot(log(b_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(B_yrstab) ~ log(S_yrav)))
with(BEF_yr, plot(B_yrstab ~ log(S_yrav)))
with(BEF_yr, plot(log(B_yrstab) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(B_yrstab)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(B_yrstab)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ B_yrstab))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ B_yrstab))
with(BEF_yr, plot(log(b_yrav) ~ tempS_yr))
with(BEF_yr, plot(log(b_yrav) ~ salS_yr))
with(BEF_yr, plot(log(B_yrstab) ~ salS_yr))
with(BEF_yr, plot(log(B_yrstab) ~ tempS_yr))

with(BEF_yr, plot(log(b_shrimp_yrav) ~ tempS_yr))
with(BEF_yr, plot(log(b_flounder_yrav) ~ tempS_yr))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(b_yrav)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(S_yrav)))



mod1 <- with(BEF_yr, lm(log(b_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(S_yrav)))
abline(mod1$coefficients)
summary(mod1)




#### FIGURE 1 - RASTER MAPS ####

#a) number of trawls per raster - uses ID.df
ID.df$identity <- 1

trawl_num <- ID.df %>%
  group_by(rasterID) %>%
  summarize(trawlnum = sum(identity))

trawl_num <- trawl_num[trawl_num$rasterID %in% IDlist, ]
trawl_num <- cbind(trawl_num, rasterID_coord$x, rasterID_coord$y)
names(trawl_num) <- c("ID", "trawlnum", "long", "lat")

#extent of maps
new.extent <- c(-83.5, -75, 27.5, 37)

#36 good raster regions
Trawl_raster <- rasterize(trawl_num[,3:4], oc_raster, trawl_num$trawlnum)
res(Trawl_raster)
plot(Trawl_raster)

Trawl_raster <- crop(x = Trawl_raster, y = new.extent)


tm_shape(Trawl_raster) +
  tm_raster(title = "") +
  tm_layout(legend.text.size = 1, legend.position = c("right", "bottom")) +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#b) average rarefied richness to 5 trawls #Species Richness
Species_Raster <- rasterize(trawl_num[, 3:4], oc_raster, BEF_yr$S_yrav)
res(Species_Raster)
plot(Species_Raster)

Species_Raster <- crop( x = Species_Raster, y = new.extent)

tm_shape(Species_Raster) +
  tm_raster(title = "", palette = "Blues") +
  tm_layout(legend.text.size = 1, legend.position = c("right", "bottom")) +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#c) average total biomass 5 trawls # Fish Productivity
Biomass_Raster <- rasterize(trawl_num[, 3:4], oc_raster, BEF_yr$b_yrav)
res(Biomass_Raster)
plot(Biomass_Raster)

Biomass_Raster <- crop(x = Biomass_Raster, y = new.extent)

tm_shape(Biomass_Raster) +
  tm_raster(title = "", palette = "Greens") +
  tm_layout(legend.text.size = 1, legend.position = c("right", "bottom")) +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#d) average stability biomass at 5 trawls #Stability of Fish Productivity
Stability_Raster <- rasterize(trawl_num[, 3:4], oc_raster, BEF_yr$B_yrstab)
res(Stability_Raster)
plot(Stability_Raster)

Stability_Raster <- crop(x = Stability_Raster, y = new.extent)

tm_shape(Stability_Raster) +
  tm_raster(title = "", palette = "PuRd") +
  tm_layout(legend.text.size = 1, legend.position = c("right", "bottom")) +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()


#### FIGURE 2 - ENVIRONMENTAL MAPS ####
#a) average surface temperature
SurfaceTemp_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                BEF_yr$tempS_yr)
res(SurfaceTemp_Raster)
plot(SurfaceTemp_Raster)

SurfaceTemp_Raster <- crop(x = SurfaceTemp_Raster, y = new.extent)

tm_shape(SurfaceTemp_Raster) +
  tm_raster(title = "", palette = "-RdYlBu") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1, 
            legend.position = c("right", "bottom"))

#b) average surface salinity
SurfaceSal_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                               BEF_yr$salS_yr)
res(SurfaceSal_Raster)
plot(SurfaceSal_Raster)

SurfaceSal_Raster <- crop(x = SurfaceSal_Raster, y = new.extent)

tm_shape(SurfaceSal_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))

#### TABLE 1 #### 

#data prep
#pulling columns from final_output that are used and log transforming bio, stab, and S
moddat <- as.data.frame(cbind(log2(BEF_yr$b_yrav), log2(BEF_yr$B_yrstab), 
                              log2(BEF_yr$b_shrimp_yrav), log2(BEF_yr$b_shrimp_yrstab),
                              log2(BEF_yr$b_flounder_yrav), log2(BEF_yr$b_flounder_yrstab),
                              log2(BEF_yr$S_yrav), as.numeric(BEF_yr$tempS_yr), as.numeric(BEF_yr$salS_yr)))
moddat[] <- sapply(moddat, as.numeric)
moddat <- as.data.frame(cbind(BEF_yr$ID, moddat))
names(moddat) <- c("ID", "F_bio", "F_stab", "Sh_bio", "Sh_stab", "Fl_bio", 
                    "Fl_stab", "Srich", "tempS", "salS")


#Standardized beta coefficient models
  #scaling variables (subtracting mean and dividing by standard deviation)
moddat_S <- as.data.frame(scale(moddat[,-1], center = T, scale = T))
  #adding startID column and renaming columns
moddat_S[] <- sapply(moddat_S, as.numeric)
moddat_S <- as.data.frame(cbind(BEF_yr$ID, moddat_S))
names(moddat_S) <- c("ID", "F_bio", "F_stab", "Sh_bio", "Sh_stab", "Fl_bio", 
                           "Fl_stab", "Srich", "tempS", "salS")


#FISH
  #biomass
    #run model; log transformations built in before scaling step
bioModel_S_fish <- lm(F_bio ~ Srich + tempS + salS, data = moddat_S)
summary(bioModel_S_fish)
    #standardized regression coefficients
lm.beta(bioModel_S_fish)
plot(bioModel_S_fish)
    #raw coefficients
bioModel_fish <- lm(F_bio ~ Srich + tempS + salS, data = moddat)
summary(bioModel_fish)

  #stability
    #model
stabModel_S_fish <- lm(F_stab ~ Srich + tempS + salS, data = moddat_S)
summary(stabModel_S_fish)
    #standardized regression coefficients
lm.beta(stabModel_S_fish)
plot(stabModel_S_fish)
  #raw coefficients
stabModel_fish <- lm(F_stab ~ Srich + tempS + salS, data = moddat)
summary(stabModel_fish)



#SHRIMP
  #also include fish bio - not included in table
modtest_shrimp <- lm(Sh_bio ~ Srich + tempS + salS + F_bio, data = moddat_S)
summary(modtest_shrimp)
lm.beta(modtest_shrimp)

  #biomass
    #run model; log transformations built in before scaling step
bioModel_S_shrimp <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat_S)
summary(bioModel_S_shrimp)
    #standardized regression coefficients
lm.beta(bioModel_S_shrimp)
plot(bioModel_S_shrimp)
  #raw coefficients
bioModel_shrimp <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat)
summary(bioModel_shrimp)


  #stability
    #model
stabModel_S_shrimp <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat_S)
summary(stabModel_S_shrimp)
  #standardized regression coefficients
lm.beta(stabModel_S_shrimp)
plot(stabModel_S_shrimp)
  #raw coefficients
stabModel_shrimp <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat)
summary(stabModel_shrimp)


#FLOUNDER
  #biomass
    #run model; log transformations built in before scaling step
bioModel_S_flounder <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat_S)
summary(bioModel_S_flounder)
    #standardized regression coefficients
lm.beta(bioModel_S_flounder)
plot(bioModel_S_flounder)
  #raw coefficients
bioModel_flounder <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat)
summary(bioModel_flounder)

  #stability
    #model
stabModel_S_flounder <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat_S)
summary(stabModel_S_flounder)
    #standardized regression coefficients
lm.beta(stabModel_S_flounder)
plot(stabModel_S_flounder)
  #raw coefficients
stabModel_flounder <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat)
summary(stabModel_flounder)



#### FIGURE THREE - PARTIAL RESIDUAL PLOTS ####
  
  ##FISH##

  #all variables except temp and salinity previously log2 transformed
F_bioModel <- lm(F_bio ~ Srich + tempS + salS, data = moddat)
summary(F_bioModel)
F_stabModel <- lm(F_stab ~ Srich + tempS + salS, data = moddat)
summary(F_stabModel)

#a) biomass ~ richness
termplot(F_bioModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(5.0, 5.8, 0.1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(F_bioModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(F_stabModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(5.0, 6.0, 0.1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(F_stabModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ salinity
termplot(F_bioModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(31, 36, 1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(F_bioModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat$salS, res), col='red', lty=2, lwd=5)

#d) stability ~ salinity
termplot(F_stabModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(F_stabModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat$salS, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(F_bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(F_bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(F_stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(F_stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat$tempS, res), col='red', lty=2, lwd=5)


  ##SHRIMP##

#all variables except temp and salinity previously log2 transformed
Sh_bioModel <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat)
summary(Sh_bioModel)
Sh_stabModel <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat)
summary(Sh_stabModel)




#a) biomass ~ richness
termplot(Sh_bioModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-5, 3))
axis(side=1, cex.axis=2, at = seq(5.0, 5.8, 0.1))
axis(side=2, cex.axis=2, at = seq(-5.0, 3.0, 1))
res = residuals(Sh_bioModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(Sh_stabModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-5, 3))
axis(side=1, cex.axis=2, at = seq(5.0, 6.0, 0.1))
axis(side=2, cex.axis=2, at = seq(-5.0, 3.0, 1))
res = residuals(Sh_stabModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ salinity
termplot(Sh_bioModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-5, 3))
axis(side=1, cex.axis=2, at = seq(31, 36, 1))
axis(side=2, cex.axis=2, at = seq(-5.0, 3.0, 1))
res = residuals(Sh_bioModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat$salS, res), col='red', lty=2, lwd=5)

#d) stability ~ salinity
termplot(Sh_stabModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-5, 3))
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2, at = seq(-5.0, 3.0, 1))
res = residuals(Sh_stabModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat$salS, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(Sh_bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-5, 3))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-5.0, 3.0, 1))
res = residuals(Sh_bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(Sh_stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-5, 3))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-5, 3, 1))
res = residuals(Sh_stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat$tempS, res), col='red', lty=2, lwd=5)



  ##FLOUNDER##

#all variables except temp and salinity previously log2 transformed
Fl_bioModel <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat)
summary(Fl_bioModel)
Fl_stabModel <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat)
summary(Fl_stabModel)

#a) biomass ~ richness
termplot(Fl_bioModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(5.0, 5.8, 0.1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_bioModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(Fl_stabModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(5.0, 6.0, 0.1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_stabModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ salinity
termplot(Fl_bioModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(31, 36, 1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_bioModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat$salS, res), col='red', lty=2, lwd=5)

#d) stability ~ salinity
termplot(Fl_stabModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_stabModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat$salS, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(Fl_bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(Fl_stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='',
         ylim=c(-3, 2))
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-3, 2, 1))
res = residuals(Fl_stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat$tempS, res), col='red', lty=2, lwd=5)



#### SUPPLEMENTAL FIGURE - MAPPING MODEL RESIDUALS ####


#FISH
#plotting residuals of DPR model
F_DPRresiduals_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                 F_bioModel$residuals)
res(F_DPRresiduals_Raster)
plot(F_DPRresiduals_Raster)

F_DPRresiduals_Raster <- crop(x = F_DPRresiduals_Raster, y = new.extent)

tm_shape(F_DPRresiduals_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))


#plotting residuals of DSR model
F_DSRresiduals_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                 F_stabModel$residuals)
res(F_DSRresiduals_Raster)
plot(F_DSRresiduals_Raster)

F_DSRresiduals_Raster <- crop(x = F_DSRresiduals_Raster, y = new.extent)

tm_shape(F_DSRresiduals_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))



#SHRIMP
#plotting residuals of DPR model
Sh_DPRresiduals_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                 Sh_bioModel$residuals)
res(Sh_DPRresiduals_Raster)
plot(Sh_DPRresiduals_Raster)

Sh_DPRresiduals_Raster <- crop(x = Sh_DPRresiduals_Raster, y = new.extent)

tm_shape(Sh_DPRresiduals_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))

#plotting residuals of DSR model
Sh_DSRresiduals_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                 Sh_stabModel$residuals)
res(Sh_DSRresiduals_Raster)
plot(Sh_DSRresiduals_Raster)

Sh_DSRresiduals_Raster <- crop(x = Sh_DSRresiduals_Raster, y = new.extent)

tm_shape(Sh_DSRresiduals_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))



#FLOUNDER
#plotting residuals of DPR model
Fl_DPRresiduals_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                 Fl_bioModel$residuals)
res(Fl_DPRresiduals_Raster)
plot(Fl_DPRresiduals_Raster)

Fl_DPRresiduals_Raster <- crop(x = Fl_DPRresiduals_Raster, y = new.extent)

tm_shape(Fl_DPRresiduals_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))

#plotting residuals of DSR model
Fl_DSRresiduals_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                 Fl_stabModel$residuals)
res(Fl_DSRresiduals_Raster)
plot(Fl_DSRresiduals_Raster)

Fl_DSRresiduals_Raster <- crop(x = Fl_DSRresiduals_Raster, y = new.extent)

tm_shape(Fl_DSRresiduals_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(states) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 1,
            legend.position = c("right", "bottom"))




