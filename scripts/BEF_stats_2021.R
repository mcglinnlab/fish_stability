library(dplyr)
library(tidyr)
library(raster)
library(tmap)
library(tmaptools)

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
b_flounder_yrsd <- with(BEF, tapply(b_flounder_av, list(ID), sd))




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
                           Nind_yrav, b_shrimp_yrav, b_flounder_yrav, B_yrsd,
                           S_yrsd, Spie_yrsd, sN_yrsd, Nind_yrsd, b_shrimp_yrsd,
                           b_flounder_yrsd, tempS_yr, tempB_yr, salB_yr, salS_yr,
                           tempS_yrsd, tempB_yrsd, salB_yrsd, salS_yrsd))
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




####FIGURE 1 - RASTER MAPS ####

#a) number of trawls per raster - uses ID.df
ID.df$identity <- 1

trawl_num <- ID.df %>%
  group_by(rasterID) %>%
  summarize(trawlnum = sum(identity))

trawl_num <- trawl_num[trawl_num$rasterID %in% IDlist, ]
trawl_num <- cbind(trawl_num, rasterID_coord$x, rasterID_coord$y)
names(trawl_num) <- c("ID", "trawlnum", "long", "lat")

#extent of maps
new.extent <- c(-84, -75, 27, 38)

#36 good raster regions
Trawl_raster <- rasterize(trawl_num[,3:4], oc_raster, trawl_num$trawlnum)
res(Trawl_raster)
plot(Trawl_raster)

Trawl_raster <- crop(x = Trawl_raster, y = new.extent)

tm_shape(Trawl_raster) +
  tm_raster(title = "Number of Trawls") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#b) average rarefied richness to 5 trawls
Species_Raster <- rasterize(trawl_num[, 3:4], oc_raster, BEF_yr$S_yrav)
res(Species_Raster)
plot(Species_Raster)

Species_Raster <- crop( x = Species_Raster, y = new.extent)

tm_shape(Species_Raster) +
  tm_raster(title = "Number of Species (S)", palette = "Blues") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#c) average total biomass 5 trawls
Biomass_Raster <- rasterize(trawl_num[, 3:4], oc_raster, BEF_yr$b_yrav)
res(Biomass_Raster)
plot(Biomass_Raster)

Biomass_Raster <- crop(x = Biomass_Raster, y = new.extent)

tm_shape(Biomass_Raster) +
  tm_raster(title = " Fish Biomass (kg)", palette = "Greens") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#d) average stability biomass at 5 trawls
Stability_Raster <- rasterize(trawl_num[, 3:4], oc_raster, BEF_yr$B_yrstab)
res(Stability_Raster)
plot(Stability_Raster)

Stability_Raster <- crop(x = Stability_Raster, y = new.extent)

tm_shape(Stability_Raster) +
  tm_raster(title = "Stability of Fish Biomass", palette = "PuRd") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass()

#e) average surface temperature
SurfaceTemp_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                                BEF_yr$tempS_yr)
res(SurfaceTemp_Raster)
plot(SurfaceTemp_Raster)

SurfaceTemp_Raster <- crop(x = SurfaceTemp_Raster, y = new.extent)

tm_shape(SurfaceTemp_Raster) +
  tm_raster(title = "Surface Water Temperature (C)", palette = "-RdYlBu") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 0.75, 
            legend.title.size = 2)

#f) average surface salinity
SurfaceSal_Raster <- rasterize(trawl_num[, 3:4], oc_raster,
                               BEF_yr$salS_yr)
res(SurfaceSal_Raster)
plot(SurfaceSal_Raster)

SurfaceSal_Raster <- crop(x = SurfaceSal_Raster, y = new.extent)

tm_shape(SurfaceSal_Raster) +
  tm_raster(title = "", palette = "-RdYlGn") +
  tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass() +
  tm_layout(legend.text.size = 0.75,
            legend.title.size = 2)

