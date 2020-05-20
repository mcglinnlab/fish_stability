library(ggplot2)
library(dplyr)
library(tidyr)
library(lmodel2)

#Organizing BEF analysis into single script with graphs and analysis

  #### PRE RASTERIZATION - AT TRAWL EVENT LEVEL ####
#data sets
  #DATA SET ONE
  #raw data before aggregation by eventname
  SEAMAP_inner
  SEAMAP_inner <- SEAMAP_inner[,-1]
  
  
  #DATA SET TWO
#7,412 events. data aggregated by event 
  s_environ
  
#adding month column
  dates <- s_environ$date
  dates <- as.Date(dates, "%m/%d/%Y")
  month <- strftime(dates, "%m")
  s_environ <- cbind(month, s_environ)

#plots
  
  plot(s_environ$biomass ~ s_environ$S)
  
  plot(s_environ$biomass ~ s_environ$year)
  
  plot(s_environ$biomass ~ s_environ$yrcat)
  
  
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
  model9 <- with(s_environ, lm(log(biomass) ~ S))
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
  yrag_sub
  
  B_AV <- with(yrag_sub, tapply(averagebio, list(ID), mean))
  S_AV <- with(yrag_sub, tapply(averageS, list(ID), mean))
  S_SD <- with(yrag_sub, tapply(averageS, list(ID), sd))
  B_VAR <- with(yrag_sub, tapply(averagebio, list(ID), function(x)(var(x)/ (mean(x)^2))))
  B_SD <- with(yrag_sub, tapply(averagebio, list(ID), sd))
  B_invar <- 1/B_VAR
  
#B_VAR ~ S_AV
  model12 <- lmodel2(B_VAR ~ S_AV, nperm = 100)
  model12
  plot(model12)
  
  plot(B_VAR ~ S_AV, xlab = "Average Species Richness per Raster Region (S)", 
       ylab = "Biomass Var per Raster Region (kg)", cex = 1.5)
  abline(model12$coefficients, lwd = 2.5)
  
#B_invar ~ S_AV
  model15 <- lmodel2(B_invar ~ S_AV, nperm = 100)
  model15
  plot(model15)
  
  plot(B_invar ~ S_AV, xlab = "Average Species Richness(S)", 
       ylab = "Biomass invar (kg)", cex = 1.5)
  abline(model15$coefficients, lwd = 2.5)
  
#S_SD ~ S_AV
  model13 <- lmodel2(S_SD ~ S_AV, nperm = 100)
  model13
  plot(model13)
  
  plot(S_SD ~ S_AV, xlab = "Average Species Richness per Raster Region (S)"
       , ylab = "Species Richness SD per Raster Region (S)", cex = 1.5)
  abline(model13$coefficients, lwd = 2.5)
  
#B_AV ~ S_AV
  model14 <- lmodel2(B_AV ~ S_AV, nperm = 100)
  model14
  plot(model14)
  
  plot(B_AV ~ S_AV, xlab = "Average Species Richness", ylab = "Average Biomass (kg)",
       cex = 1.5)
  abline(model14$coefficients, lwd = 2.5)
  
#B_VAR ~ S_SD with fill S_AV
  data <- as.data.frame(cbind(S_SD, S_AV, B_VAR))
  dat <- as.data.frame(cbind(S_SD, B_VAR))
  
  ggplot(data = data, aes(x = S_SD, y = B_VAR, fill = S_AV)) +
    geom_point(size = 8, shape = 21) +
    geom_smooth(data = dat, method = "lm" ) +
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
  
  
  
  
  
  