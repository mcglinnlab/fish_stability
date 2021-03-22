library(dplyr)
library(tidyr)

#### BEF SCALE ONE ANALYSIS ####

#DATA SET
  #SUMMARY_BEF - 324,000  rows, 1,000 iterations, 36 rasters, 9 time bins
summary_BEF <- read.csv("~/fish_stability/data/BEF/summary_BEF.csv", header = T)
summary_BEF <- summary_BEF[, -1]


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



mod1 <- with(BEF_yr, lm(log(b_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(S_yrav)))
abline(mod1$coefficients)
