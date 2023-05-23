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
summary_BEF <- read.csv("./gitdat/summary_BEF3.csv")

plot(S ~ Nind, data = BEF)
lines(lowess(summary_BEF$Nind, summary_BEF$S), col='red', lwd =2)
plot(biomass ~ N, data = summary_BEF)
plot(biomass ~ Nind, data = summary_BEF)
lines(lowess(summary_BEF$Nind, summary_BEF$biomass), col='red', lwd =2)


#ID.df - Information about each raster IDs
ID.df <- read.csv("./gitdat/ID.df.csv")
ID.df <- ID.df[, -1]

#ID list - list of good raster IDs
IDlist <- c(1246, 1294, 1340, 1486, 1532, 1534, 1536, 1582, 1586, 1630, 1680, 
            1730, 1777, 1778, 1826, 1875, 1923, 1924, 1972, 1973, 2021, 2022, 
            2120, 2169, 2170, 2219, 2269, 2318, 2319, 2419, 2519, 2569, 2620, 
            2670, 2721, 2771)



#### Averaging across bootstrap iterations ####

#biologic  
#average over bootstraps
b_av <- with(summary_BEF, tapply(biomass, list(unique_ID), mean))
S_av <- with(summary_BEF, tapply(S, list(unique_ID), mean))
Spie_av <- with(summary_BEF, tapply(sPIE, list(unique_ID), mean))
sN_av <- with(summary_BEF, tapply(s_N, list(unique_ID), mean))
sHill_av <- with(summary_BEF, tapply(s_Hill, list(unique_ID), mean))
sasym_av <- with(summary_BEF, tapply(s_asym, list(unique_ID), mean))
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




BEF <- as.data.frame(cbind(b_av, S_av, Spie_av, sN_av, sasym_av, sHill_av, Nind_av, b_shrimp_av,
                           b_flounder_av, B_bootsd, S_bootsd, Spie_bootsd, 
                           sN_bootsd, Nind_bootsd, b_shrimp_bootsd, b_flounder_bootsd, 
                           tempS, tempB, salB, salS, tempSsd, tempBsd, salBsd, salSsd))
BEF$unique_ID <- rownames(BEF)

BEF <- BEF %>%
  separate(unique_ID, c("ID", "yr_cat"))

par(mfrow=c(2,3))
plot(b_av ~ S_av, data = BEF)
with(BEF, lines(lowess(S_av, b_av), col='red', lwd =2))
plot(b_av ~ Spie_av, data = BEF)
with(BEF, lines(lowess(Spie_av, b_av), col='red', lwd =2))
plot(b_av ~ sN_av, data = BEF, log='xy')
with(BEF, lines(lowess(sN_av, b_av), col='red', lwd =2))

plot(b_av ~ sHill_av, data = BEF, log='xy')
with(BEF, lines(lowess(sHill_av, b_av), col='red', lwd =2))

plot(b_av ~ sasym_av, data = BEF, log='xy')
with(BEF, lines(lowess(sasym_av, b_av), col='red', lwd =2))

with(BEF, cor(log(sasym_av), log(b_av)))
with(BEF, cor(log(S_av), log(b_av), use = 'complete.obs'))

summary(lm(log(b_av) ~ log(sasym_av) + tempS + salS, data = BEF))



plot(S_av ~ Nind_av, data = BEF)
with(BEF, lines(lowess(Nind_av, S_av), col='red', lwd =2))
plot(b_av ~ Nind_av, data = BEF)
with(BEF, lines(lowess(Nind_av, b_av), col='red', lwd =2))


plot(biomass ~ Nind, data = summary_BEF)
lines(lowess(summary_BEF$Nind, summary_BEF$biomass), col='red', lwd =2)




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







#### compare S with Spie and s_N####

  #each group first ~S then ~Spie and ~s_N

with(BEF_yr, plot(b_yrav ~ S_yrav))
with(BEF_yr, plot(b_yrav ~ Spie_yrav))
with(BEF_yr, plot(b_yrav ~ sN_yrav))

with(BEF_yr, plot(log(b_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(sN_yrav)))

with(BEF_yr, plot(log(B_yrstab) ~ log(S_yrav)))
with(BEF_yr, plot(log(B_yrstab) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(B_yrstab) ~ log(sN_yrav)))

with(BEF_yr, plot(B_yrstab ~ log(S_yrav)))
with(BEF_yr, plot(B_yrstab ~ log(Spie_yrav)))
with(BEF_yr, plot(B_yrstab ~ log(sN_yrav)))

with(BEF_yr, plot(log(B_yrstab) ~ log(S_yrav)))
with(BEF_yr, plot(log(B_yrstab) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(B_yrstab) ~ log(sN_yrav)))

with(BEF_yr, plot(log(b_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_yrav) ~ log(sN_yrav)))

with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(sN_yrav)))

with(BEF_yr, plot(log(b_flounder_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(sN_yrav)))

with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(sN_yrav)))

with(BEF_yr, plot(log(b_flounder_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_flounder_yrav) ~ log(sN_yrav)))

with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(S_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(Spie_yrav)))
with(BEF_yr, plot(log(b_shrimp_yrav) ~ log(sN_yrav)))



#### MODEL COMPARE ####

#data prep
#pulling columns from final_output that are used and log transforming bio, stab, and S
moddat <- as.data.frame(cbind(log2(BEF_yr$b_yrav), log2(BEF_yr$B_yrstab), 
                              log2(BEF_yr$b_shrimp_yrav), log2(BEF_yr$b_shrimp_yrstab),
                              log2(BEF_yr$b_flounder_yrav), log2(BEF_yr$b_flounder_yrstab),
                              log2(BEF_yr$S_yrav), log2(BEF_yr$Spie_yrav), 
                              log2(BEF_yr$sN_yrav), as.numeric(BEF_yr$tempS_yr), 
                              as.numeric(BEF_yr$salS_yr)))
moddat[] <- sapply(moddat, as.numeric)
moddat <- as.data.frame(cbind(BEF_yr$ID, moddat))
names(moddat) <- c("ID", "F_bio", "F_stab", "Sh_bio", "Sh_stab", "Fl_bio", 
                   "Fl_stab", "Srich", "Spie", "s_N", "tempS", "salS")


#Standardized beta coefficient models
#scaling variables (subtracting mean and dividing by standard deviation)
moddat_S <- as.data.frame(scale(moddat[,-1], center = T, scale = T))
#adding startID column and renaming columns
moddat_S[] <- sapply(moddat_S, as.numeric)
moddat_S <- as.data.frame(cbind(BEF_yr$ID, moddat_S))
names(moddat_S) <- c("ID", "F_bio", "F_stab", "Sh_bio", "Sh_stab", "Fl_bio", 
                     "Fl_stab", "Srich", "Spie", "s_N", "tempS", "salS")


#FISH
#biomass
#S
  #run model; log transformations built in before scaling step
bioModel_S_fish <- lm(F_bio ~ Srich + tempS + salS, data = moddat_S)
summary(bioModel_S_fish)
  #standardized regression coefficients
lm.beta(bioModel_S_fish)
plot(bioModel_S_fish)
  #raw coefficients
bioModel_fish <- lm(F_bio ~ Srich + tempS + salS, data = moddat)
summary(bioModel_fish)


#Spie
  #run model; log transformations built in before scaling step
bioModel_S_fish <- lm(F_bio ~ Spie + tempS + salS, data = moddat_S)
summary(bioModel_S_fish)
  #standardized regression coefficients
lm.beta(bioModel_S_fish)
plot(bioModel_S_fish)
  #raw coefficients
bioModel_fish <- lm(F_bio ~ Spie + tempS + salS, data = moddat)
summary(bioModel_fish)


  #s_N
#run model; log transformations built in before scaling step
bioModel_S_fish <- lm(F_bio ~ s_N + tempS + salS, data = moddat_S)
summary(bioModel_S_fish)
#standardized regression coefficients
lm.beta(bioModel_S_fish)
plot(bioModel_S_fish)
#raw coefficients
bioModel_fish <- lm(F_bio ~ s_N + tempS + salS, data = moddat)
summary(bioModel_fish)

par(mfrow=c(1,3))
termplot(bioModel_S_fish, partial.resid = T, se = T)



#stability
#S
  #model
stabModel_S_fish <- lm(F_stab ~ Srich + tempS + salS, data = moddat_S)
summary(stabModel_S_fish)
  #standardized regression coefficients
lm.beta(stabModel_S_fish)
plot(stabModel_S_fish)
  #raw coefficients
stabModel_fish <- lm(F_stab ~ Srich + tempS + salS, data = moddat)
summary(stabModel_fish)


#Spie
  #model
stabModel_S_fish <- lm(F_stab ~ Spie + tempS + salS, data = moddat_S)
summary(stabModel_S_fish)
  #standardized regression coefficients
lm.beta(stabModel_S_fish)
plot(stabModel_S_fish)
  #raw coefficients
stabModel_fish <- lm(F_stab ~ Spie + tempS + salS, data = moddat)
summary(stabModel_fish)


#s_N
#model
stabModel_S_fish <- lm(F_stab ~ s_N + tempS + salS, data = moddat_S)
summary(stabModel_S_fish)
#standardized regression coefficients
lm.beta(stabModel_S_fish)
plot(stabModel_S_fish)
#raw coefficients
stabModel_fish <- lm(F_stab ~ s_N + tempS + salS, data = moddat)
summary(stabModel_fish)





#SHRIMP

#biomass
  #S
#run model; log transformations built in before scaling step
bioModel_S_shrimp <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat_S)
summary(bioModel_S_shrimp)
#standardized regression coefficients
lm.beta(bioModel_S_shrimp)
plot(bioModel_S_shrimp)
#raw coefficients
bioModel_shrimp <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat)
summary(bioModel_shrimp)

  #Spie
#run model; log transformations built in before scaling step
bioModel_S_shrimp <- lm(Sh_bio ~ Spie + tempS + salS, data = moddat_S)
summary(bioModel_S_shrimp)
#standardized regression coefficients
lm.beta(bioModel_S_shrimp)
plot(bioModel_S_shrimp)
#raw coefficients
bioModel_shrimp <- lm(Sh_bio ~ Spie + tempS + salS, data = moddat)
summary(bioModel_shrimp)


  #s_N
#run model; log transformations built in before scaling step
bioModel_S_shrimp <- lm(Sh_bio ~ s_N + tempS + salS, data = moddat_S)
summary(bioModel_S_shrimp)
#standardized regression coefficients
lm.beta(bioModel_S_shrimp)
plot(bioModel_S_shrimp)
#raw coefficients
bioModel_shrimp <- lm(Sh_bio ~ s_N + tempS + salS, data = moddat)
summary(bioModel_shrimp)


#stability
  #S
#model
stabModel_S_shrimp <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat_S)
summary(stabModel_S_shrimp)
#standardized regression coefficients
lm.beta(stabModel_S_shrimp)
plot(stabModel_S_shrimp)
#raw coefficients
stabModel_shrimp <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat)
summary(stabModel_shrimp)

  #Spie
#model
stabModel_S_shrimp <- lm(Sh_stab ~ Spie + tempS + salS, data = moddat_S)
summary(stabModel_S_shrimp)
#standardized regression coefficients
lm.beta(stabModel_S_shrimp)
plot(stabModel_S_shrimp)
#raw coefficients
stabModel_shrimp <- lm(Sh_stab ~ Spie + tempS + salS, data = moddat)
summary(stabModel_shrimp)

  #s_N
#model
stabModel_S_shrimp <- lm(Sh_stab ~ s_N + tempS + salS, data = moddat_S)
summary(stabModel_S_shrimp)
#standardized regression coefficients
lm.beta(stabModel_S_shrimp)
plot(stabModel_S_shrimp)
#raw coefficients
stabModel_shrimp <- lm(Sh_stab ~ s_N + tempS + salS, data = moddat)
summary(stabModel_shrimp)






#FLOUNDER
#biomass
  #S
#run model; log transformations built in before scaling step
bioModel_S_flounder <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat_S)
summary(bioModel_S_flounder)
#standardized regression coefficients
lm.beta(bioModel_S_flounder)
plot(bioModel_S_flounder)
#raw coefficients
bioModel_flounder <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat)
summary(bioModel_flounder)

  #Spie
#run model
bioModel_S_flounder <- lm(Fl_bio ~ Spie + tempS + salS, data = moddat_S)
summary(bioModel_S_flounder)
#standardized regression coefficients
lm.beta(bioModel_S_flounder)
plot(bioModel_S_flounder)
#raw coefficients
bioModel_flounder <- lm(Fl_bio ~ Spie + tempS + salS, data = moddat)
summary(bioModel_flounder)

  #s_N
#run model
bioModel_S_flounder <- lm(Fl_bio ~ s_N + tempS + salS, data = moddat_S)
summary(bioModel_S_flounder)
#standardized regression coefficients
lm.beta(bioModel_S_flounder)
plot(bioModel_S_flounder)
#raw coefficients
bioModel_flounder <- lm(Fl_bio ~ s_N + tempS + salS, data = moddat)
summary(bioModel_flounder)

#stability
  #S
#model
stabModel_S_flounder <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat_S)
summary(stabModel_S_flounder)
#standardized regression coefficients
lm.beta(stabModel_S_flounder)
plot(stabModel_S_flounder)
#raw coefficients
stabModel_flounder <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat)
summary(stabModel_flounder)

  #Spie
#model
stabModel_S_flounder <- lm(Fl_stab ~ Spie + tempS + salS, data = moddat_S)
summary(stabModel_S_flounder)
#standardized regression coefficients
lm.beta(stabModel_S_flounder)
plot(stabModel_S_flounder)
#raw coefficients
stabModel_flounder <- lm(Fl_stab ~ Spie + tempS + salS, data = moddat)
summary(stabModel_flounder)

  #s_N
#model
stabModel_S_flounder <- lm(Fl_stab ~ s_N + tempS + salS, data = moddat_S)
summary(stabModel_S_flounder)
#standardized regression coefficients
lm.beta(stabModel_S_flounder)
plot(stabModel_S_flounder)
#raw coefficients
stabModel_flounder <- lm(Fl_stab ~ s_N + tempS + salS, data = moddat)
summary(stabModel_flounder)




