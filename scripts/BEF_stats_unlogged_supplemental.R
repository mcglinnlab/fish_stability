
library(QuantPsyc)

#BEF_stats supplemental script

#read data
BEF_yr <- read.csv("./data/BEF_yr.csv", header = T)
BEF_yr <- BEF_yr[, -1]


#### non log transformed beta coefficient models ####

#data prep
#pulling columns from final_output that are used and log transforming bio, stab, and S
moddat_nl <- as.data.frame(cbind(BEF_yr$b_yrav, BEF_yr$B_yrstab, 
                              BEF_yr$b_shrimp_yrav, BEF_yr$b_shrimp_yrstab,
                              BEF_yr$b_flounder_yrav, BEF_yr$b_flounder_yrstab,
                              BEF_yr$S_yrav, as.numeric(BEF_yr$tempS_yr), as.numeric(BEF_yr$salS_yr)))
moddat_nl[] <- sapply(moddat_nl, as.numeric)
moddat_nl <- as.data.frame(cbind(BEF_yr$ID, moddat_nl))
names(moddat_nl) <- c("ID", "F_bio", "F_stab", "Sh_bio", "Sh_stab", "Fl_bio", 
                   "Fl_stab", "Srich", "tempS", "salS")


#Standardized beta coefficient models
#scaling variables (subtracting mean and dividing by standard deviation)
moddat_nl_S <- as.data.frame(scale(moddat_nl[,-1], center = T, scale = T))
#adding startID column and renaming columns
moddat_nl_S[] <- sapply(moddat_nl_S, as.numeric)
moddat_nl_S <- as.data.frame(cbind(BEF_yr$ID, moddat_nl_S))
names(moddat_nl_S) <- c("ID", "F_bio", "F_stab", "Sh_bio", "Sh_stab", "Fl_bio", 
                     "Fl_stab", "Srich", "tempS", "salS")


#FISH
#biomass
#run model; log transformations built in before scaling step
bioModel_S_fish <- lm(F_bio ~ Srich + tempS + salS, data = moddat_nl_S)
summary(bioModel_S_fish)
#standardized regression coefficients
lm.beta(bioModel_S_fish)
plot(bioModel_S_fish)
#raw coefficients
bioModel_fish <- lm(F_bio ~ Srich + tempS + salS, data = moddat_nl)
summary(bioModel_fish)

#stability
#model
stabModel_S_fish <- lm(F_stab ~ Srich + tempS + salS, data = moddat_nl_S)
summary(stabModel_S_fish)
#standardized regression coefficients
lm.beta(stabModel_S_fish)
plot(stabModel_S_fish)
#raw coefficients
stabModel_fish <- lm(F_stab ~ Srich + tempS + salS, data = moddat_nl)
summary(stabModel_fish)



#SHRIMP
#also include fish bio - not included in table
modtest_shrimp <- lm(Sh_bio ~ Srich + tempS + salS + F_bio, data = moddat_nl_S)
summary(modtest_shrimp)
lm.beta(modtest_shrimp)

#biomass
#run model; log transformations built in before scaling step
bioModel_S_shrimp <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat_nl_S)
summary(bioModel_S_shrimp)
#standardized regression coefficients
lm.beta(bioModel_S_shrimp)
plot(bioModel_S_shrimp)
#raw coefficients
bioModel_shrimp <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat_nl)
summary(bioModel_shrimp)


#stability
#model
stabModel_S_shrimp <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat_nl_S)
summary(stabModel_S_shrimp)
#standardized regression coefficients
lm.beta(stabModel_S_shrimp)
plot(stabModel_S_shrimp)
#raw coefficients
stabModel_shrimp <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat_nl)
summary(stabModel_shrimp)


#FLOUNDER
#biomass
#run model; log transformations built in before scaling step
bioModel_S_flounder <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat_nl_S)
summary(bioModel_S_flounder)
#standardized regression coefficients
lm.beta(bioModel_S_flounder)
plot(bioModel_S_flounder)
#raw coefficients
bioModel_flounder <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat_nl)
summary(bioModel_flounder)

#stability
#model
stabModel_S_flounder <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat_nl_S)
summary(stabModel_S_flounder)
#standardized regression coefficients
lm.beta(stabModel_S_flounder)
plot(stabModel_S_flounder)
#raw coefficients
stabModel_flounder <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat_nl)
summary(stabModel_flounder)



#### FIGURE THREE - PARTIAL RESIDUAL PLOTS ####

##FISH##

#all variables except temp and salinity previously log2 transformed
F_bioModel <- lm(F_bio ~ Srich + tempS + salS, data = moddat_nl)
summary(F_bioModel)
F_stabModel <- lm(F_stab ~ Srich + tempS + salS, data = moddat_nl)
summary(F_stabModel)

#a) biomass ~ richness
termplot(F_bioModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(30, 60, 5))
axis(side=2, cex.axis=2, at = seq(-1000, 1000, 200))
res = residuals(F_bioModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat_nl$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(F_stabModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(30.0, 60.0, 5))
axis(side=2, cex.axis=2, at = seq(-10.0, 10.0, 1))
res = residuals(F_stabModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat_nl$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ salinity
termplot(F_bioModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(31, 36, 1))
axis(side=2, cex.axis=2, at = seq(-350, 400, 50))
res = residuals(F_bioModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat_nl$salS, res), col='red', lty=2, lwd=5)

#d) stability ~ salinity
termplot(F_stabModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2, at = seq(-10, 10, 2))
res = residuals(F_stabModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat_nl$salS, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(F_bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-350.0, 500, 50))
res = residuals(F_bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat_nl$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(F_stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-5, 15, 2))
res = residuals(F_stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat_nl$tempS, res), col='red', lty=2, lwd=5)


##SHRIMP##

#all variables except temp and salinity previously log2 transformed
Sh_bioModel <- lm(Sh_bio ~ Srich + tempS + salS, data = moddat_nl)
summary(Sh_bioModel)
Sh_stabModel <- lm(Sh_stab ~ Srich + tempS + salS, data = moddat_nl)
summary(Sh_stabModel)




#a) biomass ~ richness
termplot(Sh_bioModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(35, 55, 2))
axis(side=2, cex.axis=2, at = seq(-22, 22, 2))
res = residuals(Sh_bioModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat_nl$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(Sh_stabModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(35, 70, 2))
axis(side=2, cex.axis=2, at = seq(-10.0, 10.0, 1))
res = residuals(Sh_stabModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat_nl$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ salinity
termplot(Sh_bioModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(31, 36, 1))
axis(side=2, cex.axis=2, at = seq(-20.0, 20.0, 2))
res = residuals(Sh_bioModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat_nl$salS, res), col='red', lty=2, lwd=5)

#d) stability ~ salinity
termplot(Sh_stabModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2, at = seq(-6.0, 6.0, 2))
res = residuals(Sh_stabModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat_nl$salS, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(Sh_bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(20, 26, 0.5))
axis(side=2, cex.axis=2, at = seq(-15, 20, 2))
res = residuals(Sh_bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat_nl$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(Sh_stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-5, 10, 1))
res = residuals(Sh_stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat_nl$tempS, res), col='red', lty=2, lwd=5)



##FLOUNDER##

#all variables except temp and salinity previously log2 transformed
Fl_bioModel <- lm(Fl_bio ~ Srich + tempS + salS, data = moddat_nl)
summary(Fl_bioModel)
Fl_stabModel <- lm(Fl_stab ~ Srich + tempS + salS, data = moddat_nl)
summary(Fl_stabModel)

#a) biomass ~ richness
termplot(Fl_bioModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(35, 60, 5))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_bioModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat_nl$Srich), res), col='red', lty=2, lwd=5)

#b) stability ~ richness
termplot(Fl_stabModel, terms = "Srich", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(35, 60.0, 2))
axis(side=2, cex.axis=2, at = seq(-10.0, 10, 1))
res = residuals(Fl_stabModel, 'partial')
res = res[ , 'Srich', drop=FALSE]
lines(lowess((moddat_nl$Srich), res), col='red', lty=2, lwd=5)

#c) biomass ~ salinity
termplot(Fl_bioModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(31, 36, 1))
axis(side=2, cex.axis=2, at = seq(-3.0, 2.0, 1))
res = residuals(Fl_bioModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat_nl$salS, res), col='red', lty=2, lwd=5)

#d) stability ~ salinity
termplot(Fl_stabModel, terms = "salS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(27, 36, 1))
axis(side=2, cex.axis=2, at = seq(-5.0, 10.0, 2))
res = residuals(Fl_stabModel, 'partial')
res = res[ , 'salS', drop=FALSE]
lines(lowess(moddat_nl$salS, res), col='red', lty=2, lwd=5)

#e) biomass ~ temperature
termplot(Fl_bioModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(21, 25, 0.5))
axis(side=2, cex.axis=2, at = seq(-3.0, 5.0, 1))
res = residuals(Fl_bioModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat_nl$tempS, res), col='red', lty=2, lwd=5)

#f) stability ~ temperature 
termplot(Fl_stabModel, terms = "tempS", partial=T, se=T,
         lwd.term=5,lwd.se=3.5, pch = 16, cex = 2,
         col.term='blue', col.se='lightblue',
         col.res = 'black', col.smth = "red",
         frame.plot=F, axes=F, xlab='', ylab='')
axis(side=1, cex.axis=2, at = seq(21.5, 25.5, 0.5))
axis(side=2, cex.axis=2, at = seq(-3, 5, 1))
res = residuals(Fl_stabModel, 'partial')
res = res[ , 'tempS', drop=FALSE]
lines(lowess(moddat_nl$tempS, res), col='red', lty=2, lwd=5)

