#Biomass as a function of species richness post unequal sampling correction
####READ IN DATA ####
#results when data is not aggregated into years
resultsfullpoint2 <- read.csv( "~./fish_stability/data/resultsfullpoint2.csv",
                               header = T)
#results when data is aggregated into three year stints
yrag_resultsfull <- read.csv("~./fish_stability/data/yrag_resultsfullpoint2.csv",
                             header = T)
#results when yrag data is subset for IDs that are complete over the years
yrag_sub <- read.csv("~./fish_stability/data/yrag_sub.csv", header = T)



####LOOKING AT RAW GRAPHS####

#Just looking at raw graphs 
with(resultsfullpoint2, plot(averagebio ~ averageS))
with(resultsfullpoint2, plot(averagebio ~ year))

devtools::install_github('mobiodiv/mobr', ref = 'dev')

data(inv_comm)


####DEEP STRATA REMOVED- ONE YEAR BIN####
#ANALYSIS when using resultsfullpoint2 data frame. This is when the deep strata
  #has been removed but the years have not been aggregated


## these three are biomass sd, species richness average, and invar average for IDs over all years 

bsd = with(resultsfullpoint2, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(resultsfullpoint2, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(resultsfullpoint2, tapply(averagebio, list(ID), mean, na.rm=T))
var = with(resultsfullpoint2, tapply(averagebio, list(ID), var, na.rm = T))
cv = var / (bm ^ 2)
invar = 1/ cv

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)
plot(invar ~ sm)
plot(invar[invar < 20] ~ sm[invar < 20])

#plot of invar as a function of species rich ## 


plot(invar ~ sm)
abline(coef(lm(invar ~ sm)))
summary(lm(invar ~ sm))
lminvarsm <- glm(invar ~ sm)
plot(lminvarsm)



####DEEP STRATA REMOVED - THREE YEAR BIN####
#ANALYSIS when using yrag_resultsfull data frame. This is when the deep strata
#has been removed and the years have been aggregated into three year chunks. 



## these three are biomass sd, species richness average, and invar average for IDs over 3 yr bins

bsd = with(yrag_three, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(yrag_three, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(yrag_three, tapply(averagebio, list(ID), mean, na.rm=T))
var = with(yrag_three, tapply(averagebio, list(ID), var, na.rm = T))
cv = var / (bm ^ 2)
invar = 1/ cv

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)
plot(invar ~ sm)
plot(invar ~ sm, ylim = c(0, 30))


#plot of invar as a function of species rich ## outliers were primarily removed 
  #by fixing sampling threshold in perm code. A few came back after removing outer
    #depth range. 

plot(invar ~ sm)
abline(coef(lm(invar ~ sm)))
summary(lm(invar ~ sm))
lminvarsm <- lm(invar ~ sm)
plot(lminvarsm)

#looking at relationship with outliers removed 

plot(invar[invar < 50] ~ sm[invar < 50])
abline(coef(lm(invar[invar < 50] ~ sm[invar < 50])))
summary(lm(invar[invar < 50] ~ sm[invar < 50]))




####DEEP REMOVED, THREE YEAR BIN, good IDs REPRESENTED IN ALL BINS####

#ANALYSIS when using yrag_sub data frame. This is when the deep strata
#has been removed and the years have been aggregated into three year chunks. A 
  #list of 'good' IDs is pulled out where these IDs have representation over
  #all year chunks a-i



#subsetting raster IDs with at least 5 trawls across 3 yr time frame 
  #list of IDs with representation across years
IDlist <- c(1496, 1554, 1610, 1786, 1842, 1844, 1846, 1902, 1906, 1960, 2020,
            2080, 2137, 2138, 2196, 2255, 2313, 2314, 2372, 2373, 2431, 2432,
            2550, 2609, 2610, 2669, 2729, 2788, 2789, 2909, 3029, 3089, 3150,
            3210, 3271, 3331)
#pulling out rows with the above listed IDs
yrag_sub <- yrag_three[yrag_three$ID %in% IDlist,]

#doing summaries for biomass, species, invar values

bsd = with(yrag_sub, tapply(averagebio, list(ID), sd, na.rm=T))
sm =  with(yrag_sub, tapply(averageS, list(ID), mean, na.rm=T))
bm = with(yrag_sub, tapply(averagebio, list(ID), mean, na.rm=T))
var = with(yrag_sub, tapply(averagebio, list(ID), var, na.rm = T))
cv = var / (bm ^ 2)
invar = 1/ cv

#plots between biomass, sd biomass and species rich
plot(bsd ~ sm)
plot(bm, bsd)
plot(sm, bm)
plot(cv ~ bsd)
plot(invar ~ sm)
plot(log(invar) ~ log(sm))


#plot and lm 
plot(invar ~ sm)
abline(coef(lm(invar ~ sm)))
summary(lm(invar ~ sm))
lminvarsm <- lm(invar ~ sm)
plot(lminvarsm)

plot(invar ~ sm)
lines(x, y)
abline(coef(lm(invar ~ sm + bsd)))
summary(lm(invar ~ sm + bsd))
lminvarsmbsd <- lm(invar ~ sm + bsd)
plot(lminvarsmbsd)



####DOES VAR SPECIES RICHNESS MATTER? THREE YEAR BIN####
    
    #investigating if var in species richness in a raster through time has an impact

S_SD <- with(yrag_sub, tapply(averageS, list(ID), sd))
B_SD <- with(yrag_sub, tapply(averagebio, list(ID), sd))
plot(B_SD ~ S_SD, ylab = "SD in biomass", xlab = "SD in species richness")
summary(lm(B_SD ~ S_SD))
amod <- (lm(B_SD ~ S_SD))
plot(amod)
plot(B_SD ~ S_SD, ylab = "Biomass SD", xlab = "Species Richness SD")
abline(amod$coefficients)


#var of biomass vs average species richness
B_SD
B_AV <- with(yrag_sub, tapply(averagebio, list(ID), mean))

S_AV <- with(yrag_sub, tapply(averageS, list(ID), mean))
S_SD

###### Var Bio vs Av Species 3 yr bin ####

model12 <- lm(B_SD ~ S_AV)
summary(model12)
plot(model12)

plot(B_SD ~ S_AV, xlab = "Average Species Richness per Raster Region (S)", 
     ylab = "Biomass SD per Raster Region (kg)", cex = 1.5)
abline(model12$coefficients, lwd = 2.5)



##### SD species vs Av Species - three yr bin ####

model13 <- lm(S_SD ~ S_AV)
summary(model13)
plot(model13)

plot(S_SD ~ S_AV, xlab = "Average Species Richness per Raster Region (S)"
     , ylab = "Species Richness SD per Raster Region (S)", cex = 1.5)
abline(model13$coefficients, lwd = 2.5)



##### biomass AV vs AV species - three year bin, biomass not averaged for each number of species ####
model14 <- lm(B_AV ~ S_AV)
summary(model14)
plot(model14)

plot(B_AV ~ S_AV, xlab = "Average Species Richness", ylab = "Average Biomass (kg)",
     cex = 1.5)
abline(model14$coefficients, lwd = 2.5)







