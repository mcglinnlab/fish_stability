#Trying some things 

library(tmap)
library(lattice)
library(cartography)
library(maps)
library(ggplot2)
library(sp)
library(maptools)
library(tmaptools)

usa <-map_data("usa")

s_rarefac <- read.csv("~./fish_stability/data/s_rarefac.csv", header = T)

data(tmap.pal.info)
palette_explorer()

#average species richness map 0.2 for poster

tm_shape(SpeciesRich_raster) +
  tm_raster(title = "Average Species Richness") +
tm_shape(continents) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position = c("left", "bottom"))

#trawl density richness map 0.2 for poster
#tm_mode()
tm_shape(Trawl_raster) +
  tm_raster(title = "Trawl Density", palette = 'YlGnBu') +
  tm_shape(continents, bbox = extent) +
  tm_borders(lwd = 0.5) +
  tm_scale_bar(position=c("left", "bottom"))


#species richness versus biomass graph for poster

xyplot(s_rarefac$biomass ~ s_rarefac$S|s_rarefac$year , relation = 'free')
xyplot(s_rarefac$biomass ~ s_rarefac$S, groups = s_rarefac$year)
     

#sig  
lm1989 <- lm(s_rarefac$biomass[s_rarefac$year == '1989'] ~ s_rarefac$S[s_rarefac$year == '1989'])
plot(s_rarefac$biomass[s_rarefac$year == '1989'] ~ s_rarefac$S[s_rarefac$year == '1989'], xlab = "Species Richness", ylab = "Total Biomass (kg)")
abline(lm1989$coefficients)
summary(lm1989)

#sig
lm1990 <- lm(s_rarefac$biomass[s_rarefac$year == '1990'] ~ s_rarefac$S[s_rarefac$year == '1990'])
plot(s_rarefac$biomass[s_rarefac$year == '1990'] ~ s_rarefac$S[s_rarefac$year == '1990'])
abline(lm1990$coefficients)
summary(lm1990)

#sig
lm1991 <- lm(s_rarefac$biomass[s_rarefac$year == '1991'] ~ s_rarefac$S[s_rarefac$year == '1991'])
plot(s_rarefac$biomass[s_rarefac$year == '1991'] ~ s_rarefac$S[s_rarefac$year == '1991'])
abline(lm1991$coefficients)
summary(lm1991)

#sig
lm1992 <- lm(s_rarefac$biomass[s_rarefac$year == '1992'] ~ s_rarefac$S[s_rarefac$year == '1992'])
plot(s_rarefac$biomass[s_rarefac$year == '1992'] ~ s_rarefac$S[s_rarefac$year == '1992'])
abline(lm1992$coefficients)
summary(lm1992)

#notsig
lm1993 <- lm(s_rarefac$biomass[s_rarefac$year == '1993'] ~ s_rarefac$S[s_rarefac$year == '1993'])
plot(s_rarefac$biomass[s_rarefac$year == '1993'] ~ s_rarefac$S[s_rarefac$year == '1993'])
abline(lm1993$coefficients)
summary(lm1993)

#sig
lm1994 <- lm(s_rarefac$biomass[s_rarefac$year == '1994'] ~ s_rarefac$S[s_rarefac$year == '1994'])
plot(s_rarefac$biomass[s_rarefac$year == '1994'] ~ s_rarefac$S[s_rarefac$year == '1994'])
abline(lm1994$coefficients)
summary(lm1994)

#sig
lm1995 <- lm(s_rarefac$biomass[s_rarefac$year == '1995'] ~ s_rarefac$S[s_rarefac$year == '1995'])
plot(s_rarefac$biomass[s_rarefac$year == '1995'] ~ s_rarefac$S[s_rarefac$year == '1995'])
abline(lm1995$coefficients)
summary(lm1995)

#sig
lm1996 <- lm(s_rarefac$biomass[s_rarefac$year == '1996'] ~ s_rarefac$S[s_rarefac$year == '1996'])
plot(s_rarefac$biomass[s_rarefac$year == '1996'] ~ s_rarefac$S[s_rarefac$year == '1996'])
abline(lm1996$coefficients)
summary(lm1996)

#sig
lm1997 <- lm(s_rarefac$biomass[s_rarefac$year == '1997'] ~ s_rarefac$S[s_rarefac$year == '1997'])
plot(s_rarefac$biomass[s_rarefac$year == '1997'] ~ s_rarefac$S[s_rarefac$year == '1997'])
abline(lm1997$coefficients)
summary(lm1997)

#sig
lm1998 <- lm(s_rarefac$biomass[s_rarefac$year == '1998'] ~ s_rarefac$S[s_rarefac$year == '1998'])
plot(s_rarefac$biomass[s_rarefac$year == '1998'] ~ s_rarefac$S[s_rarefac$year == '1998'])
abline(lm1998$coefficients)
summary(lm1998)

#sig
lm1999 <- lm(s_rarefac$biomass[s_rarefac$year == '1999'] ~ s_rarefac$S[s_rarefac$year == '1999'])
plot(s_rarefac$biomass[s_rarefac$year == '1999'] ~ s_rarefac$S[s_rarefac$year == '1999'])
abline(lm1999$coefficients)
summary(lm1999)

#sig
lm2000 <- lm(s_rarefac$biomass[s_rarefac$year == '2000'] ~ s_rarefac$S[s_rarefac$year == '2000'])
plot(s_rarefac$biomass[s_rarefac$year == '2000'] ~ s_rarefac$S[s_rarefac$year == '2000'])
abline(lm2000$coefficients)
summary(lm2000)

#sig
lm2001 <- lm(s_rarefac$biomass[s_rarefac$year == '2001'] ~ s_rarefac$S[s_rarefac$year == '2001'])
plot(s_rarefac$biomass[s_rarefac$year == '2001'] ~ s_rarefac$S[s_rarefac$year == '2001'])
abline(lm2001$coefficients)
summary(lm2001)

#sig
lm2002 <- lm(s_rarefac$biomass[s_rarefac$year == '2002'] ~ s_rarefac$S[s_rarefac$year == '2002'])
plot(s_rarefac$biomass[s_rarefac$year == '2002'] ~ s_rarefac$S[s_rarefac$year == '2002'])
abline(lm2002$coefficients)
summary(lm2002)

#sig
lm2003 <- lm(s_rarefac$biomass[s_rarefac$year == '2003'] ~ s_rarefac$S[s_rarefac$year == '2003'])
plot(s_rarefac$biomass[s_rarefac$year == '2003'] ~ s_rarefac$S[s_rarefac$year == '2003'], xlab = 'Species Richness', ylab = 'Total Biomass (kg)')
abline(lm2003$coefficients)
summary(lm2003)


#notsig
lm2004t <- lm(s_rarefac$biomass[s_rarefac$year == '2004'] ~ s_rarefac$S[s_rarefac$year == '2004'])
summary(lm2004t)
lm2004 <- lm(s_rarefac$biomass[s_rarefac$year == '2004'] ~ poly(s_rarefac$S[s_rarefac$year == '2004'],2))
plot(s_rarefac$biomass[s_rarefac$year == '2004'] ~ s_rarefac$S[s_rarefac$year == '2004'])
abline(lm2004t$coefficients)
lines(s_rarefac$S[s_rarefac$year== '2004'], lm2004$fitted.values)
summary(lm2004)

#sig
lm2005 <- lm(s_rarefac$biomass[s_rarefac$year == '2005'] ~ s_rarefac$S[s_rarefac$year == '2005'])
plot(s_rarefac$biomass[s_rarefac$year == '2005'] ~ s_rarefac$S[s_rarefac$year == '2005'])
abline(lm2005$coefficients)
summary(lm2005)

#notsig
lm2006 <- lm(s_rarefac$biomass[s_rarefac$year == '2006'] ~ s_rarefac$S[s_rarefac$year == '2006'])
plot(s_rarefac$biomass[s_rarefac$year == '2006'] ~ s_rarefac$S[s_rarefac$year == '2006'])
abline(lm2006$coefficients)
summary(lm2006)

#notsig
lm2007 <- lm(s_rarefac$biomass[s_rarefac$year == '2007'] ~ s_rarefac$S[s_rarefac$year == '2007'])
plot(s_rarefac$biomass[s_rarefac$year == '2007'] ~ s_rarefac$S[s_rarefac$year == '2007'])
abline(lm2007$coefficients)
summary(lm2007)

#notsig
lm2008 <- lm(s_rarefac$biomass[s_rarefac$year == '2008'] ~ s_rarefac$S[s_rarefac$year == '2008'])
plot(s_rarefac$biomass[s_rarefac$year == '2008'] ~ s_rarefac$S[s_rarefac$year == '2008'])
abline(lm2008$coefficients)
summary(lm2008)

#notsig
lm2009 <- lm(s_rarefac$biomass[s_rarefac$year == '2009'] ~ s_rarefac$S[s_rarefac$year == '2009'])
plot(s_rarefac$biomass[s_rarefac$year == '2009'] ~ s_rarefac$S[s_rarefac$year == '2009'])
abline(lm2009$coefficients)
summary(lm2009)

#sig
lm2010 <- lm(s_rarefac$biomass[s_rarefac$year == '2010'] ~ s_rarefac$S[s_rarefac$year == '2010'])
plot(s_rarefac$biomass[s_rarefac$year == '2010'] ~ s_rarefac$S[s_rarefac$year == '2010'])
abline(lm2010$coefficients)
summary(lm2010)

#notsig
lm2011 <- lm(s_rarefac$biomass[s_rarefac$year == '2011'] ~ s_rarefac$S[s_rarefac$year == '2011'])
plot(s_rarefac$biomass[s_rarefac$year == '2011'] ~ s_rarefac$S[s_rarefac$year == '2011'], xlab = 'Species Richness', ylab = 'Total Biomass (kg)')
abline(lm2011$coefficients)
summary(lm2011)

#notsig
lm2012 <- lm(s_rarefac$biomass[s_rarefac$year == '2012'] ~ s_rarefac$S[s_rarefac$year == '2012'])
plot(s_rarefac$biomass[s_rarefac$year == '2012'] ~ s_rarefac$S[s_rarefac$year == '2012'])
abline(lm2012$coefficients)
summary(lm2012)

#sig
lm2013 <- lm(s_rarefac$biomass[s_rarefac$year == '2013'] ~ s_rarefac$S[s_rarefac$year == '2013'])
plot(s_rarefac$biomass[s_rarefac$year == '2013'] ~ s_rarefac$S[s_rarefac$year == '2013'])
abline(lm2013$coefficients)
summary(lm2013)

#sig
lm2014 <- lm(s_rarefac$biomass[s_rarefac$year == '2014'] ~ s_rarefac$S[s_rarefac$year == '2014'])
plot(s_rarefac$biomass[s_rarefac$year == '2014'] ~ s_rarefac$S[s_rarefac$year == '2014'])
abline(lm2014$coefficients)
summary(lm2014)

#notsig
lm2015 <- lm(s_rarefac$biomass[s_rarefac$year == '2015'] ~ s_rarefac$S[s_rarefac$year == '2015'])
plot(s_rarefac$biomass[s_rarefac$year == '2015'] ~ s_rarefac$S[s_rarefac$year == '2015'])
abline(lm2015$coefficients)
summary(lm2015)

plot(0:50, seq(from = 0, to = 1000, by = 20), type = "n", xlab = "Species Richness", ylab = "Total Biomass (kg)")
abline(lm2015$coefficients)
abline(lm2014$coefficients, col = 'green', lwd = 3)
abline(lm2013$coefficients, col = "green", lwd = 3)
abline(lm2012$coefficients)
abline(lm2011$coefficients)
abline(lm2010$coefficients, col = "green", lwd = 3)
abline(lm2009$coefficients)
abline(lm2008$coefficients)
abline(lm2007$coefficients)
abline(lm2006$coefficients)
abline(lm2005$coefficients, col = "red", lwd = 3)
abline(lm2004t$coefficients)
abline(lm2003$coefficients, col = "red", lwd = 3)
abline(lm2002$coefficients, col = "green", lwd = 3)
abline(lm2001$coefficients, col = "green", lwd = 3)
abline(lm2000$coefficients, col = "green", lwd = 3)
abline(lm1999$coefficients, col = 'green', lwd = 3)
abline(lm1998$coefficients, col = 'green', lwd = 3)
abline(lm1997$coefficients, col = 'green')
abline(lm1996$coefficients, col ="green", lwd = 3)
abline(lm1995$coefficients, col = 'green', lwd = 3)
abline(lm1994$coefficients, col = 'green', lwd = 3)
abline(lm1993$coefficients)
abline(lm1992$coefficients, col = 'green', lwd = 3)
abline(lm1991$coefficients, col = "green", lwd = 3)
abline(lm1990$coefficients, col = 'green', lwd =3)
abline(lm1989$coefficients, col = "green", lwd = 3)



plot(0:50, seq(from = 0, to = 1000, by = 20), type = "n", xlab = "Species Richness", ylab = "Total Biomass (kg)")
abline(lm2015$coefficients)
abline(lm2014$coefficients, col = 'green')
abline(lm2013$coefficients, col = "green")
abline(lm2012$coefficients)
abline(lm2011$coefficients)
abline(lm2010$coefficients, col = "green")
abline(lm2009$coefficients)
abline(lm2008$coefficients)
abline(lm2007$coefficients)
abline(lm2006$coefficients)
abline(lm2005$coefficients, col = "red")
abline(lm2004t$coefficients)
abline(lm2003$coefficients, col = "red")
abline(lm2002$coefficients, col = "green")
abline(lm2001$coefficients, col = "green")
abline(lm2000$coefficients, col = "green")
abline(lm1999$coefficients, col = 'green')
abline(lm1998$coefficients, col = 'green')
abline(lm1997$coefficients, col = 'green')
abline(lm1996$coefficients, col ="green")
abline(lm1995$coefficients, col = 'green')
abline(lm1994$coefficients, col = 'green')
abline(lm1993$coefficients)
abline(lm1992$coefficients, col = 'green')
abline(lm1991$coefficients, col = "green")
abline(lm1990$coefficients, col = 'green')
abline(lm1989$coefficients, col = "green")



averagebiomass <- as.data.frame(aggregate(s_rarefac$biomass, list(s_rarefac$year), mean))
biomasssd <- as.data.frame(aggregate(s_rarefac$biomass, list(s_rarefac$year), sd))
biomasssd$half <- biomasssd$x/2
plot(averagebiomass$x~averagebiomass$Group.1)
barplot(averagebiomass$x/2, averagebiomass$Group.1, xlab = "Year:1989 - 2015", ylab= "Average Biomass Per Trawl (kg)")
plot(s_rarefac$biomass~s_rarefac$year)

#plot for poster
plot(averagebiomass$Group.1, averagebiomass$x, xlab = "Year", ylab= "Average Biomass Per Trawl Event (kg)", type = "o", col = "red")
par(new = TRUE)
plot(averagespeciesrichness$Group.1, averagespeciesrichness$x, type = 'o', col='blue', axes=F, frame.plot=F, xlab='', ylab='')
axis(side=4, ylab = "Average Species Richness")
mtext("Average Species Richness Per Trawl Event", side=4, line = 3)
legend("top",legend=c("Biomass", "Species Richness"), col=c("red", "blue"), lty=1, cex=0.8)

totalbiomass <- as.data.frame(aggregate(s_rarefac$biomass, list(s_rarefac$year), sum))
barplot(totalbiomass$x, totalbiomass$Group.1)

averagespeciesrichness <- as.data.frame(aggregate(s_rarefac$S, list(s_rarefac$year), mean))
