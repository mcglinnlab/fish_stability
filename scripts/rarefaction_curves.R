
#Rarefaction Curves

#Example

library(vegan)
data('BCI')
S <- specaccum(BCI) # observed number of species
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)




data(BCI)
sp1 <- specaccum(BCI)
sp2 <- specaccum(BCI, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")
## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)




#load s_rarefac
s_rarefac <- read.csv('~./fish_stability/data/s_rarefac.csv', header = T)
s_rarefac <- s_rarefac[,8:209]


SP1 <- specaccum(s_rarefac)
SP2 <- specaccum(s_rarefac, "random")
plot(SP1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
#boxplot(SP2, col="yellow", add=TRUE, pch="+")

mod1 <- fitspecaccum(SP1, "lomolino")
coef(mod1)
fitted <- as.data.frame(fitted(mod1))
plot(SP1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
## Fit Arrhenius models to all random accumulations
#mods <- fitspecaccum(SP2, "arrh")
#plot(mods, col="hotpink")
#boxplot(SP2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
AICs <- as.data.frame(sapply(mods$models, AIC))





