#permutation of events within each raster region to account for unequal sampling effort

#series of three for loops accounting for year, grid ID, and then permutation of selecting events

for (i in 1989:2015)
{
  yearpull <- ID.df[ID.df$year == "i",columns of S biomass ID]
  for (i in 1: #number of trawl IDs)
  {
    eventsavailforchoosing <- ID.df$eventnumber[ID.df$ID == "i"]
    for (i in 1:1000)
    {
      event <- sample( eventsavail, eventthreshold, replace = F)
      averageS <- mean(ID.df$S[ID.df$eventname == "c(event)"])
      averagebio <- mean(ID.df$biomass[ID.df$eventname == "c(event)"])
      varbiomass <- var(ID.df$biomass[ID.df$eventname == "c(event)"])
    }
    averageS <- mean (averageS)
    averagebio <- mean(averagebio)
    averagevarbio <- mean(varbiomass)
    NEW.df <- as.data.frame(cbind(ID, averageS, averagebio, averagevarbio))
    
  }
}
