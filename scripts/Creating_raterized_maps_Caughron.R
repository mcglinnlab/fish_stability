#Creating Rasterized Maps

library(rgdal)
library(sp)
library(raster)
library(doParallel) 
library(foreach)
library(maps)
library(maptools)

#Creating Map




#Enviroplot
# enviro_plot function to aggregate and plot environmental variables
# y = environmental raster from initial_cleanup
# w = file path for pdf
# output is raster list, name and save accordingly

enviro_plot <- function(y, w) {
  factor_val <- c(2, 4, 8, 16, 32)
  enviro_list <- lapply(factor_val, function (x)
    aggregate(y, fac = x, fun = mean))
  enviro_list <- c(y, enviro_list)
  return(enviro_list)
  pdf(w)
  for (i in 1:6) {
    plot(enviro_list[[i]], main = paste('resolution =', res(enviro_list[[i]])))
    plot(continents, add = T, col = "black")
  }
  dev.off()
}


#temp plot
temp_list <- enviro_plot(temp_raster, './figures/temperature.pdf')
save(temp_list, file = './data/raster/temp_list.Rdata')
load('./data/raster/temp_list.Rdata')







