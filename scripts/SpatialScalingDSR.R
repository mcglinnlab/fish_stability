
#Spatial Scaling of DSR

# Idea for how to write spatial part
  #Make a loop that randomize the list of ID squares
  #Loop makes calcs of var of invar and average species richness with 1 square included, 2 square until length(ID)
  #Rerandomize list of IDs and repeat many many times to create curves
  #Average curves


#renaming dataframe so results dataframe stays intact

ssr_results <- resultsfullpoint2

#fake data set to try things on
#a <- c(1,2,3,4,5,6,7,8,9,10)
#b <- c(2,3,5,6,7,3,5,4,5,6)
#ab <- data.frame(cbind(a,b))



#examples of reordering data set
#rows <- sample(nrow(ab))
#abnew <- ab[rows, ]


for (p in 1:5) {
  rows <- sample(nrow(ssr_results))
  ssr_new <- ssr_results[rows, ]
  
  for (a in 1:length(ssr_results)){
    #selects 1 row then 2 then 3 until all row included
    sub_ssr <- ssr_new[1:a, ]
    
    #vector of cumulative average of biomass and species as more IDs added
    bio[a] <- average(sub_ssr$averagebio)
    S[a] <- average(sub_ssr$averageS)
    
    #creates vectors of just varbio and averagebio columns to be used to calc 
      #new cv and new invar for growing number of raster IDs
    varbio[a] <- sub_ssr$varbio
    averabiobio[a] <- sub_ssr$averagebio
    
    #calculating cv and invar each time a new raster ID is added
    cv[a] <- varbio[a]/(averagebio[a] ^ 2)
    invar[a] <- 1/(cv[a])
    
    
    #creating columns that tell me how many raster IDs are included (numID) and 
    # which reshuffling the row is associated with (run)
    numID <- a
    run <- p
    
    #creating data frame to save to
    tempcurve <- data.frame(cbind(numID, run, bio, S, CV, invar))
    tempcurve <- colnames("numID", "run", "bio", "S", "CV", "invar")
  }
  curve <- rbind(curve, tempcurve)  
}






