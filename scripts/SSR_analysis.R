library(ggplot2)
library(dplyr)
library(viridis)


####QUICK GRAPHS####
#a few quick graphs with scale_output 
#bio ~ S 
with(scale_output, plot(bio ~ S))
#S ~ scale
with(scale_output, plot(S ~ scale))
#varbio ~ bio
with(scale_output, plot(varbio ~ bio))
#varbio ~ S
with(scale_output, plot(varbio ~ S))
#varbio ~ scale
with(scale_output, plot(varbio ~ scale))
#varbio ~ varS 
with(scale_output, plot(varbio ~ varS))
#stability ~ S
with(scale_output, plot(stability ~ S))
#stability ~ scale
with(scale_output, plot(stability ~ scale))


#varbio ~ scale. fill = startID
ggplot(data = scale_output, aes(x = scale, y = varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Var Biomass") +
  theme_bw()

#invarbio ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Invar Biomass") +
  theme_bw()

#varbio ~ S fill = scale
ggplot(data = scale_output, aes(x = S, y = varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("S") +
  ylab("Biomass Var") +
  theme_bw()

#invarbio ~ S fill = startID
ggplot(data = scale_output, aes(x = S, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("S") +
  ylab("Invar Biomass") +
  theme_bw()

#bio ~ S fill = scale
ggplot(data = scale_output, aes(x = S, y = bio, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

#bio ~ S fill = startID
ggplot(data = scale_output, aes(x = S, y = bio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

#S ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = S, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("S") +
  theme_bw()


#bio ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = bio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Biomass") +
  theme_bw()

#invar ~ scale fill = startID
ggplot(data = scale_output, aes(x = scale, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("Scale") +
  ylab(" Invar Biomass") +
  theme_bw()

#invar ~ S fill = startID
ggplot(data = scale_output, aes(x = scale, y = 1/varbio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  xlab("S") +
  ylab("Invar Biomass") +
  theme_bw()


#creating graphs like above but fill is center raster lat/long
coord <- as.data.frame(coordcenter)
coordreplat <- rep(coord$y, each = 36)
coordreplong <- rep(coord$x, each = 36)
scale_output_locate <- as.data.frame(cbind(scale_output, coordreplong, coordreplat))

ggplot(data = scale_output_locate, aes(x = scale, y = S, fill = coordreplat)) +
  geom_point(size = 4, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()



#### CURVE FOR EACH BOOTRUN ####

#averaging boot runs
av_scale_output <- scale_output %>%
  group_by(boot, scale) %>%
  summarize(bio = mean(bio),
            varbio = mean(varbio),
            S = mean(S),
            varS = mean(varS),
            stability = mean(stability)
            )

#a few quick graphs with av_scale_output 
#bio ~ S 
with(av_scale_output, plot(bio ~ S))
#S ~ scale
with(av_scale_output, plot(S ~ scale))
#varbio ~ bio
with(av_scale_output, plot(varbio ~ bio))
#varbio ~ S
with(av_scale_output, plot(varbio ~ S))
#varbio ~ scale
with(av_scale_output, plot(varbio ~ scale))
#varbio ~ varS 
with(av_scale_output, plot(varbio ~ varS))
#stability ~ S
with(av_scale_output, plot(stability ~ S))
#stability ~ scale
with(av_scale_output, plot(stability ~ scale))



#varbio ~ scale. fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, col = 2) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab("Var Biomass") +
  theme_bw()

with(av_scale_output, cor(scale, varbio))


#invarbio ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = stability, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T, col = 2) +
  scale_fill_viridis(option = "C") +
  labs(fill = "Bootstrap") +
  xlab("Scale") +
  ylab("Stability") +
  theme_bw()

with(av_scale_output, cor(scale, stability ))


#varbio ~ S fill = boot
ggplot(data = av_scale_output, aes(x = S, y = varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Biomass Var") +
  theme_bw()

with(av_scale_output, cor(S, varbio))

#invarbio ~ S fill = boot
ggplot(data = av_scale_output, aes(x = S, y = stability, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "C") +
  labs(fill = "Bootstrap") +
  xlab("S") +
  ylab("Stability") +
  theme_bw()

with(av_scale_output, cor(S, stability))

#bio ~ S fill = boot
ggplot(data = av_scale_output, aes(x = S, y = bio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "C") +
  labs(fill = "Bootstrap") +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

with(av_scale_output, cor(S, bio))

#bio ~ S fill = boot
#ggplot(data = av_scale_output, aes(x = S, y = bio, fill = boot)) +
 # geom_point(size = 4.5, shape = 21) +
  #geom_smooth(method = loess, size = 3, se = T) +
  #scale_fill_viridis(option = "C") +
  #xlab("S") +
  #ylab("Biomass") +
  #theme_bw()


#bio ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = bio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  #geom_smooth(method = "lm", col = 2, size = 3) +
  scale_fill_viridis(option = "C") +
  labs(fill = "Bootstrap") +
  xlab("Scale") +
  ylab("Biomass") +
  theme_bw()

with(av_scale_output, cor(scale, bio))

#invar ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = stability, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab(" Invar Biomass") +
  theme_bw()

with(av_scale_output, cor(scale, stability))

#S ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = S, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "C") +
  labs(fill = "Bootstrap") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()

with(av_scale_output, cor(scale, S))

#invar ~ S fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = 1/varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Invar Biomass") +
  theme_bw()


#major graph comparing SAR and SSR runs and averages

ggplot(data = av_scale_output, aes(x = scale, fill = boot)) +
 # geom_point(aes(y = ((stability)*2) - min((stability)*2)), size = 3, shape = 23 ) +
  #geom_point(aes(y = (S- min(S))), size = 6, shape = 21 ) +
  geom_ribbon(data = stabSUMM, aes(ymin = ((stability*2) - min(stability*2)) - ci,
                                   ymax = ((stability*2) - min(stability*2)) + ci), 
              fill = "grey70") +
  geom_ribbon(data = sSUMM, aes(ymin = (S - min(S)) - ci,
                                ymax = (S- min(S)) + ci), fill = "grey70") +
  geom_line(data = dat, aes(x= scale, y = (stability*2)-min(stability*2)),
            size = 4, color = "red") +
  geom_line(data= dat, aes(x = scale, y = (s - min(s))), size = 4) +
  scale_fill_viridis(option = "C") +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stability"))+
  labs(fill = "Bootstrap") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()

#no y axis change

ggplot(data = av_scale_output, aes(x = scale, fill = boot)) +
  geom_point(aes(y = ((stability)) - min((stability))), size = 3, shape = 23 ) +
  geom_point(aes(y = (S- min(S))), size = 6, shape = 21 ) +
 # geom_ribbon(data = stabSUMM, aes(ymin = ((stability) - min(stability)) - ci,
  #                                 ymax = ((stability) - min(stability)) + ci), 
   #           fill = "grey70") +
  #geom_ribbon(data = sSUMM, aes(ymin = (S - min(S)) - ci,
    #                            ymax = (S- min(S)) + ci), fill = "grey70") +
  geom_line(data = dat, aes(x= scale, y = (stability)-min(stability)),
            size = 4, color = "red") +
  geom_line(data= dat, aes(x = scale, y = (s - min(s))), size = 4) +
  scale_fill_viridis(aes(y= S), option = "C") +
  scale_y_continuous(sec.axis = sec_axis(~./1, name = "Stability"))+
  xlab("Scale") +
  ylab("S") +
  theme_bw()



#major graph without any the transformations

ggplot(data = av_scale_output, aes(x = scale, fill = boot)) +
  geom_point(aes(y = ((stability)) - min((stability))), size = 3, shape = 23 ) +
  geom_point(aes(y = (S)), size = 6, shape = 21 ) +
  #geom_ribbon(data = stabSUMM, aes(ymin = ((stability)) - ci,
   #                                ymax = ((stability)) + ci), 
    #          fill = "grey70") +
  #geom_ribbon(data = sSUMM, aes(ymin = (S - ci),
     #                           ymax = (S + ci)), fill = "grey70") +
  geom_line(data = dat, aes(x= scale, y = (stability)),
            size = 4, color = "red") +
  geom_line(data= dat, aes(x = scale, y = (s)), size = 4) +
  scale_fill_viridis(aes(y= S), option = "C") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()




#recreate Zhang et al analysis making curves linear with log transformations
S_scale_lm <- with(av_scale_output, lm(log10(S) ~ log10(scale)))
summary(S_scale_lm)
with(av_scale_output, plot(S ~ scale))
with(av_scale_output, plot(log10(S) ~ log10(scale)))
abline(S_scale_lm$coefficients)


ggplot(data = av_scale_output, aes(x = log10(scale), y = log10(S), fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = lm, size = 2, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("log Scale") +
  ylab(" log S") +
  theme_bw()


stab_scale_lm <- with(av_scale_output, lm(log10(stability) ~ log10(scale)))
summary(stab_scale_lm)
with(av_scale_output, plot(stability ~ scale))
with(av_scale_output, plot(log10(stability) ~ log10(scale)))
abline(stab_scale_lm$coefficients)


ggplot(data = av_scale_output, aes(x = log10(scale), y = log10(stability), fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = lm, size = 2, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("log Scale") +
  ylab("log Stability") +
  theme_bw()




#### SINGLE AVERAGED CURVE####
b <- with(av_scale_output, tapply(bio, list(scale), mean))
s <- with(av_scale_output, tapply(S, list(scale), mean))
ssd <- with(av_scale_output, tapply(S, list(scale), sd))
bvar <- with(av_scale_output, tapply(varbio, list(scale),mean))
bsd <- with(av_scale_output, tapply(bio, list(scale), sd))
stability <- with(av_scale_output, tapply(stability, list(scale), mean))

scale <- c(1:36)

plot(b ~ s, xlab = "S", ylab = "biomass", cex = 2, lwd = 2)
plot(bvar ~ s, xlab = "S", ylab = "biomass var", cex = 2, lwd = 2)
plot(stability ~ s, xlab = "S", ylab = "stability", cex = 2, lwd = 2)

plot(b ~ scale, xlab = "scale", ylab = "biomass", cex = 2, lwd = 2)
plot(bvar ~ scale, xlab = "scale", ylab = "biomass var", cex = 2, lwd = 2 )


plot(stability ~ scale, xlab = "scale", ylab = "stability", cex = 2, lwd = 2)
plot(s ~ scale, xlab = "scale", ylab = "S", cex = 2, lwd = 2)


dat <- as.data.frame(cbind(b, s, bvar, stability, scale))

ggplot(data = dat, aes(x = s, y = b, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

ggplot(data = dat, aes(x = s, y = binvar, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

ggplot(data = dat, aes(x = scale, y = binvar, fill = s)) +
  geom_point(size = 4.5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  xlab("Scale") +
  ylab("Invar Biomass") +
  theme_bw()


#Investigate Latitude 

#averaging by start ID runs
lat_scale_output <- scale_output %>% 
 dplyr::group_by(startID, scale) %>%
  dplyr::summarize(bio = mean(bio),
            varbio = mean(varbio),
            S = mean(S),
            varS = mean(varS),
            stability = 1/mean(varbio))

lat_scale_output <- as.data.frame(lat_scale_output)


#creating graphs like above but fill is center raster lat/long
coord <- as.data.frame(coordcenter)
coordreplat <- rep(coord$y, each = 36)
coordreplong <- rep(coord$x, each = 36)
lat_scale_output <- as.data.frame(cbind(lat_scale_output, coordreplong, coordreplat))


ggplot(data = lat_scale_output, aes(x = scale, y = S, fill = coordreplat)) +
  geom_point(size = 5, shape = 21) +
  #geom_smooth(data = dat, method = "lm" ) +
  scale_fill_viridis(option = "A") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()

#a few quick graphs with av_scale_output 
#bio ~ S 
with(lat_scale_output, plot(bio ~ S))
#S ~ scale
with(lat_scale_output, plot(S ~ scale))
#varbio ~ bio
with(lat_scale_output, plot(varbio ~ bio))
#varbio ~ S
with(lat_scale_output, plot(varbio ~ S))
#varbio ~ scale
with(lat_scale_output, plot(varbio ~ scale))
#varbio ~ varS 
with(lat_scale_output, plot(varbio ~ varS))
#stability ~ S
with(lat_scale_output, plot(stability ~ S))
#stability ~ scale
with(lat_scale_output, plot(stability ~ scale))



#stability through latitude 
with(lat_scale_output, boxplot(stability ~ coordreplat, 
        xlab = "Raster Centroid Latitude", ylab = "Stability"))

with(lat_scale_output, boxplot(S ~ coordreplat, 
       xlab = "Raster Centroid Latitude", ylab = "S"))

with(lat_scale_output, boxplot(bio ~ coordreplat, 
      xlab = "Raster Centroid Latitude", ylab = "Biomass"))


#varbio ~ scale. fill = lat
ggplot(data = lat_scale_output, aes(x = scale, y = varbio, fill = coordreplat)) +
  geom_point(size = 5, shape = 21) +
  geom_smooth(method = loess, size = 3, col = 2) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("Scale") +
  ylab("Var Biomass") +
  theme_bw()

with(lat_scale_output, cor(scale, varbio))


#invarbio ~ scale fill = lat
ggplot(data = lat_scale_output, aes(x = scale, y = stability, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T, col = 2) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("Scale") +
  ylab("Invar Biomass") +
  theme_bw()az

with(lat_scale_output, cor(scale, stability ))


#varbio ~ S fill = boot
ggplot(data = lat_scale_output, aes(x = S, y = varbio, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("S") +
  ylab("Biomass Var") +
  theme_bw()

with(lat_scale_output, cor(S, varbio))

#invarbio ~ S fill = boot
ggplot(data = lat_scale_output, aes(x = S, y = stability, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("S") +
  ylab("Stability") +
  theme_bw()

with(lat_scale_output, cor(S, stability))

#bio ~ S fill = boot
ggplot(data = lat_scale_output, aes(x = S, y = bio, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

with(lat_scale_output, cor(S, bio))


#bio ~ scale fill = boot
ggplot(data = lat_scale_output, aes(x = scale, y = bio, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  #geom_smooth(method = "lm", col = 2, size = 3) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("Scale") +
  ylab("Biomass") +
  theme_bw()

with(lat_scale_output, cor(scale, bio))

#invar ~ scale fill = boot
ggplot(data = lat_scale_output, aes(x = scale, y = stability, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("Scale") +
  ylab("Stability") +
  theme_bw()

with(lat_scale_output, cor(scale, stability))

#S ~ scale fill = boot
ggplot(data = lat_scale_output, aes(x = scale, y = S, fill = coordreplat)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  labs(fill = "Latitude") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()

with(lat_scale_output, cor(scale, S))



#big plot #not working right now
ggplot(data = lat_scale_output, aes(x = scale, fill = startID)) +
  geom_point(aes(y = ((stability)*2) - min((stability)*2)), size = 3, shape = 23 ) +
  geom_point(aes(y = (S- min(S))), size = 6, shape = 21 ) +
  geom_line(data = dat2, aes(x= scalel, y = (stabilityl*2)-min(stabilityl*2)),
            size = 4, color = "red") +
  geom_line(data= dat2, aes(x = scalel, y = (sl - min(sl))), size = 4) +
  scale_fill_viridis(aes(y= S), option = "C") +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stability"))+
  xlab("Scale") +
  ylab("S") +
  theme_bw()



##single curve latitude 
bl <- with(lat_scale_output, tapply(bio, list(scale), mean))
sl <- with(lat_scale_output, tapply(S, list(scale), mean))
ssdl <- with(lat_scale_output, tapply(S, list(scale), sd))
bvarl <- with(lat_scale_output, tapply(varbio, list(scale),mean))
bsdl <- with(lat_scale_output, tapply(bio, list(scale), sd))
stabilityl <- with(lat_scale_output, tapply(stability, list(scale), mean))
scalel <- c(1:36)
dat2 <- as.data.frame(cbind(bl, sl, bvarl, scalel, stabilityl))





#SCALE IMPACT ON BEF

bef_scale_output <- scale_output %>% 
  dplyr::group_by(scale) %>%
  dplyr::summarize(bio = mean(bio),
                   varbio = mean(varbio),
                   S = mean(S),
                   varS = mean(varS),
                   stability = 1/mean(varbio))

bef_scale_output <- as.data.frame(bef_scale_output)

#quick graphs
with(av_scale_output, plot(bio[scale == "2"] ~ s[scale == "2"]))





#### TAKE TWO #### 

#Figure 1: Map of Number of Trawls -- see Chap 1 take 2 figure 1a

#Figure 2:
#a) log biomass ~ log richness; fill = scale

scale_output_av_scto30 <- scale_output_av[scale_output_av$scale < 31,]

ggplot(data = scale_output_av_scto30,
       aes(x = log2(bio), y = log2(S), fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  #geom_errorbar( aes(ymin = log2(bio) - log2(biose) ,
  #ymax = log2(bio) + log2(biose),
  #xmin = log2(S) - log2(Sse),
  #xmax = log2(S) + log2(Sse)), width = 0.2) +
  xlab("log Species Richness (S)") +
  ylim(5, 7.5) +
  ylab("log Biomass (kg)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=20, color = "black"),
        axis.title=element_text(size=20,face="bold"))


#b) log stability ~ log richness; fill = scale

scale_output_av_scto30 <- scale_output_av[scale_output_av$scale < 31,]

ggplot(data = scale_output_av_scto30,
       aes(x = log2(stability), y = log2(S), fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
 #geom_errorbar( aes(ymin = log2(stability) - stabilityse ,
                   #ymax = log2(stability) + stabilityse,
                   #xmin = log2(S) - Sse,
                   #xmax = log2(S) + Sse), width = 0.2) +
  xlab("log Species Richness (S)") +
  ylim(5, 7.5) +
  ylab("log Stability of Biomass") +
  theme_bw() +
  theme(panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         axis.line = element_line(colour = "black"), 
         axis.text=element_text(size=20, color = "black"),
         axis.title=element_text(size=20,face="bold"))

#c) slope of (log biomass ~ log richness) ~ scale

ggplot(data = bioSscale_av[-c(31:36), ], aes(x = scale, y = slopeav)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  geom_errorbar( aes(ymin = slopeav-slopese, ymax = slopeav+slopese),width = 0.2) +
  xlab("Scale") +
  ylim(3.5, 7) +
  ylab("log Biomass (kg) ~ log Species Slope") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=20, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#d) slope of (log stability ~ log richness) ~ scale

ggplot(data = stabSscale_av[-c(31:36), ], aes(x = scale, y = slopeav)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  geom_errorbar( aes(ymin = slopeav-slopese, ymax = slopeav+slopese),width = 0.2) +
  xlab("Scale") +
  ylab("log stability ~ log Species Slope") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#Figure 3
#a) SAR

#to scale 36
ggplot(data = scale_output_av, aes(x = scale, y = S, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  #scale_fill_viridis(option = "A") +
  labs(fill = "Start Location") +
  xlab("Scale") +
  ylab("S") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#to scale 30
ggplot(data = scale_output_av_scto30, aes(x = scale, y = S, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  #scale_fill_viridis(option = "A") +
  labs(fill = "Start Location") +
  xlab("Scale") +
  ylab("Species Richness (S)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#single curve with error bars for difference between different starting locations
  #se represents error between starting locations at each scale

singlecurveSARSSR <- scale_output_av_scto30 %>%
  group_by(scale) %>%
  summarize(Sav = mean(S),
            Sse = sqrt(var(S)/length(S)), 
            bioav = mean(bio),
            biose = sqrt(var(bio)/length(bio)),
            stabilityav = mean(stability),
            stabilityse = sqrt(var(stability)/length(stability)))

names(singlecurveSARSSR) <- c("scale", "S", "Sse", "bio", "biose", "stability",
                              "stabilityse") 


ggplot(data = singlecurveSARSSR, aes(x = scale, y = S)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  geom_errorbar( aes(ymin = S - Sse, ymax = S + Sse), width = 0.2) +
  xlab("Scale") +
  ylim(50, 120) +
  ylab("Species Richness (S)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=20, color = "black"),
        axis.title=element_text(size=20,face="bold"))


#b) SSR

#to scale 36
ggplot(data = scale_output_av, aes(x = scale, y = stability, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  #scale_fill_viridis(option = "A") +
  labs(fill = "Start Location") +
  xlab("Scale") +
  ylab("Stability of Biomass") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#to scale 30
ggplot(data = scale_output_av_scto30, aes(x = scale, y = stability, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  labs(fill = "Start Location") +
  xlab("Scale") +
  ylab("Stability of Biomass") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#single curve
ggplot(data = singlecurveSARSSR, aes(x = scale, y = stability)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  geom_errorbar( aes(ymin = stability - stabilityse,
                     ymax = stability + stabilityse), width = 0.2) +
  xlab("Scale") +
  ylim(0, 17) +
  ylab("Stability of Biomass") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=20, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#c) Biomass Area Relationship

#to scale 36
ggplot(data = scale_output_av, aes(x = scale, y = bio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  #scale_fill_viridis(option = "A") +
  labs(fill = "Start Location") +
  xlab("Scale") +
  ylab("Biomass (kg)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#to scale 30
ggplot(data = scale_output_av_scto30, aes(x = scale, y = bio, fill = startID)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  labs(fill = "Start Location") +
  xlab("Scale") +
  ylab("Biomass (kg)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#single curve
ggplot(data = singlecurveSARSSR, aes(x = scale, y = bio)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  geom_errorbar( aes(ymin = bio - biose,
                     ymax = bio + biose), width = 0.2) +
  xlab("Scale") +
  ylab("Biomass (kg)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=20, color = "black"),
        axis.title=element_text(size=20,face="bold"))

#Table 1: Multiple Regression Interaction Effects 
#a) log biomass ~ log(S) + log(scale) + log(S * scale) ?? should scale be log
  
  #to scale 30
bioscalemod <- lm(log2(bio) ~ log2(S) * scale, data = scale_output_av_scto30)
summary(bioscalemod)

#b) log stability ~ log(S) + log(scale) + log(S*scale)
stabscalemod <- lm(log2(stability) ~ log2(S) * scale, data = scale_output_av_scto30)
summary(stabscalemod)






#averaging scale_output across bootstrap iterations #se in this case is between
  #boot iterations at the same location
scale_output_av <- scale_output1_20 %>%
  group_by(startID, scale) %>%
  summarize(Sav = mean(S),
            Sse = sqrt(var(S)/length(S)),
            varS = mean(varS),
            bioav = mean(bio),
            biose = sqrt(var(bio)/length(bio)),
            varbio = mean(varbio),
            stabilityav = mean(stability),
            stabilityse = sqrt(var(stability)/length(stability))
            )

names(scale_output_av) <- c( "startID", "scale", "S", "Sse", "varS", "bio", 
                            "biose", "varbio", "stability", "stabilityse" )
            

#log(biomass) ~ log(S) at each scale
tempslope <- NULL
tempintercpt <- NULL
tempscale <- NULL
tempboot <- NULL
boot_sub <- NULL

for (a in 1:20) { 
  boot_sub <- scale_output1_20[scale_output1_20$boot == a, ]
  for (i in 1:36) {
    mod <- with(boot_sub, lm(log2(bio[scale == i]) ~ log2(S[scale == i])))
    scale <- i 
    boot <- a
    slope <- mod$coefficients[2]
    intercpt <- mod$coefficients[1]
    
    tempslope <- c(tempslope, slope)
    tempintercpt <- c(tempintercpt, intercpt)
    tempscale <- c(tempscale, scale)
    tempboot <- c(tempboot, boot)
  }
}
bioSscale <- as.data.frame(cbind(tempboot, tempscale, tempslope, tempintercpt))
names(bioSscale) <- c("boot", "scale", "slope", "intercept")
plot(slope ~ scale, data = bioSscale, ylim= c(0, 15))


bioSscale_av <- bioSscale %>%
  group_by(scale) %>%
  summarize(slopeav = mean(slope),
            slopese = sqrt(var(slope)/length(slope)),
            interceptav = mean(intercept),
            interceptse = sqrt(var(intercept)/length(intercept)))

with(bioSscale_av, plot(slopeav ~ scale))


ggplot(data = bioSscale_av[-c(31:36), ], aes(x = scale, y = slopeav)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  geom_errorbar( aes(ymin = slopeav-slopese, ymax = slopeav+slopese),width = 0.2) +
  xlab("Scale") +
  ylim(3.5, 7) +
  ylab("log Biomass (kg) ~ log Species Slope") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=20, color = "black"),
        axis.title=element_text(size=20,face="bold"))





#log(stability) ~ log(s) at each scale
tempslope <- NULL
tempintercpt <- NULL
tempscale <- NULL
tempboot <- NULL
boot_sub <- NULL

for (a in 1:20) { 
  boot_sub <- scale_output1_20[scale_output1_20$boot == a, ]
  for (i in 1:36) {
    with(boot_sub, plot(log2(stability[scale == i]) ~ log2(S[scale == i])))
    mod <- with(boot_sub, lm(log2(stability[scale == i]) ~ log2(S[scale == i])))
    scale <- i 
    boot <- a
    slope <- mod$coefficients[2]
    intercpt <- mod$coefficients[1]

    tempslope <- c(tempslope, slope)
    tempintercpt <- c(tempintercpt, intercpt)
    tempscale <- c(tempscale, scale)
    tempboot <- c(tempboot, boot)
  }
}
stabSscale <- as.data.frame(cbind(tempboot, tempscale, tempslope, tempintercpt))
names(stabSscale) <- c("boot", "scale", "slope", "intercept")
plot(slope ~ scale, data = stabSscale, ylim= c(0, 15))


stabSscale_av <- stabSscale %>%
  group_by(scale) %>%
  summarize(slopeav = mean(slope),
            slopese = sqrt(var(slope)/length(slope)),
            interceptav = mean(intercept),
            interceptse = sqrt(var(intercept)/length(intercept)))

with(stabSscale_av, plot(slopeav ~ scale))


ggplot(data = stabSscale_av[-c(31:36), ], aes(x = scale, y = slopeav)) +
  geom_point(size = 4.5, shape = 16) +
  geom_smooth(method = loess, size = 3, se = F, col = 2) +
  scale_fill_viridis(option = "A") +
  geom_errorbar( aes(ymin = slopeav-slopese, ymax = slopeav+slopese),width = 0.2) +
  xlab("Scale") +
  ylab("log stability ~ log Species Slope") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text=element_text(size=25, color = "black"),
        axis.title=element_text(size=20,face="bold"))




