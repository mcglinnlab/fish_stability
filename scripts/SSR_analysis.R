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





