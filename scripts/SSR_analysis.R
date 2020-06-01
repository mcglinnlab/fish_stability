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
            stability = 1/mean(varbio)
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
  geom_smooth(method = loess, size = 3) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab("Var Biomass") +
  theme_bw()

#invarbio ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = 1/varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab("Invar Biomass") +
  theme_bw()

#varbio ~ S fill = boot
ggplot(data = av_scale_output, aes(x = S, y = varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Biomass Var") +
  theme_bw()

#invarbio ~ S fill = boot
ggplot(data = av_scale_output, aes(x = S, y = 1/varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Invar Biomass") +
  theme_bw()

#bio ~ S fill = scale
ggplot(data = av_scale_output, aes(x = S, y = bio, fill = scale)) +
  geom_point(size = 4.5, shape = 21) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()

#bio ~ S fill = boot
ggplot(data = av_scale_output, aes(x = S, y = bio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("S") +
  ylab("Biomass") +
  theme_bw()


#bio ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = bio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab("Biomass") +
  theme_bw()

#invar ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = 1/varbio, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 3, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab(" Invar Biomass") +
  theme_bw()

#S ~ scale fill = boot
ggplot(data = av_scale_output, aes(x = scale, y = S, fill = boot)) +
  geom_point(size = 4.5, shape = 21) +
  geom_smooth(method = loess, size = 1, se = T) +
  scale_fill_viridis(option = "C") +
  xlab("Scale") +
  ylab("S") +
  theme_bw()

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
  geom_point(aes(y = ((1/varbio)*7) - min((1/varbio)*7)), size = 3, shape = 23 ) +
  geom_point(aes(y = (S- min(S))), size = 6, shape = 21 ) +
  geom_ribbon(data = stabSUMM, aes(ymin = ((stability*7) - min(stability*7)) - ci,
                                   ymax = ((stability*7) - min(stability*7)) + ci), 
              fill = "grey70") +
  geom_ribbon(data = sSUMM, aes(ymin = (S - min(S)) - ci,
                                ymax = (S- min(S)) + ci), fill = "grey70") +
  geom_line(data = dat, aes(x= scale, y = (stability*7)-min(stability*7)),
            size = 4, color = "red") +
  geom_line(data= dat, aes(x = scale, y = (s - min(s))), size = 4) +
  scale_fill_viridis(aes(y= S), option = "C") +
  scale_y_continuous(sec.axis = sec_axis(~./7, name = "Stability"))+
  xlab("Scale") +
  ylab("S") +
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


dat <- as.data.frame(cbind(b, s, bvar, binvar, scale, stability))

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

