
require(plyr)
require(maptools)
require(PBSmapping)
require(ggplot2)
require(grid)
require(rgdal)
require(rgeos)
require(sp)
require(raster)


Proj <- "+proj=aea +lon_0=110w +lat_0=0 +lat_1=30n +lat_2=60n +x_0=120w +y_0=0"


###############################
#-----      Polygons      ----#

# land polygon
path <- file.path(.libPaths()[2],"PBSmapping/gshhg-bin-2.3.4/gshhs_f.b")
coast <-importGSHHS(path, xlim=c(215, 250), ylim=c(30,66),  n=100, maxLevel =1, useWest = T)
land <-PolySet2SpatialPolygons(coast) 
proj.world <- spTransform(land, Proj)
smooth.world <- gSimplify(proj.world, tol = 500)
m.world <- fortify(smooth.world)

## track polygon
load("Data/TrackPolygon.rda")
bb.poly <- spTransform(bb.poly, Proj)
bb <- fortify(bb.poly)


##################################
#-------- STRATA DATA ---------#

strata <- read.csv("Data/Strata.csv", header=T)[-6,]

# generate map gridlines
strat <- gridlines(land, easts=c(-130,-125, -120), 
                   norths=strata$lat_upper, ndiscr = 20) 
dfs <- data.frame(z = c(1,2), row.names=sapply(slot(strat, "lines"), 
                                               function(x) slot(x, "ID")))
strat.df <- SpatialLinesDataFrame(strat, data = dfs)
strat.proj <- spTransform(strat.df, CRS=Proj)
strat.grat <- fortify(strat.proj)
strat.grat <- strat.grat[strat.grat$id == "NS",]



#############################
#------  MAP OBJECTS  ------#

# generate map gridlines
grat <- gridlines(land, easts=c(-130,-125, -120), norths=c(40,50), ndiscr = 20) 
df <- data.frame(z = c(1,2), row.names=sapply(slot(grat, "lines"), function(x) slot(x, "ID")))
grat.df <- SpatialLinesDataFrame(grat, data = df)
proj.grat <- spTransform(grat.df, CRS=Proj)
grats <- fortify(proj.grat)
grats <- grats[grats$id == "EW",]


# map axis labels
xlabels <- data.frame(x=c(-130,-125, -120),y=c(30.5, 31, 32))
xlabels$xlab <- xlabels$x
coordinates(xlabels) <- ~x + y
proj4string(xlabels) <- CRS("+proj=longlat")
xlabs <- spTransform(xlabels, Proj)
xlabs <- as.data.frame(xlabs)


ylabels <- data.frame(x=c(-134,-135.25,-136.6,-138,-139.5),
                      y=strata$lat_upper)
ylabels$ylab <- ylabels$y
coordinates(ylabels) <- ~x + y
proj4string(ylabels) <- CRS("+proj=longlat")
ylabs <- spTransform(ylabels, Proj)
ylabs <- as.data.frame(ylabs)


# scale bar
p1 <- data.frame(x=-129,y=33)
coordinates(p1) <- ~x + y
proj4string(p1) <- CRS("+proj=longlat")
p1 <- spTransform(p1, Proj)

p2 <- data.frame(x=-126,y=34)
coordinates(p2) <- ~x + y
proj4string(p2) <- CRS("+proj=longlat")
p2 <- spTransform(p2, Proj)

gDistance(p1,p2)/1000

scalebar <- cbind(as.data.frame(p1),end = as.data.frame(p2), lab = "300 km")
scalebar$end.y <- scalebar$y
scalebar$xlab <- mean(c(scalebar$x,scalebar$end.x))

# arrow
arrowbar <- data.frame(x=-122.2,y=c(58,60))
coordinates(arrowbar) <- ~x + y
proj4string(arrowbar) <- CRS("+proj=longlat")
arrowbar <- spTransform(arrowbar, Proj)
arrowbar <- as.data.frame(arrowbar)
arrowbar <- cbind(arrowbar[1,],end = arrowbar[2,], lab = "N")
slope <- (arrowbar$end.y - arrowbar$y) / (arrowbar$end.x - arrowbar$x)
arrowbar$angle <- atan(slope)*180/pi -90



############################
#-------- Citites ---------#

cities <- read.csv("Data/Cities.csv",header=T)
coordinates(cities) <- c("X", "Y")
proj4string(cities) <- CRS("+proj=longlat")
proj.cities <- spTransform(cities, Proj)
cities <- as.data.frame(proj.cities)



##################################
#-------- ACOUSTIC DATA ---------#

load("Data/Track.1998-2015.rda") #track

#project track lat and long
coordinates(track) <- ~Lon+Lat
proj4string(track) <- CRS("+proj=longlat")
proj.track <- spTransform(track, Proj)
track <- as.data.frame(proj.track)



###############################################################################
##############################################################################
#plots

extentplot <- ggplot(data = NULL) +
  #strata
  geom_line(data = strat.grat, aes(x=long, y=lat, group = group), 
            size=1, colour = "black") +
  #land polygon
  geom_polygon(data = m.world, aes(x=long, y=lat, group = group), 
               fill = "gray75", colour = NA,size = 0.01) +
  # track polygon
  geom_polygon(data = bb, aes(x = long, y = lat), 
             fill=NA, colour = "red", size = 1) + 
  # track polygon
  geom_text(data = ylabs, aes(x = x+200000, y = y+10000, label = ylab), 
               colour = "black", size = 4, angle = -11, hjust = 0) + 
  # axes
  labs(x="", y="") +
  scale_x_continuous(breaks = xlabs$x, labels = xlabs$xlab) +
  geom_segment(data = scalebar, 
               aes(x = x, y = y, xend = end.x, yend = end.y), 
               size = 1.2, colour = "black", lineend = "butt") +
  geom_text(data = scalebar, 
            aes(x = xlab, y = y+80000, label =lab), 
            size = 3, colour = "black") +
  geom_segment(data = arrowbar, 
               aes(x = x, y = y, xend = end.x, yend = end.y), 
               size = .5, colour = "black", lineend = "round",
               arrow = arrow(angle = 45, type = "open", 
                             length = unit(.3, "cm"))) +
  geom_text(data = arrowbar, 
            aes(x = mean(c(x,end.x)), y = mean(c(y,end.y)), 
                label = lab, angle = angle), 
            size = 5, colour = "black", fontface = 2) +
  # cities
  geom_point(data = cities, aes(x = X, y = Y), 
             pch=20, colour = "black",  size = 2) +
  geom_text(data = cities, aes(x = X, y = Y, label=label), 
            colour = "black", size = 3, hjust = -.2) +
#themes 
coord_fixed(xlim=c(-1900000, -500000), ylim=c(3500000, 6500000))  +
theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
      panel.background = element_rect(fill="white",colour="white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size=8, colour = "black"),
      axis.text.y = element_blank(),
      axis.ticks.length = unit(0.1,"cm"),
      axis.ticks.y= element_blank(),
      axis.title = element_blank(),
      plot.margin = unit(c(.3,.3,.3,.3), "lines")) # trbl 
extentplot


# Save as a pdf
pdf("Figures/TrackRegion.Map.pdf", width=3, height=6) 
extentplot
dev.off()




###############################################################################
##############################################################################
#plots

yearsplot <- ggplot(data = NULL) +
  facet_wrap(~year, nrow = 2)+
  # survey track
  geom_point(data = track, aes(x = Lon, y = Lat), size = .05, pch = 16) +
  #land polygon
  geom_polygon(data = m.world, aes(x=long, y=lat, group = group), 
               fill = "gray75", colour = "gray65",size = 0.01) +
  # track polygon
  geom_polygon(data = bb, aes(x = long, y = lat), fill=NA, 
               colour = "red", size = .1) + 
  # axes
  labs(x="", y="") +
  scale_x_continuous(breaks = xlabs$x, labels = xlabs$xlab) +
  scale_y_continuous(breaks = ylabs$y, labels = ylabs$ylab) +
  #themes 
  coord_fixed(xlim=c(-1650000, -650000), ylim=c(3500000, 6600000))  +
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        axis.ticks.length = unit(0.05,"cm"),
        axis.title = element_blank(),
        plot.margin = unit(c(.5,.5,.3,.3), "lines"),
        panel.margin = unit(0,"lines")) # top, right, bottom, and left 
yearsplot


# Save as a pdf
pdf("Figures/TrackRegion.Years.Map.pdf", width=6, height=8) 
yearsplot
dev.off()
