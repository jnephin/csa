require(plyr)
require(maptools)
require(PBSmapping)
require(ggplot2)
require(grid)
require(rgdal)
require(rgeos)
require(sp)
require(raster)
require(PBSmapping)
require(maptools)



Proj <- "+proj=aea +lon_0=110w +lat_0=0 +lat_1=30n +lat_2=60n +x_0=120w +y_0=0"


###############################
#-----  Regional Polygon  ----#


# land polygon
path <- file.path(.libPaths()[2],"PBSmapping/gshhg-bin-2.3.4/gshhs_f.b")
coast <-importGSHHS(path, xlim=c(215, 250), ylim=c(30,60),  n=15, maxLevel =1, useWest = T)
land <-PolySet2SpatialPolygons(coast) 
proj.world <- spTransform(land, Proj)
m.world <- fortify(proj.world)


#############################
#------  MAP OBJECTS  ------#

# generate map gridlines
grat <- gridlines(land, easts=c(-130,-125, -120), norths=c(40,50), ndiscr = 20) 
df <- data.frame(z = c(1,2), row.names=sapply(slot(grat, "lines"), function(x) slot(x, "ID")))
grat.df <- SpatialLinesDataFrame(grat, data = df)
proj.grat <- spTransform(grat.df, CRS=Proj)
grats <- fortify(proj.grat)
grats$Year <- rep(1998,nrow(grats))


# map axis labels
xlabels <- data.frame(x=c(-130,-125, -120),y=c(32.5, 33, 33.5))
xlabels$xlab <- xlabels$x
coordinates(xlabels) <- ~x + y
proj4string(xlabels) <- CRS("+proj=longlat")
xlabs <- spTransform(xlabels, Proj)
xlabs <- as.data.frame(xlabs)


ylabels <- data.frame(x=c(-134.5, -139),y=c(40,50))
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
scalebar$Year <- rep(1998,nrow(scalebar))

# arrow
arrowbar <- data.frame(x=-136,y=c(53,56))
coordinates(arrowbar) <- ~x + y
proj4string(arrowbar) <- CRS("+proj=longlat")
arrowbar <- spTransform(arrowbar, Proj)
arrowbar <- as.data.frame(arrowbar)
arrowbar <- cbind(arrowbar[1,],end = arrowbar[2,], lab = "N")
slope <- (arrowbar$end.y - arrowbar$y) / (arrowbar$end.x - arrowbar$x)
arrowbar$angle <- atan(slope)*180/pi -90
arrowbar$Year <- rep(1998,nrow(arrowbar))


# strip names
strptitle <- data.frame(Year = c(1998, 2001, 2003, 2005, 2007, 2009, 2011, 2012, 2013, 2015))
strptitle$y <- 6400000
strptitle$x <- -1000000


#########################################################################################
#########################################################################################
###  ANOMALY MAPS ##
#########################################################################################
#########################################################################################


# load sst raster
load("Data/Chl.NPO_AnomRaster.1998-2015.qd.rda") # CHL

# reclassify CHL
summary(CHL$anom)
brks <- c(min(CHL$anom),-40,-20,-10,-1,-.1,.1,1,10,20,40,max(CHL$anom))
CHL$reclass <- cut(CHL$anom, breaks = brks, include.lowest = T)


# sst colours 
red.pal <- colorRampPalette(c("#ffffff", "#FEE64C" , "#FC702A",  "#D9000B"), 
                            bias = 1.2, space = c("rgb", "Lab"))(6)
blue.pal <- colorRampPalette(c("#0B57DE", "#2EA0F2","#4ACCFF", "#ffffff"), 
                             bias = 1.2, space = c("rgb", "Lab"))(6)
pals <- c(blue.pal[1:6], red.pal[2:6])

pal <- data.frame(bin = sort(unique(CHL$reclass)), cols=pals)
pal$cols <- as.character(pal$cols)
pal$labs <- c("<-40", " -40", " -20", " -10", "  -1", "   0", 
              "   1", "  10", "  20", "  40", "> 40")

#sst dataset with colours
anon <- merge(CHL,pal, by.x="reclass",by.y="bin")



##################
# save anom map

plots <- c("April", "May", "June", "July", "August", "September", "Spring", "Summer")
mths <- c("04", "05", "06", "07", "08", "09", "Spring", "Summer")

for (i in 1:length(plots)){
  
  dat <- anon[anon$Month == mths[i],]
  
  name <- paste(plots[i],"\nChlorophyll a\nAnomalies ", sep="")
  
  sstPlot <- ggplot(data = NULL) +
    geom_raster(data = dat, aes(x=x, y=y, fill=factor(cols))) +
    scale_fill_identity(name, breaks = pal$cols, labels = pal$labs, guide = "legend") +
    facet_wrap( ~ Year, nrow = 2) +
    geom_polygon(data = m.world, aes(x=long, y=lat, group = group), 
                 fill = "gray75", colour = "gray65",size = 0.01) +
    geom_line(data = grats, aes(x=long, y=lat, group = group), size=.01, colour = "grey55") +
    geom_segment(data = scalebar, 
                 aes(x = x, y = y, xend = end.x, yend = end.y), 
                 size = 1, colour = "black", lineend = "butt") +
    geom_text(data = scalebar, 
              aes(x = xlab, y = y+80000, label =lab), 
              size = 2.5, colour = "black") +
    geom_segment(data = arrowbar, 
                 aes(x = x, y = y, xend = end.x, yend = end.y), 
                 size = .5, colour = "black", lineend = "round",
                 arrow = arrow(angle = 45, type = "open", 
                               length = unit(.2, "cm"))) +
    geom_text(data = arrowbar, 
              aes(x = mean(c(x,end.x)), y = mean(c(y,end.y)), label = lab, angle = angle), 
              size = 4, colour = "black", fontface = 2) +
    geom_text(data = strptitle, 
              aes(x = x, y = y, label = Year), 
              size = 3, colour = "black") +
    labs(x="", y="") +
    scale_x_continuous(breaks = xlabs$x, labels = xlabs$xlab) +
    scale_y_continuous(breaks = ylabs$y, labels = ylabs$ylab) +
    coord_fixed(xlim=c(-1900000, -800000), ylim=c(3675000, 6400000)) +
    theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
          panel.background = element_rect(fill="white",colour="white"),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=7, colour = "black"),
          axis.ticks.length = unit(0.1,"cm"),
          legend.text = element_text(size=8),
          legend.title = element_text(size=9, face="plain"),
          legend.background = element_blank(), legend.key = element_blank(),
          legend.justification = c(0,1), legend.position = c(1,.7), 
          plot.margin = unit(c(.2,5.5,.2,.2), "lines"), # top, right, bottom, and left 
          panel.margin = unit(0.2, "lines")) +
    guides(fill = guide_legend(title.position = "top", label.position = "right",
                         direction = "vertical", keyheight = 0.7,
                         keywidth = .8, label.hjust = 0))
  
  
  # Save as a pdf
  pdf(paste("Figures/CHL/Maps/CHLanom.1998-2015.Map",plots[i],"pdf",sep="."), 
      width=7, height=6) 
  print(sstPlot)
  dev.off()
  
}







#########################################################################################
#########################################################################################
###  CHLOROPHYLL MAPS  ###
#########################################################################################
#########################################################################################




# load sst raster
load("Data/CHL.NPO_Raster.1998-2015.qd.rda") # CHL

# reclassify CHL
summary(CHL$Chlor)
brks <- c(.1, .2, .3, .5, 1, 2, 3, 5, 10, 20, 30, 50, 100)
CHL$reclass <- cut(CHL$Chlor, breaks = brks, include.lowest = T)


# CHL colours 
cols <- colorRampPalette(c( "#14A0DB", "#26B0EB", "#47C8FF", "#62E300","#A7FA16","#FFEA00", "#FF8800","#DB0000"), bias = 1.1, space = c("rgb", "Lab"))(12)
pal <- data.frame(bin = sort(unique(CHL$reclass)), cols)
pal$cols <- as.character(pal$cols)
pal$labs <- c("  0.1", "  0.2", "  0.3", "  0.5", "  1", "  2", "  3", "  5", " 10", " 20", " 30", ">50")


#final chloro dataset with colours
chl <- merge(CHL,pal, by.x="reclass",by.y="bin")




##################
# save maps as pdf


plots <- c("April", "May", "June", "July", "August", "September", "Spring", "Summer")
mths <- c("04", "05", "06", "07", "08", "09", "Spring", "Summer")

for (i in 1:length(plots)){
  
  dat <- chl[chl$Month == mths[i],]
  
  name <- paste(plots[i],"\nChlorophyll a", sep="")
  
  
  sstPlot <- ggplot(data = NULL) +
    geom_raster(data = dat, aes(x=x, y=y, fill=factor(cols))) +
    scale_fill_identity(name, breaks = pal$cols, labels = pal$labs, guide = "legend") +
    facet_wrap( ~ Year, nrow = 2) +
    geom_polygon(data = m.world, aes(x=long, y=lat, group = group), 
                 fill = "gray75", colour = "gray65",size = 0.01) +
    geom_line(data = grats, aes(x=long, y=lat, group = group), size=.01, colour = "grey55") +
    geom_segment(data = scalebar, 
                 aes(x = x, y = y, xend = end.x, yend = end.y), 
                 size = 1, colour = "black", lineend = "butt") +
    geom_text(data = scalebar, 
              aes(x = xlab, y = y+80000, label =lab), 
              size = 2.5, colour = "black") +
    geom_segment(data = arrowbar, 
                 aes(x = x, y = y, xend = end.x, yend = end.y), 
                 size = .5, colour = "black", lineend = "round",
                 arrow = arrow(angle = 45, type = "open", 
                               length = unit(.2, "cm"))) +
    geom_text(data = arrowbar, 
              aes(x = mean(c(x,end.x)), y = mean(c(y,end.y)), label = lab, angle = angle), 
              size = 4, colour = "black", fontface = 2) +
    geom_text(data = strptitle, 
              aes(x = x, y = y, label = Year), 
              size = 3, colour = "black") +
    labs(x="", y="") +
    scale_x_continuous(breaks = xlabs$x, labels = xlabs$xlab) +
    scale_y_continuous(breaks = ylabs$y, labels = ylabs$ylab) +
    coord_fixed(xlim=c(-1900000, -800000), ylim=c(3675000, 6400000)) +
    theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
          panel.background = element_rect(fill="white",colour="white"),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size=7, colour = "black"),
          axis.ticks.length = unit(0.1,"cm"),
          legend.text = element_text(size=8),
          legend.title = element_text(size=9, face="plain"),
          legend.background = element_blank(), legend.key = element_blank(),
          legend.justification = c(0,1), legend.position = c(1,.7), 
          plot.margin = unit(c(.2,5.5,.2,.2), "lines"), # top, right, bottom, and left 
          panel.margin = unit(0.2, "lines")) +
    guides(fill = guide_legend(title.position = "top", label.position = "right",
                               direction = "vertical", keyheight = 0.7,
                               keywidth = .8, label.hjust = 0))
  
  
  
  
  # Save as a pdf
  pdf(paste("Figures/CHL/Maps/CHL.1998-2015.Map",plots[i],"pdf",sep="."), 
      width=7, height=6) 
  print(sstPlot)
  dev.off()
  
}









