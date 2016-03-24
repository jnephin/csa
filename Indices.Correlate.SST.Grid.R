require(plyr)
require(ggplot2)
require(PBSmapping)
require(sp)
require(rgeos)
require(maptools)
source("base.plot.R")

####################################################################################
####################################################################################
## which areas have the greatest correlation between sst anomalies and Hake indices?


# load data
load("Data/LatIndices.rda") #indices
load("Data/SST.NPO_AnomRaster.1998-2015.qd.rda") #SST
colnames(SST)[5] <- "year"

load("Data/TrackPolygon.rda") #bb.poly
poly <- fortify(bb.poly)


#proj string
Proj <- "+proj=aea +lon_0=110w +lat_0=0 +lat_1=30n +lat_2=60n +x_0=120w +y_0=0"




###############################
#-----  Regional Polygon  ----#


# land polygon
path <- file.path(.libPaths()[2],"PBSmapping/gshhg-bin-2.3.4/gshhs_f.b")
coast <-importGSHHS(path, xlim=c(215, 250), ylim=c(30,62),  n=15, maxLevel =1, useWest = T)
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
grats$Month <- rep("April",nrow(grats))
grats$Month <- factor(grats$Month, 
                      levels = c("April","May","June","July","August","September","Spring","Summer"))


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
p1 <- data.frame(x=-128,y=33)
coordinates(p1) <- ~x + y
proj4string(p1) <- CRS("+proj=longlat")
p1 <- spTransform(p1, Proj)

p2 <- data.frame(x=-125,y=34)
coordinates(p2) <- ~x + y
proj4string(p2) <- CRS("+proj=longlat")
p2 <- spTransform(p2, Proj)

gDistance(p1,p2)/1000

scalebar <- cbind(as.data.frame(p1),end = as.data.frame(p2), lab = "300 km")
scalebar$end.y <- scalebar$y
scalebar$xlab <- mean(c(scalebar$x,scalebar$end.x))
scalebar$Month <- rep("April",nrow(scalebar))
scalebar$Month <- factor(scalebar$Month, 
                         levels = c("April","May","June","July","August","September","Spring","Summer"))


# arrow
arrowbar <- data.frame(x=-128,y=c(54,57))
coordinates(arrowbar) <- ~x + y
proj4string(arrowbar) <- CRS("+proj=longlat")
arrowbar <- spTransform(arrowbar, Proj)
arrowbar <- as.data.frame(arrowbar)
arrowbar <- cbind(arrowbar[1,],end = arrowbar[2,], lab = "N")
slope <- (arrowbar$end.y - arrowbar$y) / (arrowbar$end.x - arrowbar$x)
arrowbar$angle <- atan(slope)*180/pi -90
arrowbar$Month <- rep("April",nrow(arrowbar))
arrowbar$Month <- factor(arrowbar$Month, 
                         levels = c("April","May","June","July","August","September","Spring","Summer"))


# strip names
strptitle <- data.frame(Month = c("April","May","June","July","August","September","Spring","Summer"))
strptitle$Month <- factor(strptitle$Month, 
                          levels = c("April","May","June","July","August","September","Spring","Summer"))
strptitle$y <- 6500000
strptitle$x <- -1000000


## colour palette 
red.pal <- colorRampPalette(c("#ffffff","#FCF3B6","#FEE64C" , "#FC702A",  "#C9000A"), 
                            bias = .5, space = c("rgb", "Lab"))(6)
blue.pal <- colorRampPalette(c("#024AC9", "#2EA0F2","#4ACCFF", "#AAE3FA","#ffffff"), 
                             bias = 2, space = c("rgb", "Lab"))(6)
pal <- c(blue.pal[1:6], red.pal[2:6])



##############################################################
##         Correlate sst anomalies and hake indices         ##
##############################################################

## merge
sst.ind <- merge(SST, indices, by = "year")

## function that correlates sst anomalies with an index 
## correlation coefficient
cors <- function(x,ind) {
  cor(x$anom, x[[ind]], method = "spearman")
}


##############################################################
## loop through each index
## correlate  anom  with each index

for (i in c("pCan" , "bCan" , "Lat50" , "Lat75")){
  
ind <- ddply(sst.ind, .(x, y, Month), 
               cors, ind = i)

## rename months for plot
ind$Month[ind$Month == "04"] <- "April"
ind$Month[ind$Month == "05"] <- "May"
ind$Month[ind$Month == "06"] <- "June"
ind$Month[ind$Month == "07"] <- "July"
ind$Month[ind$Month == "08"] <- "August"
ind$Month[ind$Month == "09"] <- "September"
ind$Month <- as.factor(ind$Month) 
ind$Month <- factor(ind$Month, 
                   levels = c("April","May","June","July","August","September","Spring","Summer"))



###################################
## maps of correlation coefficient

## percent in canada

plots <- ggplot(data = NULL) +
  geom_raster(data = ind, aes(x=x, y=y, fill=V1)) +
  scale_fill_gradientn("Spearman's\nCorrelation\nCoefficient", 
                       colours = pal, limits =c(-1,1))+  
  facet_wrap( ~ Month, nrow = 2) +
  geom_polygon(data = poly, aes(x=long, y=lat, group = group), 
               fill = NA, colour = "black",size = 0.5) +
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
            aes(x = x, y = y, label = Month), 
            size = 3, colour = "black") +
  labs(x="", y="") +
  scale_x_continuous(breaks = xlabs$x, labels = xlabs$xlab) +
  scale_y_continuous(breaks = ylabs$y, labels = ylabs$ylab) +
  coord_fixed(xlim=c(-1700000, -600000), ylim=c(3450000, 6500000)) +
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
        panel.margin = unit(0.2, "lines"))
plots

pdf(paste0("Figures/LatIndices/Map/Cor.SST.",i,".Map.1998-2015.pdf"), width=6, height=6) 
print(plots)
dev.off()

}

