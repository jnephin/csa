require(plyr)
library(rgdal)
require(sp)
require(ggplot2)

##################################
#-------- ACOUSTIC DATA ---------#
##################################

load("Data/Track.1998-2015.rda") #track


# sum NASC latitude (at 0.01 resolution, roughly 1 km in distance)
track$Latitude <- round(track$Lat,2)
sum.track <- ddply(track, .(Latitude, year), transform, 
                   Biomass.density = sum(Biomass.density))
sum.track <- ddply(sum.track, .(year), function(x) {x[!duplicated(x$Latitude),]})

# sum the NASC values cummulatively by year
ord.track <- sum.track[order(sum.track$Latitude),]
cumtrack <- ddply(ord.track, "year", transform, cumBiomass = cumsum(Biomass.density))

#percent NASC
cumtrack <- ddply(cumtrack, "year", transform, maxBiomass = tail(cumBiomass,1))
cumtrack$cdf <-  cumtrack$cumBiomass/cumtrack$maxBiomass



##################################
#-------- SPATIAL CLIP  ---------#
##################################

# load canadian eez shapefile
eez.poly <- readOGR(dsn="Data/Shapefiles", layer="EEZ")
eez <- fortify(eez.poly)

# transform into spatial dataframe and project
coordinates(cumtrack) <- ~ Lon + Lat
proj4string(cumtrack) <- CRS("+proj=longlat")
proj.cumtrack <- spTransform(cumtrack, proj4string(eez.poly))

# clip
can.cumtrack <- proj.cumtrack[eez.poly,]
ind <- as.numeric(row.names(can.cumtrack))


# reproject and convert back to data.frame
cumtrack <- spTransform(proj.cumtrack, CRS("+proj=longlat"))
cumtrack <- as.data.frame(cumtrack)

# add US / can labels
cumtrack$grp <- "us"
cumtrack$grp[ind] <- "can"



##################################
#----------- INDICES ------------#
##################################

#plot settings
par(mar=c(3,3,2.5,1), mfrow = c(2,2))

#### Index 1) 
#### Percentage of Hake pop in Canadian waters
pcan <- ddply(cumtrack, .(year), transform, yrtl = sum(Biomass.density)) 
pcan <- ddply(pcan, .(year,grp), summarise, percent = sum(Biomass.density)/min(yrtl))
pcan <- pcan[pcan$grp == "can",]
plot(y = pcan$percent, x = factor(pcan$year), main = "1) Percentage of Hake in Canadian Waters", cex.main =.8, cex.axis=.6)

#### Index 2) 
#### Latitude of the 50th percentile of Biomass
p50 <- ddply(cumtrack, .(year), function(x) {x[which(abs(x$cdf-.5) == min(abs(x$cdf-.5))),]})
p50 <- aggregate(Latitude ~ cdf + year, FUN = mean, data = p50)
plot(y = p50$Latitude, x = factor(p50$year), main = "2) Latitude where 50th percentile of biomass is reached", cex.main =.8, cex.axis=.6)

#### Index 3) 
#### Latitude of the 75th percentile of Biomass
p75 <- ddply(cumtrack, .(year), function(x) {x[which(abs(x$cdf-.75) == min(abs(x$cdf-.75))),]})
p75 <- aggregate(Latitude ~ cdf + year, FUN = mean, data = p75)
plot(y = p75$Latitude, x = factor(p75$year), main = "3) Latitude where 75th percentile of biomass is reached",  cex.main =.8, cex.axis=.6)

#### Index 4) 
#### Biomass in canadian water - divided by 4 to change units from 1 to .5 nmi sq 
bcan <- ddply(cumtrack, .(year,grp), summarise, totalbio = sum(Biomass.density)/4)
bcan <- bcan[bcan$grp == "can",]
bcan$totalbio <- bcan$totalbio/1000/1000 # units = kmt
plot(y = bcan$totalbio, x = factor(bcan$year), main = "4) Biomass (kmt)  in Canadian waters",  cex.main =.8, cex.axis=.6)


## write indices to file
indices <- data.frame(year = pcan$year, 
                     pCan = pcan$percent*100,
                     bCan = bcan$totalbio,
                     Lat50 = p50$Latitude,
                     Lat75 = p75$Latitude)

## correlation between indices
cor(indices[,-1], method = "spearman")

## save indices
save(indices, file="Data/LatIndices.rda")
