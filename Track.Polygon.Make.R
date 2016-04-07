require(alphahull)
require(sp)
require(rgeos)
require(PBSmapping)
require(maptools)

##################################
#--------     TRACK     ---------#

load("Data/Track.1998-2015.rda") #track


##################################
#--------     POLYGON    ---------#

## Create polygon surrounding track points
hull <- ahull(x= track[,1], y= track[,2], alpha = 1)
bb.pts <- track[hull$arcs[,"end1"],1:2]         # extract the boundary points from XY
bb.pts <- rbind(bb.pts,bb.pts[1,])                  # add the closing point
bb <- SpatialPolygons(list(Polygons(list(Polygon(bb.pts)),ID="s1"))) # create the SpatialPolygons

## project the polygon
Proj <- "+proj=aea +lon_0=110w +lat_0=0 +lat_1=30n +lat_2=60n +x_0=120w +y_0=0"
proj4string(bb) <- CRS("+proj=longlat")
bb.proj <- spTransform(bb, Proj)

## buffer
bb.poly <- gBuffer(bb.proj, width=60000) #buffer 60 km
plot(bb.poly)


#save bouding box polygon
save(bb.poly,file="Data/TrackPolygon.rda")



### to remove coastline


##################################
#--------   COASTLINE   ---------#

## load coastline
path <- file.path(.libPaths()[2],"PBSmapping/gshhg-bin-2.3.4/gshhs_f.b")
coast <-importGSHHS(path, xlim=c(215, 250), ylim=c(30,60),  n=150, maxLevel =1, useWest = T)
#landT<- thinPolys(coast, tol = 2) #remove small polys
land <-PolySet2SpatialPolygons(coast) 
land.proj <- spTransform(land, Proj)
land.smooth <- gSimplify(land.proj, tol =8000)
land.buf <- gBuffer(land.smooth, width = 500)

# plot
plot(bb.buf, col= "yellow")
plot(land.buf, col= "blue", add=T)

## clip the coastline
bb.poly <- gDifference(bb.buf, land.buf)
plot(bb.poly)


