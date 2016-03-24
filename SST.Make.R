require(plyr)
require(rgdal)
require(sp)
require(raster)

##################################
#--------    SST DATA   ---------#

# get monthly temps for each year between april - september between 1998 and 2015
years <- seq(1998, 2015, 1)
months <- c("04","05","06","07","08","09")
dates <- merge(years,months)
loopdates <- paste(dates$x,dates$y, sep= "")


#read in sst data for matching dates
sst.data <- NULL
for (i in loopdates
) {
  filename <- paste("Data/sst/","M",i,"qd_sst.csv", sep = "")
  sstdata <- read.csv(filename, header = T, row.names = 1)
  colnames(sstdata) <- c("Longitude", "Latitude", "Temp")
  sstdata$Date <- rep(i, nrow(sstdata))
  sstdata$Month <- rep(substr(i, 5,6), nrow(sstdata))
  sstdata$Year <- rep(substr(i, 1,4), nrow(sstdata))
  sst.data <- rbind(sst.data, sstdata)
}


#------------------ PACIFIC REGION ---------------------#

## clip to pacific region
sst.data <- sst.data[sst.data$Longitude > -145 & sst.data$Longitude < -115,] 
sst.data <- sst.data[sst.data$Latitude > 30 & sst.data$Latitude < 60,] 


## check
table(sst.data$Year, sst.data$Month)

#------------------------------------------------------#



##########   CLIMATOLOGIES   ###########

#### MONTH ###

## calculate monthly climatology (between 1998 to 2015)
month.avg <- ddply(sst.data, .(Longitude, Latitude, Month), transform, 
                   clim = mean(Temp))
## calculate temp anomalies
month.avg$anom <-  month.avg$Temp - month.avg$clim




#### SPRING (04,05,06)  ### 

## only spring months
sst.spring <- sst.data[sst.data$Month == "04" | 
                         sst.data$Month == "05" |
                            sst.data$Month == "06",]
## calculate spring means for each year (1998 to 2015)
spring.avg <- ddply(sst.spring, .(Longitude, Latitude, Year), summarise,
                    Temp = mean(Temp))
## calculate spring climatology (between 1998 to 2015)
spring.avg <- ddply(spring.avg, .(Longitude, Latitude), transform,
                    clim = mean(Temp))
## calculate the anomolies per grid cell
spring.avg$anom <-  spring.avg$Temp - spring.avg$clim
## add columns
spring.avg$Month <- "Spring"
spring.avg$Date <- paste(spring.avg$Year, "00", sep ="")





#### SUMMER (07,08,09) ###


## only summer months
sst.summer <- sst.data[sst.data$Month == "07" | 
                         sst.data$Month == "08" |
                         sst.data$Month == "09",]
## calculate summer means for each year (1998 to 2015)
summer.avg <- ddply(sst.summer, .(Longitude, Latitude, Year), summarise,
                    Temp = mean(Temp))
## calculate summer climatology (between 1998 to 2015)
summer.avg <- ddply(summer.avg, .(Longitude, Latitude), transform,
                    clim = mean(Temp))
## calculate the anomolies per grid cell
summer.avg$anom <-  summer.avg$Temp - summer.avg$clim
## add columns
summer.avg$Month <- "Summer"
summer.avg$Date <- paste(summer.avg$Year, "99", sep ="")




#########################################################################################
## combine month, spring and summer dataframes
sst <- rbind(month.avg, spring.avg[names(month.avg)], summer.avg[names(month.avg)])

## Only survey years
years <- c(1998, 2001, 2003, 2005, 2007, 2009, 2011, 2012, 2013, 2015)
sst <- sst[sst$Year %in% years,]


## check
table(sst$Year, sst$Month)


#########################################################################################
#########################################################################################
# export sst data for NPO region and track region

# NPO
save(sst, file="Data/SST.NPO.1998-2015.qd.rda")


#########################################################################################

# load track polygon
load("Data/TrackPolygon.rda") #bb.poly
bb.poly <- spTransform(bb.poly, CRS("+proj=longlat"))

#create spatial points dataframe
sst.track <- sst
coordinates(sst.track) <- ~Longitude + Latitude
proj4string(sst.track) <- CRS("+proj=longlat")
  
#clip sst data using track polygon bb
sst.bb <- sst.track[bb.poly, ]
plot(sst.bb)
sst.bb <- as.data.frame(sst.bb)

# track region
save(sst.bb, file="Data/SST.Track.1998-2015.qd.rda")



#########################################################################################





##########################################################################################
##########################################################################################
# make rasters (smaller resolution for plotting)

Proj <- "+proj=aea +lon_0=110w +lat_0=0 +lat_1=30n +lat_2=60n +x_0=120w +y_0=0"


# anomaly raster

SST <- NULL
for (i in sort(unique(sst$Date))){
  ras <- rasterFromXYZ(sst[sst$Date == i,c("Longitude","Latitude","anom")], 
                       crs="+proj=longlat")
  ras.p <-  projectRaster(ras, 
                          crs=Proj, 
                          res=20000) # 20 x 20 km grid
  ras.pts <- data.frame(rasterToPoints(ras.p))
  ras.pts$Date <- i
  SST <- rbind(SST, ras.pts)
}


SST$Year <- substr(SST$Date, 1, 4)
SST$Month <- substr(SST$Date, 5, 6)
SST$Month[SST$Month == "00"] <- "Spring"
SST$Month[SST$Month == "99"] <- "Summer"


#save sst anomaly raster
save(SST, file="Data/SST.NPO_AnomRaster.1998-2015.qd.rda")



##########################################################################################

# mean temp raster

SST <- NULL
for (i in sort(unique(sst$Date))){
  ras <- rasterFromXYZ(sst[sst$Date == i,c("Longitude","Latitude","Temp")], 
                       crs="+proj=longlat")
  ras.p <-  projectRaster(ras, 
                          crs=Proj, 
                          res=20000) # 20 x 20 km grid
  ras.pts <- data.frame(rasterToPoints(ras.p))
  ras.pts$Date <- i
  SST <- rbind(SST, ras.pts)
}


SST$Year <- substr(SST$Date, 1, 4)
SST$Month <- substr(SST$Date, 5, 6)
SST$Month[SST$Month == "00"] <- "Spring"
SST$Month[SST$Month == "99"] <- "Summer"


#save sst raster
save(SST, file="Data/SST.NPO_Raster.1998-2015.qd.rda")

##########################################################################################