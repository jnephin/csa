require(plyr)
require(rgdal)
require(sp)
require(raster)


# units = Chlorophyll a concentration (mg/m^3)


##################################
#-------  Chloro DATA   --------#

# dates of interest -> april - september between 1998 and 2015
years <- seq(1998, 2015, 1)
months <- c("04","05","06","07","08","09")
dates <- merge(years,months)
intdates <- paste(dates$x,dates$y, sep= "")


# list files from modis and SeaWiFS for those months
chl.list <- list.files(path = paste(getwd(),"/Data/chl",sep=""), pattern="km9_oc6.csv")
chl.files <- grep(paste0(intdates, collapse = "|"), chl.list,  value = TRUE)


#read in chl from MODIS and SeaWiFS for matching dates
chl.data <- NULL
for (i in chl.files) {
  tmp <- read.csv(file.path("Data/Chl",i), header = T)
  colnames(tmp) <- c("Longitude", "Latitude", "Chlor")
  tmp$Date <- substr(i, 5,10)
  tmp$Month <- substr(i, 9,10)
  tmp$Year <-substr(i, 5,8)
  chl.data <- rbind(chl.data, tmp)
}


#------------------ PACIFIC REGION ---------------------#

## clip to pacific region
chl.data <- chl.data[chl.data$Longitude > -145 & chl.data$Longitude < -115,] 
chl.data <- chl.data[chl.data$Latitude > 30 & chl.data$Latitude < 60,] 

## check
table(chl.data$Year, chl.data$Month)

#------------------------------------------------------#






#########################################################################################
###########################         CLIMATOLOGIES        ################################
#########################################################################################


#### MONTH ###

## calculate monthly climatology (between 1998 to 2015)
month.avg <- ddply(chl.data, .(Longitude, Latitude, Month), transform, 
                   clim = mean(Chlor, na.rm=T))
## calculate temp anomalies
month.avg$anom <-  month.avg$Chlor - month.avg$clim




#### SPRING (04,05,06)  ### 

## only spring months
chl.spring <- chl.data[chl.data$Month == "04" | 
                         chl.data$Month == "05" |
                            chl.data$Month == "06",]
## calculate spring means for each year (1998 to 2015)
spring.avg <- ddply(chl.spring, .(Longitude, Latitude, Year), summarise,
                    Chlor = mean(Chlor, na.rm=T))
## calculate spring climatology (between 1998 to 2015)
spring.avg <- ddply(spring.avg, .(Longitude, Latitude), transform,
                    clim = mean(Chlor, na.rm=T))
## calculate the anomolies per grid cell
spring.avg$anom <-  spring.avg$Chlor - spring.avg$clim
## add columns
spring.avg$Month <- "Spring"
spring.avg$Date <- paste(spring.avg$Year, "00", sep ="")





#### SUMMER (07,08,09) ###

## only summer months
chl.summer <- chl.data[chl.data$Month == "07" | 
                         chl.data$Month == "08" |
                         chl.data$Month == "09",]
## calculate summer means for each year (1998 to 2015)
summer.avg <- ddply(chl.summer, .(Longitude, Latitude, Year), summarise,
                    Chlor = mean(Chlor, na.rm=T))
## calculate summer climatology (between 1998 to 2015)
summer.avg <- ddply(summer.avg, .(Longitude, Latitude), transform,
                    clim = mean(Chlor, na.rm=T))
## calculate the anomolies per grid cell
summer.avg$anom <-  summer.avg$Chlor - summer.avg$clim
## add columns
summer.avg$Month <- "Summer"
summer.avg$Date <- paste(summer.avg$Year, "99", sep ="")




#########################################################################################
## combine month, spring and summer dataframes
chl <- rbind(month.avg, spring.avg[names(month.avg)], summer.avg[names(month.avg)])

## Only survey years
years <- c(1998, 2001, 2003, 2005, 2007, 2009, 2011, 2012, 2013, 2015)
chl <- chl[chl$Year %in% years,]

## check
table(chl$Year, chl$Month)

#########################################################################################
#########################################################################################







#########################################################################################
#########################################################################################
# export chl data for NPO region and track region

# NPO
save(chl, file="Data/Chl.NPO.1998-2015.qd.rda")

#########################################################################################

# load track polygon
load("Data/TrackPolygon.rda") #bb.poly
bb.poly <- spTransform(bb.poly, CRS("+proj=longlat"))

#create spatial points dataframe
chl.track <- chl
coordinates(chl.track) <- ~Longitude + Latitude
proj4string(chl.track) <- CRS("+proj=longlat")
  
#clip chl data using track polygon bb
chl.bb <- chl.track[bb.poly, ]
plot(chl.bb)
chl.bb <- as.data.frame(chl.bb)

# track region
save(chl.bb, file="Data/Chl.Track.1998-2015.qd.rda")

#########################################################################################





##########################################################################################
##########################################################################################
# mean chloro for the coast


# add us and canada labels (>48.5 cut-off)
chl.bb$grp <- "US"
chl.bb$grp[chl.bb$Latitude >= 48.5] <- "Canada"

# mean
mean.chl <- ddply(chl.bb[chl.bb$Month %in% c("04","05","06","07","08","09"),], 
                  .(grp, Month, Year), summarise, 
                  value = mean(Chlor, na.rm=T),
                  anom = mean(anom, na.rm=T))

# save mean chl
save(mean.chl, file = "Data/Mean.chl.rda")





#########################################################################################
#########################################################################################
# make rasters (smaller resolution for plotting)

#load
#load("Data/Chl.NPO.1998-2015.qd.rda")

#projection
Proj <- "+proj=aea +lon_0=110w +lat_0=0 +lat_1=30n +lat_2=60n +x_0=120w +y_0=0"


# anomaly raster

CHL <- NULL
for (i in sort(unique(chl$Date))){
  ras <- rasterFromXYZ(chl[chl$Date == i,c("Longitude","Latitude","anom")], 
                       crs="+proj=longlat")
  ras.p <-  projectRaster(ras, 
                          crs=Proj, 
                          res=20000) # 20 x 20 km grid
  ras.pts <- data.frame(rasterToPoints(ras.p))
  ras.pts$Date <- i
  CHL <- rbind(CHL, ras.pts)
}


CHL$Year <- substr(CHL$Date, 1, 4)
CHL$Month <- substr(CHL$Date, 5, 6)
CHL$Month[CHL$Month == "00"] <- "Spring"
CHL$Month[CHL$Month == "99"] <- "Summer"


#save chl anomaly raster
save(CHL, file="Data/Chl.NPO_AnomRaster.1998-2015.qd.rda")



#########################################################################################
# mean temp raster

CHL <- NULL
for (i in sort(unique(chl$Date))){
  ras <- rasterFromXYZ(chl[chl$Date == i,c("Longitude","Latitude","Chlor")], 
                       crs="+proj=longlat")
  ras.p <-  projectRaster(ras, 
                          crs=Proj, 
                          res=20000) # 20 x 20 km grid
  ras.pts <- data.frame(rasterToPoints(ras.p))
  ras.pts$Date <- i
  CHL <- rbind(CHL, ras.pts)
}


CHL$Year <- substr(CHL$Date, 1, 4)
CHL$Month <- substr(CHL$Date, 5, 6)
CHL$Month[CHL$Month == "00"] <- "Spring"
CHL$Month[CHL$Month == "99"] <- "Summer"


#save chl raster
save(CHL, file="Data/Chl.NPO_Raster.1998-2015.qd.rda")

#########################################################################################
#########################################################################################