
require(plyr)

##############################################################################
# survey 


# load new hake data
data.list <- list.files(path = "Data/biomass/survey", pattern = "*.csv")
track <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/biomass/survey",i),header=T) 
  tmp$year <- sub(".csv", "", i)
  track <- rbind(track, tmp)
}


# how many duplicate lat/long exist each year?
dup <- duplicated(track[c("Lat", "Lon", "year")]) | 
       duplicated(track[c("Lat", "Lon", "year")], fromLast=TRUE)
dups <- track[dup,]; dim(dups)



# caluclate mean of duplicates
track <- ddply(track, .(Lon, Lat, year), summarise, 
               NASC = mean(NASC),
               Number.density = mean(Number.density), 
               Biomass.density = mean(Biomass.density)) 


#save 1998 to 2015 survey data
save(track,file="Data/Track.1998-2015.rda")




##############################################################################
# kriged 


# load new hake data
data.list <- list.files(path = "Data/biomass/kriged", pattern = "*.csv")
krig <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/biomass/kriged",i),header=T) 
  tmp$year <- sub(".csv", "", i)
  krig <- rbind(krig, tmp)
}


# how many duplicate lat/long exist each year?
dup <- duplicated(krig[c("Lat", "Lon", "year")]) | 
  duplicated(krig[c("Lat", "Lon", "year")], fromLast=TRUE)
dups <- krig[dup,]; dim(dups)



# columns of interest
krig <- krig[c("Lat","Lon","year", "NASC","ntk_total","wgt_total")]
colnames(krig)[5:6] <-  c("Number.density", "Biomass.density")

#save 1998 to 2015 kriged data
save(krig,file="Data/Krig.1998-2015.rda")


