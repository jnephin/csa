
require(plyr)


# load new hake data
data.list <- list.files(path = "Data/biomass", pattern = "*.csv")
track <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/Biomass",i),header=T) 
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


#save 1998 to 2015 survey nasc data
save(track,file="Data/Track.1998-2015.rda")


