require(ggplot2)
require(gridExtra)
require(plyr)
require(reshape2)
source("base.plot.R")



############################################################################
############################################################################

# load haul data
data.list <- list.files(path = "Data/biological", pattern = "haul*")
haul <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/biological",i),header=T)[-1]
  name <- sub(".csv", "", i)
  tmp$year  <- gsub("[a-z]+_", "",name)
  haul <- rbind(haul, tmp)
}

# load strata
strata <- read.csv("Data/Strata.csv",header=T)


# standardise longitude
haul$EQ_LONGITUDE <- abs(haul$EQ_LONGITUDE)

# group into strata
haul$lat_upper <- cut(haul$EQ_LATITUDE, breaks = strata$lat_upper, include.lowest = T)
haul$lat_upper <- as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", haul$lat_upper))

# check NA and change to 40 degree lat bin
summary(haul[is.na(haul$lat_upper),])
haul$lat_upper[is.na(haul$lat_upper)] <- 40

# number of hauls in each strata
table(haul$lat_upper)
table(haul$year)
table(haul$lat_upper, haul$year)

# merge haul and strata data
haul <- merge(haul, strata, by="lat_upper")

  
  
############################################################################
############################################################################

# load length by haul data
data.list <- list.files(path = "Data/biological", pattern = "aged_lengths*")
leng <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/biological",i),header=T)
  melted <- melt(tmp, id = "Length")
  name <- sub(".csv", "", i)
  melted$year  <- gsub("[a-z]+_", "",name)
  melted$variable  <- sub("X", "",melted$variable)
  leng <- rbind(leng, melted)
}
colnames(leng) <- c("LENGTH","HAUL","count", "year")


# group un-aged and aged lengths together
alen <- ddply(leng, .(LENGTH,HAUL,year), summarise,
              count = sum(count))

# merge strata and length data
# only haul data that matched length data -> only interested in hake tows
xylen <- merge(alen, haul, by = c("HAUL","year"), all.x = T)

# check for missing/non-matching haul data
xylen[is.na(xylen$strata),]

# sum length counts by strata
slen <- ddply(xylen, .(LENGTH,strata,year), summarise, count = sum(count))
summary(slen)

# save length by strata data
save(slen, file = "Length.Strata.rda")





############################################################################
############################################################################
# mean lengths in canadian and US strata

# convert length frequency counts back to raw lengths
rlen <- NULL
for (j in 1:nrow(slen)){
  if(slen$count[j] > 0){
    df <- data.frame(Length = rep(slen$LENGTH[j], slen$count[j]))
    df$strata <- slen$strata[j]
    df$year <- slen$year[j]
    rlen <- rbind(rlen, df)
  }
}

# N number of hake trawls (sample size) 
nhaul <- aggregate(HAUL ~ year + strata, function(x) length(unique(x)), data = xylen)

# add us and can labels
nhaul$grp <- "US"
nhaul$grp[nhaul$strata %in% c(5,6)] <- "Can"
rlen$grp <- "US"
rlen$grp[rlen$strata %in% c(5,6)] <- "Can"

# N number of hake trawls (sample size) for years and strata
nhaulyear <- aggregate(HAUL ~ year + grp, sum, data = nhaul)
nhaulstrata <- aggregate(HAUL ~ strata, sum, data = nhaul)


# mean length by year for hake in us and canadian waters
mleny <- do.call(data.frame, aggregate(Length ~ year + grp, data = rlen, 
                                       function(x) {c(m = mean(x), sd = sd(x))}))
mleny$n <- nhaulyear$HAUL
mleny$se <- mleny$Length.sd/sqrt(mleny$n)

# mean length by strata
mlens <- do.call(data.frame, aggregate(Length ~ strata, data = rlen, 
                                      function(x) {c(m = mean(x), sd = sd(x))}))
mlens$n <- nhaulstrata$HAUL
mlens$se <- mlens$Length.sd/sqrt(mlens$n)



# plot mean fork length by year
yplot <- basePlot +
  geom_point(data = mleny, aes(x = year, y = Length.m, colour = grp))+
  geom_line(data = mleny, aes(x = year, y = Length.m, colour = grp, group = grp))+
  geom_errorbar(data = mleny, 
                aes(x = year, ymin=Length.m - se, ymax=Length.m + se,  colour = grp), 
                width = .1)+
  labs(x = "Year", y = "Mean fork length (cm)")+
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "") + 
  scale_y_continuous(expand = c(0,0), limits = c(28,53))+
  theme(legend.position = c(.1,.1), legend.justification = c(.1,.1))
yplot



# plot mean fork length by year
splot <- basePlot +
  geom_point(data = mlens, aes(x = strata, y = Length.m))+
  geom_line(data = mlens, aes(x = strata, y = Length.m, group = 1))+
  geom_errorbar(data = mlens, 
                aes(x = strata, ymin=Length.m - se, ymax=Length.m + se), 
                width = .1)+
  labs(x = "Strata", y = "Mean fork length (cm)")+
  scale_y_continuous(expand = c(0,0), limits = c(28,53))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6))
splot


pdf("Figures/Morpho/ForkLength.byYear.byStrata.pdf", width=8, height=4) 
grid.arrange(yplot,splot, nrow = 1)
dev.off()






############################################################################
############################################################################
# hauls by strata by year

# only hake hauls
hakehaul <- xylen[!duplicated(interaction(xylen$year, xylen$HAUL)),]

# group haul depth and duration by strata
haul.strata <- ddply(hakehaul, .(strata, lat_upper, name), summarise, 
                  depth = mean(AVERAGE_FOOTROPE_DEPTH, na.rm=T),
                  depth.se = sd(AVERAGE_FOOTROPE_DEPTH, na.rm=T)/sqrt(length(strata)),
                  duration = mean(DURATION, na.rm=T),
                  duration.se = sd(DURATION, na.rm=T)/sqrt(length(strata)), 
                  N = length(strata)) 


# group haul depth and duration by year
haul.year <- ddply(hakehaul, .(year), summarise, 
                   depth = mean(AVERAGE_FOOTROPE_DEPTH, na.rm=T),
                   depth.se = sd(AVERAGE_FOOTROPE_DEPTH, na.rm=T)/sqrt(length(year)),
                   duration = mean(DURATION, na.rm=T),
                   duration.se = sd(DURATION, na.rm=T)/sqrt(length(year)), 
                   N = length(year)) 

# plot depth by year
depy <- basePlot +
  geom_point(data = haul.year, aes(x = year, y = depth))+
  geom_line(data = haul.year, aes(x = year, y = depth, group = 1))+
  geom_errorbar(data = haul.year, 
           aes(x = year, ymin=depth - depth.se, ymax=depth + depth.se), 
           width = .1)+
  labs(x = "Year", y = "Footrope depth (m)")+
  scale_y_continuous(expand = c(0,0), limits = c(80,330))
depy

# plot depth by strata
deps <- basePlot +
  geom_point(data = haul.strata, aes(x = strata, y = depth))+
  geom_line(data = haul.strata, aes(x = strata, y = depth, group = 1))+
  geom_errorbar(data = haul.strata, 
                aes(x = strata, ymin=depth - depth.se, ymax=depth + depth.se), 
                width = .1)+
  labs(x = "Strata", y = "Footrope depth (m)")+
  scale_y_continuous(expand = c(0,0), limits = c(80,330))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6))
deps


# plot duration by year
dury <- basePlot +
  geom_point(data = haul.year, aes(x = year, y = duration))+
  geom_line(data = haul.year, aes(x = year, y = duration, group = 1))+
  geom_errorbar(data = haul.year, 
                aes(x = year, ymin=duration-duration.se, ymax=duration+duration.se), 
                width = .1)+
  labs(x = "Year", y = "Haul duration")+
  scale_y_continuous(expand = c(0,0), limits = c(10,24))
dury

# plot duration by strata
durs <- basePlot +
  geom_point(data = haul.strata, aes(x = strata, y = duration))+
  geom_line(data = haul.strata, aes(x = strata, y = duration, group = 1))+
  geom_errorbar(data = haul.strata, 
                aes(x = strata, ymin=duration-duration.se, ymax=duration+duration.se), 
                width = .1)+
  labs(x = "Strata", y = "Haul duration")+
  scale_y_continuous(expand = c(0,0), limits = c(10,24))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6))
durs




###############
# Save as a pdf
pdf("Figures/Morpho/Depth.Duration.byYear.byStrata.pdf", width=7, height=5) 
grid.arrange(depy,deps,dury,durs, nrow = 2, widths = c(1.2,1))
dev.off()



