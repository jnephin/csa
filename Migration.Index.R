require(plyr)
library(rgdal)
require(sp)
require(ggplot2)
require(reshape2)
source("base.plot.R")




############################################################################
############################################################################

# ----------------------------   Migration  -------------------------------#

############################################################################
############################################################################



load("Data/Krig.1998-2015.rda") #krig


# sum NASC latitude (at 0.01 resolution, roughly 1 km in distance)
krig$Latitude <- round(krig$Lat,2)
sum.krig <- ddply(krig, .(Latitude, year), transform, 
                   Biomass.density = sum(Biomass.density))
sum.krig <- ddply(sum.krig, .(year), function(x) {x[!duplicated(x$Latitude),]})

# sum the NASC values cummulatively by year
ord.krig <- sum.krig[order(sum.krig$Latitude),]
cumkrig <- ddply(ord.krig, "year", transform, cumBiomass = cumsum(Biomass.density))

#percent NASC
cumkrig <- ddply(cumkrig, "year", transform, maxBiomass = tail(cumBiomass,1))
cumkrig$cdf <-  cumkrig$cumBiomass/cumkrig$maxBiomass



##################################
#-------- SPATIAL CLIP  ---------#
##################################

# load canadian eez shapefile
eez.poly <- readOGR(dsn="Data/Shapefiles", layer="EEZ")
eez <- fortify(eez.poly)

# transform into spatial dataframe and project
coordinates(cumkrig) <- ~ Lon + Lat
proj4string(cumkrig) <- CRS("+proj=longlat")
proj.cumkrig <- spTransform(cumkrig, proj4string(eez.poly))

# clip
can.cumkrig <- proj.cumkrig[eez.poly,]
ind <- as.numeric(row.names(can.cumkrig))


# reproject and convert back to data.frame
cumkrig <- spTransform(proj.cumkrig, CRS("+proj=longlat"))
cumkrig <- as.data.frame(cumkrig)

# add US / can labels
cumkrig$grp <- "us"
cumkrig$grp[ind] <- "can"



##################################
#----------- INDICES ------------#
##################################

#plot settings
par(mar=c(3,3,2.5,1), mfrow = c(2,2))

#### Index 1) 
#### Percentage of Hake pop in Canadian waters - krig no extrapolatino INPFC
pcan <- ddply(cumkrig, .(year,grp), summarise, 
              percent = (sum(Biomass.density)/min(maxBiomass))*100)
pcan <- pcan[pcan$grp == "can",-2]
plot(y = pcan$percent, x = factor(pcan$year), main = "1) Percentage of Hake in Canadian Waters", cex.main =.8, cex.axis=.6)

#### Index 2) 
#### Latitude of the 50th percentile of Biomass
p50 <- ddply(cumkrig, .(year), function(x) {x[which(abs(x$cdf-.5) == min(abs(x$cdf-.5))),]})
p50 <- aggregate(Latitude ~ cdf + year, FUN = mean, data = p50)
plot(y = p50$Latitude, x = factor(p50$year), main = "2) Latitude where 50th percentile of biomass is reached", cex.main =.8, cex.axis=.6)

#### Index 3) 
#### Latitude of the 75th percentile of Biomass
p75 <- ddply(cumkrig, .(year), function(x) {x[which(abs(x$cdf-.75) == min(abs(x$cdf-.75))),]})
p75 <- aggregate(Latitude ~ cdf + year, FUN = mean, data = p75)
plot(y = p75$Latitude, x = factor(p75$year), main = "3) Latitude where 75th percentile of biomass is reached",  cex.main =.8, cex.axis=.6)

#### Index 4) 
#### Biomass in canadian water - krig no extrapolatino INPFC
bcan <- aggregate(Biomass.density ~ grp + year, sum, data = cumkrig)
bcan <- bcan[bcan$grp == "can",-1]
bcan$krig <- bcan$Biomass.density/1000/1000
bcan <- bcan[,-2]
plot(y = bcan$krig, x = factor(bcan$year), main = "4) Biomass (kmt)  in Canadian waters",  cex.main =.8, cex.axis=.6)


##################################
#------- MIGRATION INDEX --------#
##################################

#anomaly
pcan$anom <- pcan$percent - mean(pcan$percent)
bcan$anom <- bcan$krig - mean(bcan$krig)

# save
save(pcan, file = "Data/Migration.percent.Index.rda")
save(bcan, file = "Data/Migration.total.Index.rda")




############################################################################
############################################################################

# --------------------------   Aged Migration  ----------------------------#

############################################################################
############################################################################


#load age length key
load("Data/Age2.Length.Key.rda") #alk2

#load length by strata data
load("Data/Length.Strata.rda") #mlen

#load 1998 to 2015 survey data
load("Data/Krig.1998-2015.rda") #krig



###################################
#-------- AGE LENGTH KEY ---------#
###################################

# age length key missing length 78
# get average for 77 and 79
add78 <- t(as.data.frame(apply(alk2[49:50,], 2, mean)))
row.names(add78) <- 78
alk2 <- rbind(alk2, add78)

# merge
aged <- merge(mlen, alk2,  by.x = "LENGTH", by.y = "row.names")

# counts per age class
mult <- aged$count * aged[,5:24]
aged_counts <- cbind(aged[,2:3], mult)



#######################################
#-------- PROPORTION OF AGES ---------#
#######################################

# sum aged counts by strata by year
agesy <- ddply(aged_counts, .(strata,year), numcolwise(sum))

# melt
meltsy <- melt(agesy, id = c("strata","year"))
meltsy$age <- as.numeric(as.character(meltsy$variable))

# proportion of each age class by strata by year
persy <- ddply(meltsy, .(strata,year), transform, 
               percent = value/sum(value)*100)


# load strata and merge for strata names
strata <- read.csv("Data/Strata.csv",header=T)
persy <- merge(persy, strata, by = "strata")
persy$name <- factor(persy$name, 
                     levels =c("Monterey","Eureka","South Columbia",
                               "North Columbia","Vancouver","Haida"))


#######################################
#-------- PLACE INTO STRATA ----------#
#######################################

# group into strata
krig$lat_upper <- cut(krig$Lat, breaks = unique(persy$lat_upper), include.lowest = T)
krig$lat_upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", krig$lat_upper))

# check NA and change to 40 degree lat bin
summary(krig[is.na(krig$lat_upper),])
krig$lat_upper[krig$Lat > 58] <- 58
krig$lat_upper[is.na(krig$lat_upper)] <- 40

# number of samples in each strata per year
table(krig$lat_upper, krig$year)

# group track by strata by year
skrig <- ddply(krig, .(lat_upper, year), summarise,
               density = sum(Number.density),
               biomass = sum(Biomass.density))

# merge summed track and age data
akrig <- merge(skrig, persy, by = c("lat_upper","year"))



#########################################
#---------- AGE BY STRATA PLOT ---------#
#########################################

# number of fish per age per strata
akrig$Age.biomass <- akrig$biomass * akrig$percent


# plot biomass by age by strata
asplot <- basePlot +
  geom_point(data = akrig, aes(x = year, y = age, size = Age.biomass), 
             pch = 1, show.legend = FALSE)+
  facet_wrap(~name, dir = "v")+
  labs(x = "Year", y = "Age")+
  scale_size_area(max_size= 8)+
  theme(legend.position = "right")
asplot


###############
# Save as a pdf
pdf("Figures/Ages/Biomass.Bubbles.Age.Strata.Year.pdf", width=7, height=7) 
asplot
dev.off()



#########################################
#-------- PROPORTION IN CANADA ---------#
#########################################

# us and canada groups
akrig$grp <- "US"
akrig$grp[akrig$strata %in% c(5,6)] <- "Canada"

# group by age by country by year
agy <- ddply(akrig, .(age, grp, year), summarise,
             Age.biomass = sum(Age.biomass))

# total biomass for each age each year
agy <- ddply(agy, .(age, year), transform, total = sum(Age.biomass))

agy$prop <- (agy$Age.biomass / agy$total) *100

# prop in canada
propcan <- agy[agy$grp == "Canada",][-2]


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


# plot biomass in canada by age
propplot <- basePlot +
  geom_line(data = propcan, aes(x = age, y = prop, colour = year))+
  labs(x = "Age", y = "Percent of Hake biomass in Canada")+
  scale_x_continuous(expand = c(0,0), breaks = seq(2,20,2), limits = c(2,22))+
  scale_y_continuous(expand = c(0,0), limits = c(-.01,95))+
  scale_colour_manual(values = pal, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right"))+
  theme(legend.justification = c(-0.02,1.25), legend.position = c(-0.02,1.25))
propplot


###############
# Save as a pdf
pdf("Figures/Ages/AgedPropCan.Biomass.Curves.pdf", width=6, height=3.5) 
propplot
dev.off()



#######################################
#-------- AGED-MIGRATION INDEX -------#
#######################################

# prop biomass in can by age 5 and age 10
mig5 <- propcan[propcan$age == 5,c("year","prop")]
mig10 <- propcan[propcan$age == 10,c("year","prop")]

#anomaly
mig5$anom <- mig5$prop - mean(mig5$prop)
mig10$anom <- mig10$prop - mean(mig10$prop)

# save
save(mig5, file = "Data/AgedMigration.5.Index.rda")
save(mig10, file = "Data/AgedMigration.10.Index.rda")







############################################################################
############################################################################

# --------------------------    Anomaly Plot   ----------------------------#

############################################################################
############################################################################


# combine all age indices
mig <- rbind(mig5,mig10)
mig$age <- c(rep("age 5",nrow(mig5)),rep("age 10",nrow(mig10)))
mig$age <- factor(mig$age, levels = c("age 5","age 10"))

# positive / negative label
mig$sign <- "p"
mig$sign[mig$anom <= 0] <- "n"


# strip labels
labstrp <- data.frame(age = c("age 5","age 10"),x = rep("2013",2), y = rep(30,2))
labstrp$age <- factor(labstrp$age, levels = c("age 5","age 10"))

# plot age anomaly indices
amigplot <- basePlot +
  facet_wrap(~age, nrow = 1)+
  geom_bar(data = mig, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  geom_text(data = labstrp, aes(x = factor(x), y =  y, label = age), 
            size = 3, hjust = 0)+
  labs(x = "", y = "Percent of hake in Canada anomaly")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"))
amigplot

##########################################################################

# group labels
pcan$grp <- "Proportion (%)"
bcan$grp <- "Total (kmt)"
mig5$grp <- "Proportion at age 5 (%)"

#rbind
ind <- rbind(pcan[,-2],bcan[,-2],mig5[,-2])

# positive / negative label
ind$sign <- "p"
ind$sign[ind$anom <= 0] <- "n"

# strip labels and plot centering
labstrp <- data.frame(grp = c("Total (kmt)","Proportion (%)",
                              "Proportion at age 5 (%)"),
                      x = rep("2009",3), 
                      y = c(318,33,33),
                      ymin = c(-355*.8,-37*.8,-37*.8),
                      ymax = c(355,37,37))


# plot anomaly bio in can
migplot <- basePlot +
  facet_wrap(~grp, nrow = 1, scales = "free")+
  geom_linerange(data = labstrp, aes(x = factor(x), ymin =  ymin, ymax = ymax), 
               colour = "white")+
  geom_bar(data = ind, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  geom_text(data = labstrp, aes(x = factor(x), y =  y, label = grp), 
            size = 2.7, hjust = 0.5)+
  labs(x = "", y = "Hake biomass in Canada anomaly")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  scale_y_continuous(expand = c(0,0))+
  theme(strip.text = element_blank(),
        axis.text.x = element_text(angle=90,vjust=.5),
        panel.margin = unit(.1, "lines"),
        plot.margin = unit(c(.5,.5,.2,.5), "lines"))
migplot

##########################################################################

# Save as a pdf
pdf("Figures/Indices/MigrationIndex_anomaly.pdf", width=7.5, height=3) 
migplot
dev.off()



