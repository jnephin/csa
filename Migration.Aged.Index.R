require(ggplot2)
require(plyr)
require(reshape2)
source("base.plot.R")

#load age length key
load("Data/Age2.Length.Key.rda") #alk2

#load length by strata data
load("Data/Length.Strata.rda") #mlen

#load 1998 to 2015 survey data
load("Data/Track.1998-2015.rda") #track

#load biomass indices 
load("Data/LatIndices.rda") #indices$bCan


############################################################################
############################################################################
# apply age length key to morpho data


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



############################################################################
############################################################################
# calculate the proportion of ages in each strata each year

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



############################################################################
############################################################################
# stratify track data


# group into strata
track$lat_upper <- cut(track$Lat, breaks = unique(persy$lat_upper), include.lowest = T)
track$lat_upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", track$lat_upper))

# check NA and change to 40 degree lat bin
summary(track[is.na(track$lat_upper),])
track$lat_upper[track$Lat > 58] <- 58
track$lat_upper[is.na(track$lat_upper)] <- 40

# number of samples in each strata per year
table(track$lat_upper, track$year)

# group track by strata by year
strack <- ddply(track, .(lat_upper, year), summarise,
             density = sum(Number.density),
             biomass = sum(Biomass.density))

# merge summed track and age data
atrack <- merge(strack, persy, by = c("lat_upper","year"))



############################################################################
############################################################################
# biomass

# number of fish per age per strata
atrack$Age.biomass <- atrack$biomass * atrack$percent


# plot biomass by age by strata
asplot <- basePlot +
  geom_point(data = atrack, aes(x = year, y = age, size = Age.biomass), 
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




############################################################################
############################################################################
# proportion of hake density in canada

# us and canada groups
atrack$grp <- "US"
atrack$grp[atrack$strata %in% c(5,6)] <- "Canada"

# group by age by country by year
agy <- ddply(atrack, .(age, grp, year), summarise,
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




############################################################################
############################################################################
# age-adjusted migration index

# prop biomass in can by age 5 and age 10
mig5 <- propcan[propcan$age == 5,c("year","prop")]
mig10 <- propcan[propcan$age == 10,c("year","prop")]

#anomaly
mig5$anom <- mig5$prop - mean(mig5$prop)
mig10$anom <- mig10$prop - mean(mig10$prop)

# save
save(mig5, file = "Data/AgedMigration.5.Index.rda")
save(mig10, file = "Data/AgedMigration.10.Index.rda")


##################################################
# plot

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


# Save as a pdf
pdf("Figures/Indices/AgedMigrationIndex_anomaly.pdf", width=7, height=3) 
amigplot
dev.off()


