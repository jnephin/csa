require(ggplot2)
require(plyr)
source("base.plot.R")

#load proportion of ages by strata
load("Data/Age.Strata.Prop.rda") #pas

#load 1998 to 2015 survey nasc data
load("Data/Track.1998-2015.rda") #track



############################################################################
############################################################################
# stratify track data


# group into strata
track$lat_upper <- cut(track$Lat, breaks = unique(pas$lat_upper), include.lowest = T)
track$lat_upper <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", track$lat_upper))

# check NA and change to 40 degree lat bin
summary(track[is.na(track$lat_upper),])
track$lat_upper[track$Lat > 58] <- 58
track$lat_upper[is.na(track$lat_upper)] <- 40

# number of samples in each strata per year
table(track$lat_upper, track$year)

# merge track and age data
atrack <- merge(track, pas, by = "lat_upper")


############################################################################
############################################################################
# density

# number of fish per age per strata
atrack$Age.density <- atrack$Number.density * atrack$percent

# group by age by strata by year
asd <- ddply(atrack, .(age, strata, name, lat_upper, year), summarise,
             density = sum(Age.density))


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")

# plot density by age by strata
asplot <- basePlot +
  geom_point(data = asd, aes(x = year, y = age, size = density), 
             pch = 1, show.legend = FALSE)+
  facet_wrap(~name, dir = "v")+
  labs(x = "Year", y = "Age")+
  scale_size_area(max_size= 8)+
  theme(legend.position = "right")
asplot


###############
# Save as a pdf
pdf("Figures/Morpho/AgedDensity.Bubbles.Strata.1998-2015.pdf", width=7, height=7) 
asplot
dev.off()



############################################################################
############################################################################
# proportion of hake density in canada

# us and canada groups
asd$grp <- "US"
asd$grp[asd$strata %in% c(5,6)] <- "Canada"

# group by age by country by year
agd <- ddply(asd, .(age, grp, year), summarise,
             density = sum(density))

# total density for each age each year
agd <- ddply(agd, .(age, year), transform, total = sum(density))

agd$prop <- agd$density / agd$total

# prop in canada
propcan <- agd[agd$grp == "Canada",][-2]


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


# plot density in canada by age
propplot <- basePlot +
  geom_line(data = propcan, aes(x = age, y = prop, colour = year))+
  labs(x = "Age", y = "Proportion in Canada")+
  scale_x_continuous(expand = c(0,0), breaks = seq(2,20,2), limits = c(1,22))+
  scale_y_continuous(expand = c(0,0), limits = c(-.01,.95))+
  scale_colour_manual(values = pal, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right"))+
  theme(legend.justification = c(0,1.04), legend.position = c(0,1.04))
propplot


###############
# Save as a pdf
pdf("Figures/Morpho/PropCan.Aged.Curves.1998-2015.pdf", width=6, height=3.5) 
propplot
dev.off()




############################################################################
############################################################################
# age-adjusted migration index

# prop in can by age curves asymptote ~ age 12
# proportion in canada at age 12

amig <- propcan[propcan$age == 12,c("year","prop")]

# save
save(amig, file = "Data/Aged.Migration.Index.rda")







