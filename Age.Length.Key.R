require(ggplot2)
require(plyr)
source("base.plot.R")



############################################################################
############################################################################

# load specimen age and length data
data.list <- list.files(path = "Data/biological", pattern = "specimen*")
spec <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/biological",i),header=T)
  tmp <- tmp[,c("HAUL","SPECIES_CODE","SEX","LENGTH","WEIGHT","AGE")]
  name <- sub(".csv", "", i)
  id <- strsplit(name, "_")[[1]]
  tmp$year <- id[2]
  tmp$loc <- id[3]
  spec <- rbind(spec, tmp)
}

# remove all non hake species (hake species code: 22500)
spec <- spec[spec$SPECIES_CODE == 22500,]

# remove NA age's
spec <- spec[!is.na(spec$AGE),]

# round lengths to the nearest centimeter
spec$LENGTH <- round(spec$LENGTH)




############################################################################
############################################################################
# mean length per age by year

# calculate median age and mean weight for each length by year
lpa <- ddply(spec, .(AGE, year), summarise, 
                 LENGTH = mean(LENGTH, na.rm=T),
                 WEIGHT = mean(WEIGHT, na.rm=T), 
                 N = length(AGE)) 


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")

# plot
laplot <- basePlot +
  geom_point(data = lpa, 
              aes(x = AGE, y=LENGTH, colour = year), 
              pch=16, size = .5)+
  stat_smooth(data = lpa, method = "loess", formula = y ~ log(x),
              aes(x= AGE, y=LENGTH, colour = year), 
              show.legend = FALSE, size=.3, se = FALSE)+
  labs(x = "Age", y = "Mean Length (cm)")+
  scale_x_continuous(expand = c(0,0), limits = c(0.5,23))+
  scale_y_continuous(expand = c(0,0), limits = c(18,65))+
  scale_colour_manual(values = pal, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right",
                                     override.aes = list(size = 2)))+
  theme(legend.justification = c(0,1.04), legend.position = c(0,1.04))
laplot

###############
# Save as a pdf
pdf("Figures/Morpho/MeanLength.byAge.Curves.1998-2015.pdf", width=6, height=4) 
laplot
dev.off()




############################################################################
############################################################################
# mean length per age by country

## ----- needs to be updated
## -----  get lat lon data from haul.csv to seperate can and us hauls


# calculate median age and mean weight for each length by year
lpaloc <- ddply(spec, .(AGE, loc), summarise, 
             LENGTH = mean(LENGTH, na.rm=T),
             WEIGHT = mean(WEIGHT, na.rm=T), 
             N = length(AGE)) 


#colours
cols <- c("#56B4E9", "#F0E442")

# plot
canusplot <- basePlot +
  geom_point(data = lpaloc, 
             aes(x = AGE, y=LENGTH, colour = loc), 
             pch=16, size = .5)+
  stat_smooth(data = lpaloc, method = "loess", formula = y ~ log(x),
              aes(x= AGE, y=LENGTH, colour = loc), 
              show.legend = FALSE, size=.3, se = FALSE)+
  labs(x = "Age", y = "Mean Length (cm)")+
  scale_x_continuous(expand = c(0,0), limits = c(0.5,23))+
  scale_y_continuous(expand = c(0,0), limits = c(18,65))+
  scale_colour_manual(values = cols, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right",
                                     override.aes = list(size = 2)))+
  theme(legend.justification = c(0,1.04), legend.position = c(0,1.04))
canusplot

###############
# Save as a pdf
pdf("Figures/Morpho/MeanLength.byAge.Curves.Can-US.pdf", width=6, height=4) 
canusplot
dev.off()



############################################################################
############################################################################
# size index 
# mean length at age 10
lpa10 <- ddply(spec[spec$AGE == 10,], .(grp, year), summarise, 
             meanLENGTH = mean(LENGTH, na.rm=T),
             sdLENGTH = sd(LENGTH, na.rm=T),
             N = length(AGE)) 

# save length at age 10 size index
save(lpa10,file="Data/Length.Age10.rda")






############################################################################
############################################################################
# age length key


# Frequency tables
alk <- table(spec$LENGTH, spec$AGE, spec$year)
for (i in unique(spec$year)){
  alk[,,i] <- prop.table(alk[,,i], 1)
}
alk[is.na(alk)] <- 0


#save age length key
save(alk,file="Data/Age.Length.Key.rda")


