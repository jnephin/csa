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
              pch=16, size = .7)+
  geom_line(data = lpa, 
             aes(x = AGE, y=LENGTH, colour = year), 
             size = .3)+
  labs(x = "Age", y = "Mean fork length (cm)")+
  scale_x_continuous(expand = c(0,0), limits = c(0.5,23))+
  scale_y_continuous(expand = c(0,0), limits = c(18,66))+
  scale_colour_manual(values = pal, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right"))+
  theme(legend.justification = c(0,1.1), legend.position = c(0,1.1))
laplot

###############
# Save as a pdf
pdf("Figures/Morpho/MeanLength.byAge.Curves.1998-2015.pdf", width=6, height=4) 
laplot
dev.off()




############################################################################
############################################################################
# mean length per age by country

# load xy haul data
load("Data/Haul.Strata.rda") #haul

# merge spec and haul
xyspec <- merge(spec, haul, by = c("HAUL","year"))


# calculate median age and mean weight for each length by year by strata
lpaloc <- ddply(xyspec, .(AGE, strata,name), summarise, 
             LENGTH = mean(LENGTH, na.rm=T),
             WEIGHT = mean(WEIGHT, na.rm=T), 
             N = length(AGE)) 
lpaloc$name <- factor(lpaloc$name, levels =c("Monterey","Eureka","South Columbia",
                                             "North Columbia","Vancouver","Haida"))

#colours
cbPalette <- c("#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2",  "#CC79A7")

# plot
canusplot <- basePlot +
  geom_point(data = lpaloc, 
             aes(x = AGE, y=LENGTH, colour = factor(name)), 
             pch=16, size = .7)+
  geom_line(data = lpaloc, 
             aes(x = AGE, y=LENGTH, colour = factor(name)), 
             size = .3)+
  labs(x = "Age", y = "Mean fork length (cm)")+
  scale_x_continuous(expand = c(0,0), limits = c(0.5,23))+
  scale_y_continuous(expand = c(0,0), limits = c(18,65))+
  scale_colour_manual(values = cbPalette, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right"))+
  theme(legend.justification = c(0,1.04), legend.position = c(0,1.04))
canusplot

###############
# Save as a pdf
pdf("Figures/Morpho/MeanLength.byAge.Curves.Strata.pdf", width=6, height=4) 
canusplot
dev.off()



############################################################################
############################################################################
# size index 

# mean length at age 10 to 13
mla <- ddply(spec[spec$AGE %in% c(10,11,12,13),], .(year), summarise, 
             meanLENGTH = mean(LENGTH, na.rm=T),
             sdLENGTH = sd(LENGTH, na.rm=T),
             N = length(year)) # number of fish

# save length at age 10 to 13 size index
save(mla,file="Data/Adult.Mean.Length.rda")




############################################################################
############################################################################
# age length key


# Frequency table
alk_table <- table(spec$LENGTH, spec$AGE)
alk <- as.data.frame.matrix(prop.table(alk_table, 1))

#save age length key
save(alk,file="Data/Age.Length.Key.rda")


