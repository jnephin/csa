require(ggplot2)
require(plyr)
require(gridExtra)
source("base.plot.R")



############################################################################
############################################################################

# load specimen data
load("Data/Specimen.rda") #spec

# load haul data
load("Data/Haul.Strata.rda") #haul


############################################################################
############################################################################
# mean length per age by year
# mean length per age by strata


# calculate median age and mean weight for each length by year
lpa <- ddply(spec, .(AGE, year), summarise, 
                 LENGTH = mean(LENGTH, na.rm=T),
                 WEIGHT = mean(WEIGHT, na.rm=T), 
                 N = length(AGE)) 


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

#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")




# plot year
yplot <- basePlot +
  geom_line(data = lpa, 
             aes(x = AGE, y=LENGTH, colour = year), 
             size = .5)+
  labs(x = "Age", y = "")+
  scale_x_continuous(expand = c(0,0), limits = c(0.5,23))+
  scale_y_continuous(expand = c(0,0), limits = c(18,66))+
  scale_colour_manual(values = pal, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right"))+
  theme(legend.justification = c(1,0), legend.position = c(1,0),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),plot.margin = unit(c(.5,.5,.5,.1), "lines"))
yplot


# plot strata
splot <- basePlot +
  geom_line(data = lpaloc, 
             aes(x = AGE, y=LENGTH, colour = factor(name)), 
             size = .5)+
  labs(x = "Age", y = "Mean fork length (cm)")+
  scale_x_continuous(expand = c(0,0), limits = c(0.5,23))+
  scale_y_continuous(expand = c(0,0), limits = c(18,66))+
  scale_colour_manual(values = cbPalette, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .7,
                                     label.position = "right"))+
  theme(legend.justification = c(1,0), legend.position = c(1,0),
        plot.margin = unit(c(.5,.1,.5,.1), "lines"))
splot


###############
# Save as a pdf
pdf("Figures/Hauls/MeanLength.byAge.Curves.pdf", width=7, height=3.5) 
grid.arrange(splot,yplot, ncol=2)
dev.off()





############################################################################
############################################################################
# age length key


# Frequency table
alk_table <- table(spec$LENGTH, spec$AGE)
alk <- as.data.frame.matrix(prop.table(alk_table, 1))

#save age length key
save(alk,file="Data/Age.Length.Key.rda")


# Frequency table age 2+ and >= 28 cm
alk_table <- table(spec$LENGTH, spec$AGE)
alk_table2 <- alk_table[,-c(1,2)] # no age 1 or 2
alk_table2 <- alk_table2[17:67,] # >= 28 cm
alk2 <- as.data.frame.matrix(prop.table(alk_table2, 1))

#save age length key for ages 2+
save(alk2,file="Data/Age2.Length.Key.rda")





