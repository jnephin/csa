require(ggplot2)
require(gridExtra)
require(plyr)
require(sp)
require(rgdal)
source("base.plot.R")



##################################
#--------     TRACK     ---------#
##################################

# Load track data
load("Data/Track.1998-2015.rda") #track




##########################################
##            ZERO FREQUENCY            ##
##########################################


# How many zero data points in each year?
df <- NULL
for (i in unique(track$year)){
d <- head(table(track$NASC[track$year == i]),1)
tl <- table(track$year[track$year == i])[1]
dat <- data.frame(zeros=d, total=tl, year=i)
df <- rbind(df,dat)
}
df$ratio <- df$zeros/df$total*100
df

zeroPlot <- basePlot +
  geom_point(data = df, aes(x = year, y=zeros), 
               size = 2)+
  labs(x = "", y = "Number of zeros")
zeroPlot

ratioPlot <- basePlot +
  geom_point(data = df, aes(x = year, y=ratio), 
             size = 2)+
  labs(x = "", y = "Percent of zeros")
ratioPlot

pdf("Figures/Biomass/Zeros.1998-2015.pdf", width = 8, height = 4)
grid.arrange(zeroPlot, ratioPlot, ncol=2)
dev.off()



##########################################
##          BIOMASS FREQUENCY           ##
##########################################

# remove zeros
ptrack <- track[track$Biomass.density >0,]


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


FreqPlot <- basePlot +
  geom_density(data = ptrack, aes(x = log(Biomass.density), y=..count.., fill = year), 
               alpha = 0)+
  geom_density(data = ptrack, aes(x = log(Biomass.density), y=..count.., colour = year))+
  scale_fill_manual(values = pal, name = "Year", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .1,
                                     label.position = "top", 
                                    override.aes = list(size =.5, alpha = 1, colour = NA)))+
  scale_colour_manual(values = pal, guide = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,1150)) +
  scale_x_continuous(expand = c(0,0), limits = c(1,17)) +
  labs(x = expression(paste("Log(Biomass) kg/",nmi^2)), y = "Frequency")+
  theme(legend.justification = c(1,1.04), legend.position = c(1,1.04))
FreqPlot


DensityPlot <- basePlot +
  geom_density(data = ptrack, aes(x = log(Biomass.density), ..density.., fill = year), 
               alpha = 0)+
  geom_density(data = ptrack, aes(x = log(Biomass.density), ..density.., colour = year))+
  scale_fill_manual(values = pal, name = "Year", guide = "none")+
  scale_colour_manual(values = pal, guide = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,.37)) +
  scale_x_continuous(expand = c(0,0), limits = c(1,17)) +
  labs(x = expression(paste("Log(Biomass) kg/",nmi^2)), y = "Density")+
  theme(legend.justification = c(1,1.04), legend.position = c(1,1.04))
DensityPlot


###############
# Save as a pdf
pdf("Figures/Biomass/Biomass.Histogram.Curves.1998-2015.pdf", width=8, height=4) 
grid.arrange(FreqPlot, DensityPlot, ncol=2)
dev.off()



##############################################
##             MEAN BIOMASS BINS            ##
##############################################

# removed zeros 
pmean <- ddply(ptrack, .(year), summarise,
              mean = mean(Biomass.density, na.rm=T), sd = sd(Biomass.density, na.rm=T))
# with zeros 
zmean <- ddply(track, .(year), summarise,
               mean = mean(Biomass.density, na.rm=T), sd = sd(Biomass.density, na.rm=T))


MeanPlot <- basePlot +
  geom_bar(data = pmean, aes(x = factor(year), y=mean),
           stat = "identity", width = .8)+
  scale_fill_manual(values = pal, name = "Year", guide = 
                      guide_legend(direction = "vertical", 
                                   keywidth = .5,  keyheight = .1,
                                   label.position = "top", 
                                   override.aes = list(size =.5)))+
  coord_cartesian(ylim =  c(0,335000))+
  scale_y_continuous(expand = c(0,0)) +
  labs(y = expression(paste("Mean Biomass kg/",nmi^2," zeros removed")), x = "")
MeanPlot

zMeanPlot <- basePlot +
  geom_bar(data = zmean, aes(x = factor(year), y=mean),
           stat = "identity", width = .8)+
  scale_fill_manual(values = pal, name = "Year", guide = 
                      guide_legend(direction = "vertical", 
                                   keywidth = .5,  keyheight = .1,
                                   label.position = "top", 
                                   override.aes = list(size =.5)))+
  coord_cartesian(ylim =  c(0,48500))+
  scale_y_continuous(expand = c(0,0)) +
  labs(y = expression(paste("Mean Biomass kg/",nmi^2," including zeros")), x = "")
zMeanPlot

###############
# Save as a pdf
pdf("Figures/Biomass/Biomass.Mean.Bar.1998-2015.pdf", width=9, height=4) 
grid.arrange(zMeanPlot, MeanPlot, ncol=2)
dev.off()



