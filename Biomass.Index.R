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

#stock biomass kmt
stock <- data.frame(year = c(1998,2001,2003,2005,2007,2009,2011,2012,2013,2015),
                    krig = c(1227.35, 795.825, 1882.773, 1020.52, 825.38, 
                             1301.31, 596.28,1220.65, 1789.72, 2003.22))

# anomaly
stock$anom <- stock$krig - mean(stock$krig)

# save stock biomass index 
save(stock,file="Data/StockBiomass.Index.rda")


# positive / negative label
stock$sign <- "p"
stock$sign[stock$anom <= 0] <- "n"

# plot biomass anomaly indices
anomplot <- basePlot +
  geom_bar(data = stock, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  labs(x = "", y = "Stock biomass (kmt) anomaly")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))
anomplot


# Save as a pdf
pdf("Figures/Indices/StockBiomassIndex_anomaly.pdf", width=4, height=3) 
anomplot
dev.off()


# plot stock biomass 
stockplot <- basePlot +
  geom_bar(data = stock, aes(x = factor(year), y = krig),
           width = .8, show.legend = FALSE, stat = "identity")+
  labs(x = "", y = "Stock biomass (kmt)")+
  scale_y_continuous(expand = c(0,0), limits = c(0,2050))
stockplot

# Save as a pdf
pdf("Figures/Biomass/StockBiomass.Bar.pdf", width=4, height=3) 
stockplot
dev.off()



##########################################
##          ZEROS - PATCHINESS          ##
##########################################


# How many zero data points in each year?
df <- NULL
for (i in unique(track$year)){
d <- head(table(track$NASC[track$year == i]),1)
tl <- table(track$year[track$year == i])[1]
dat <- data.frame(zeros=d, total=tl, year=i)
df <- rbind(df,dat)
}
df$ratio <- (df$zeros/df$total) * 100
df

# anomaly
df$anom <- df$ratio - mean(df$ratio)

# save patchiness index 
patch <- df[,3:5]
save(patch,file="Data/Patchiness.Index.rda")


# positive / negative label
patch$sign <- "p"
patch$sign[patch$anom <= 0] <- "n"

# plot patch anomaly indices
patchplot <- basePlot +
  geom_bar(data = patch, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  labs(x = "", y = "Percent of survey area without hake anomaly")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))
patchplot


# Save as a pdf
pdf("Figures/Indices/PatchinessIndex_anomaly.pdf", width=4, height=3) 
patchplot
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
  scale_fill_manual(values = pal, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .5,  keyheight = .1,
                                     label.position = "bottom", 
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
  scale_fill_manual(values = pal, name = "", guide = "none")+
  scale_colour_manual(values = pal, guide = "none")+
  scale_y_continuous(expand = c(0,0), limits = c(0,.37)) +
  scale_x_continuous(expand = c(0,0), limits = c(1,17)) +
  labs(x = expression(paste("Log(Biomass) kg/",nmi^2)), y = "Density")+
  theme(legend.justification = c(1,1.04), legend.position = c(1,1.04))
DensityPlot


###############
# Save as a pdf
pdf("Figures/Biomass/Biomass.Histogram.Curves.pdf", width=8, height=4) 
grid.arrange(FreqPlot, DensityPlot, ncol=2)
dev.off()



##############################################
##             MEAN BIOMASS BINS            ##
##############################################

# removed zeros 
pmean <- ddply(ptrack, .(year), summarise, mean = mean(Biomass.density, na.rm=T)/1000)
pmean$anom <- pmean$mean - mean(pmean$mean) #units mt

# with zeros 
zmean <- ddply(track, .(year), summarise, mean = mean(Biomass.density, na.rm=T)/1000)


MeanPlot <- basePlot +
  geom_bar(data = pmean, aes(x = factor(year), y=mean),
           stat = "identity", width = .8)+
  coord_cartesian(ylim =  c(0,335))+
  scale_y_continuous(expand = c(0,0)) +
  labs(y = expression(paste("Mean Biomass mt/",nmi^2," zeros removed")), x = "")
MeanPlot

zMeanPlot <- basePlot +
  geom_bar(data = zmean, aes(x = factor(year), y=mean),
           stat = "identity", width = .8)+
  coord_cartesian(ylim =  c(0,48))+
  scale_y_continuous(expand = c(0,0)) +
  labs(y = expression(paste("Mean Biomass mt/",nmi^2," including zeros")), x = "")
zMeanPlot

###############
# Save as a pdf
pdf("Figures/Biomass/Biomass.Mean.Bar.pdf", width=9, height=4) 
grid.arrange(zMeanPlot, MeanPlot, ncol=2)
dev.off()


##############################################################
# aggregation 

# save aggregation index 
aggr <- pmean
save(aggr,file="Data/Aggregated.Index.rda")


# positive / negative label
aggr$sign <- "p"
aggr$sign[aggr$anom <= 0] <- "n"

# plot aggregated anomaly indices
aggrplot <- basePlot +
  geom_bar(data = aggr, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  labs(x = "", y = expression(paste("Mean biomass density (mt/",nmi^2,") anomaly")))+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))
aggrplot


# Save as a pdf
pdf("Figures/Indices/AggregatedIndex_anomaly.pdf", width=4, height=3) 
aggrplot
dev.off()

