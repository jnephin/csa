require(ggplot2)
require(gridExtra)
require(plyr)
require(sp)
require(rgdal)
source("base.plot.R")



##################################
#--------     STOCK     ---------#
##################################

# Load krig data
load("Data/Krig.1998-2015.rda") #krig

#stock biomass kmt
stock <- aggregate(Biomass.density ~ year, data = krig, sum)
stock$krig <- stock$Biomass.density/1000/1000
stock <- stock[,-2]

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


# plot stock biomass in total and canada

#combine data
load("Data/Migration.total.Index.rda") #bcan
load("Data/StockBiomass.Index.rda") #stock
stock$krig <- stock$krig - bcan$krig
canstock <- rbind(stock,bcan)
canstock$grp <- c(rep("US",10),rep("Canada",10))

# x axis
yrs <- c("98","01","03","05","07","09","11","12","13","15")

stockplot <- basePlot +
  geom_bar(data = canstock, aes(x = factor(year), y = krig, fill = grp),
           colour="black", width = .8, stat = "identity", size=.1)+
  labs(x = "Years", y = "Stock biomass (kmt)")+
  scale_y_continuous(expand = c(0,0), limits = c(0,2050))+
  scale_x_discrete(labels = yrs)+
  scale_fill_manual(values = c("grey80", "grey50"), name = "") +
  theme(legend.position = c(.5,1.05), legend.direction = "horizontal", 
        legend.justification = c(.5,1),
        legend.key.height = unit(.3,"cm"), legend.key.width = unit(.3,"cm"))
stockplot

# Save as a pdf
pdf("Figures/Biomass/StockBiomass.Bar.pdf", width=3.5, height=2.5) 
stockplot
dev.off()




##########################################
##          BIOMASS FREQUENCY           ##
##########################################


# Load track data
load("Data/Track.1998-2015.rda") #track

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





##########################################
##          ZEROS - PATCHINESS          ##
##########################################


# Load track data
load("Data/Track.1998-2015.rda") #track

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

# x axis
yrs <- c("98","01","03","05","07","09","11","12","13","15")

# positive / negative label
patch$sign <- "p"
patch$sign[patch$anom <= 0] <- "n"

# plot patch anomaly indices
patchplot <- basePlot +
  geom_bar(data = patch, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  labs(x = "Years", y = "Patchiness Index")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  scale_y_continuous(expand = c(0,0), limits = c(-26,26)) +
  scale_x_discrete(labels = yrs)+
  theme(plot.margin = unit(c(.2,.1,.2,.2), "lines")) #trbl
patchplot

##########################################
##         ZEROS - AGGREGATION          ##
##########################################

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
  labs(x = "Years", y = "Biomass Density Index")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  scale_y_continuous(expand = c(0,0), limits = c(-180,180)) +
  scale_x_discrete(labels = yrs)+
  theme(plot.margin = unit(c(.2,.2,.2,.1), "lines")) #trbl
aggrplot


# Save as a pdf
pdf("Figures/Indices/PatchAggrIndex_anomaly.pdf",  width=5.2, height=2.3) 
grid.arrange(patchplot,aggrplot, nrow=1)
dev.off()

