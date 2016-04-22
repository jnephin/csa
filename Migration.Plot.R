source("base.plot.R")
require(plyr)
require(scales)
require(gridExtra)
load("Data/Migration.percent.Index.rda") #pcan
load("Data/Migration.total.Index.rda") #bcan
load("Data/Krig.1998-2015.rda") #krig


# sum Biomass by latitude (at 0.01 deg resolution)
krig$Latitude <- round(krig$Lat,2)
sum.krig <- ddply(krig, .(Latitude, year),  with, each(sum)(Biomass.density/1000/1000))

# sum Biomass values cummulatively by year
ord.krig <- sum.krig[order(sum.krig$Latitude),]
cumkrig <- ddply(ord.krig, "year", transform, cumBiomass = cumsum(sum))

# percent Biomass
cumkrig <- ddply(cumkrig, "year", transform, maxBiomass = tail(cumBiomass,1))
cumkrig$cdf <-  cumkrig$cumBiomass/cumkrig$maxBiomass



#################################################################
#################################################################
###                        PLOTS                              ###
#################################################################
#################################################################


pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


## Plot cum biomass v. Latitude
cumplot <- basePlot +
  geom_rect(data = data.frame(y= 0, ymax = 2100, x = 48.4, xmax = 58.5),
            aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), fill = "grey80") +
  geom_path(data = cumkrig, 
            aes(x = Latitude, y = cumBiomass, colour = factor(year)), size = .7) +
  scale_colour_manual(values = pal, name = "")+
  annotate("text", x = 48.2, y = 2000, label = "US", hjust = 1,size=3)+
  annotate("text", x = 58.2, y = 2000, label = "CANADA", hjust = 1, colour = "White",size=3)+
  coord_cartesian(xlim = c(35.,58.5), ylim= c(0,2100), expand = c(0,0))+
  labs(x = "Latitude", y = "Cumulative Biomass (kmt)") +
  theme(legend.key.height = unit(.4,"cm"), legend.key.width = unit(.3,"cm"),
        legend.justification = c(0,1), legend.position = c(-0.01,1.1))
cumplot

#################################################################
# Save as a pdf
pdf("Figures/Migration/CumBiomass.Curves.pdf", width=5.5, height=3) 
cumplot
dev.off()



## Plot percent biomass v. Latitude
perplot <- basePlot +
  geom_rect(data = data.frame(y= -0.01, ymax = 1.01, x = 48.4, xmax = 58.5),aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), fill = "grey80") +
  geom_path(data = cumkrig, aes(x = Latitude, y = cdf, colour = factor(year)), size = .7) +
  scale_colour_manual(values = pal, name = "")+
  annotate("text", x = 48.2, y = 0, label = "US", hjust = 1, vjust = -.2, size=3)+
  annotate("text", x = 58.2, y = 0, label = "CANADA", hjust = 1, vjust = -.2, colour = "White",size=3)+
  coord_cartesian(xlim = c(34,58.5), ylim= c(-0.01,1.01), expand = c(0,0))+
  labs(x = "Latitude", y = "Biomass Percentile") +
  theme(legend.key.height = unit(.4,"cm"), legend.key.width = unit(.3,"cm"),
        legend.justification = c(0,1), legend.position = c(-0.01,1.1)) 
perplot

#################################################################
# Save as a pdf
pdf("Figures/Migration/PercentBiomass.Curves.pdf", width=5.5, height=3) 
perplot
dev.off()




# Percent index by year barplot
indplot <- basePlot +
  geom_bar(data = pcan, aes(x = factor(year), y = percent), 
           stat = "identity", fill = "grey30", width=.8)+
  scale_y_continuous(limits = c(0, 52), expand = c(0,0)) +
  labs(x = "year", y = "Percentage of Hake in Canadian waters")  +
  theme(legend.key.height = unit(.4,"cm"), 
        legend.key.width = unit(.4,"cm"),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        legend.position = c(.8,.65),
        axis.title.y = element_text(vjust = .7))
indplot


#################################################################
# Save as a pdf
pdf("Figures/Migration/PerCan.Barplot.pdf", width=4, height=2.8) 
indplot
dev.off()




# Biomass index by year barplot
bioindplot <- basePlot +
  geom_bar(data = bcan, aes(x = factor(year), y = krig), 
           stat = "identity", fill = "grey30", width=.8)+
  scale_y_continuous(limits = c(0, 640), expand = c(0,0)) +
  labs(x = "year", y = "Hake biomass (kmt) in Canadian waters")  +
  theme(legend.key.height = unit(.4,"cm"), 
        legend.key.width = unit(.4,"cm"),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9),
        legend.position = c(.8,.65),
        axis.title.y = element_text(vjust = .7))
bioindplot


#################################################################
# Save as a pdf
pdf("Figures/Migration/BioCan.Barplot.pdf", width=4, height=2.8) 
bioindplot
dev.off()







