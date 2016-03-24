source("base.plot.R")
require(plyr)
require(scales)
require(gridExtra)
load("Data/LatIndices.rda") #indices
load("Data/Track.1998-2015.rda") #track


# sum Biomass by latitude (at 0.01 deg resolution)
track$Latitude <- round(track$Lat,2)
sum.track <- ddply(track, .(Latitude, year),  with, each(sum)(Biomass.density))

# sum Biomass values cummulatively by year
ord.track <- sum.track[order(sum.track$Latitude),]
cumtrack <- ddply(ord.track, "year", transform, cumBiomass = cumsum(sum))

#percent Biomass
cumtrack <- ddply(cumtrack, "year", transform, maxBiomass = tail(cumBiomass,1))
cumtrack$cdf <-  cumtrack$cumBiomass/cumtrack$maxBiomass


#################################################################
#################################################################
###                        PLOTS                              ###
#################################################################
#################################################################


pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


## Plot cum biomass v. Latitude
cumplot <- basePlot +
  geom_rect(data = data.frame(y= 0, ymax = 390000000, x = 48.4, xmax = 58.5),
            aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), fill = "grey80") +
  geom_path(data = cumtrack, 
            aes(x = Latitude, y = cumBiomass, colour = factor(year)), size = .7) +
 scale_colour_manual(values = pal, name = "")+
  annotate("text", x = 48.2, y = 365000000, label = "US", hjust = 1,size=3)+
  annotate("text", x = 58.2, y = 365000000, label = "CANADA", hjust = 1, colour = "White",size=3)+
  coord_cartesian(xlim = c(35.,58.5), ylim= c(0,380000000), expand = c(0,0))+
  labs(x = "Latitude", y = "Cumulative Biomass") +
  theme(legend.key.height = unit(.4,"cm"), legend.key.width = unit(.3,"cm"),
        legend.justification = c(0,1), legend.position = c(-0.01,1.1))
cumplot

#################################################################
# Save as a pdf
pdf("Figures/LatIndices/CumBiomass.Curves.1998-2015.pdf", width=5.5, height=3) 
cumplot
dev.off()



## Plot percent biomass v. Latitude
perplot <- basePlot +
  geom_rect(data = data.frame(y= -0.01, ymax = 1.01, x = 48.4, xmax = 58.5),aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), fill = "grey80") +
  geom_path(data = cumtrack, aes(x = Latitude, y = cdf, colour = factor(year)), size = .7) +
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
pdf("Figures/LatIndices/PercentBiomass.Curves.1998-2015.pdf", width=5.5, height=3) 
perplot
dev.off()





# Percent index by year barplot
indplot <- basePlot +
  geom_bar(data = indices, aes(x = factor(year), y = pCan), 
           stat = "identity", fill = "grey30", width=.8)+
  scale_y_continuous(limits = c(0, 54), expand = c(0,0)) +
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
pdf("Figures/LatIndices/PerCan.Barplot.1998-2015.pdf", width=4, height=2.8) 
indplot
dev.off()




# Biomass index by year barplot
bioindplot <- basePlot +
  geom_bar(data = indices, aes(x = factor(year), y = bCan), 
           stat = "identity", fill = "grey30", width=.8)+
  scale_y_continuous(limits = c(0, 40), expand = c(0,0)) +
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
pdf("Figures/LatIndices/BioCan.Barplot.1998-2015.pdf", width=4, height=2.8) 
bioindplot
dev.off()





######################################################################
## How do these indices of hake distribution with lat relate to the total Biomass for each year? 

tot <- data.frame(
  krig = c(1534.60, 861.74, 2137.53, 1376.10, 942.72, 
           1502.27, 674.57,1279.42, 1929.24, 2156.00),
  pCan = indices$pCan, year = indices$year)

# correlation
ct <- cor.test(tot$krig,tot$pCan, method = "spearman")
r <- round(ct$estimate,2)
p <- round(ct$p.value,3)

ptot <- basePlot +
  geom_point(data =  tot, 
             aes(y = pCan, x = krig, fill = factor(year)),size=2.5, pch=21) +
  scale_fill_manual(values = pal, name = "")+
  labs(y = "Percentage of Hake in Canadian waters", 
       x = "Kriged Biomass (kmt)") +
  scale_x_continuous(labels=comma) +
  theme(legend.key.height = unit(.4,"cm"), 
        legend.key.width = unit(.3,"cm"),
        axis.text.x = element_text(size=8, colour = "black"),
        legend.position = "right")
ptot


#################################################################
# Save as a pdf
pdf("Figures/LatIndices/Cor.PCan.KrigBiomass.1998-2015.pdf", width=5, height=3.5) 
grid.arrange(ptot)
grid.text(bquote(rho~"="~.(r)), x = .14, y = .9, hjust =0, gp = gpar(fontsize = 8))
grid.text(bquote("P"~"="~.(p)), x = .14, y = .85, hjust =0, gp = gpar(fontsize = 8))
dev.off()



##-- N-S Hake distribution seems to be independant of total Hake biomass





