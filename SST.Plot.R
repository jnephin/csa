require(plyr)
require(gridExtra)

source("base.plot.R")
load("Data/SST.Track.1998-2015.qd.rda") #sst.bb


#################################################################
##   Mean anomalies at different temporal and spatial scales   ##
#################################################################

## zonal temp anomalies 
summer.zonal.fine <- ddply(sst.bb[sst.bb$Month == "Summer",], 
                           .(Latitude, Year), with, each(mean, sd)(anom))
spring.zonal.fine <- ddply(sst.bb[sst.bb$Month == "Spring",], 
                           .(Latitude, Year), with, each(mean, sd)(anom))

## round Latitude to 1 deg
gsst.bb <- sst.bb
gsst.bb$Latitude <- round(gsst.bb$Latitude)

## regional temp anomalies by month
monthly.reg <- ddply(gsst.bb[!(gsst.bb$Month == "Spring" | gsst.bb$Month == "Summer"),],
                     .(Month, Year), with, each(mean, sd)(anom))

## regional temp anomalies by year in canada and us waters
gsst.bb$Country <- "Canada"
gsst.bb$Country[gsst.bb$Latitude <= 48] <- "US"
summer.cntry <- ddply(gsst.bb[gsst.bb$Month == "Summer" | sst.bb$Month == "Spring",], 
                      .(Year, Month, Country), with, each(mean, sd)(anom))


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")



##########################################
##      SPRING ZONAL TEMP ANOMALIES     ##
##########################################

LatSpringPlot <- basePlot +
  geom_hline(yintercept = 0, size=.1, linetype = "dashed")+
  geom_line(data = spring.zonal.fine, aes(x = Latitude, y = mean, colour = factor(Year))) +
  geom_ribbon(data = spring.zonal.fine, aes(x = Latitude, fill = factor(Year), ymin=mean-sd, ymax=mean+sd), alpha =.1)+
  scale_colour_manual(values = pal, name = "Year", guide = 
                        guide_legend(direction = "horizontal", nrow=1,
                                     keywidth = .5,  keyheight = .1,
                                     label.position = "top", override.aes = list(size=.8)))+
  scale_fill_manual(values = pal, guide = "none")+
  coord_cartesian(ylim=c(-2,2))+
  scale_x_continuous(limits = c(32.625,58.125), breaks = c(35,40,45,50,55),expand = c(0,0)) +
  labs(x = expression(paste("Latitude (",degree,")")), 
       y = expression(paste("Spring Mean Temperature Anomalies (",degree,"C)")))+
  theme(legend.justification = c(1,-.06), legend.position = c(1,-.06))
LatSpringPlot
 
###############
# Save as a pdf
pdf("Figures/SST/ZonalSpringTemp.1998-2015.pdf", width=5.5, height=3.5) 
LatSpringPlot
dev.off()


##########################################
##      SUMMER ZONAL TEMP ANOMALIES     ##
##########################################



LatSummerPlot <- basePlot +
  geom_hline(yintercept = 0, size=.1, linetype = "dashed")+
  geom_line(data = summer.zonal.fine, 
            aes(x = Latitude, y = mean, colour = factor(Year))) +
  geom_ribbon(data = summer.zonal.fine, 
              aes(x = Latitude, fill = factor(Year), ymin=mean-sd, ymax=mean+sd), alpha =.1)+
  scale_colour_manual(values = pal, name = "Year", guide = 
                        guide_legend(direction = "horizontal", nrow=1,
                                     keywidth = .5,  keyheight = .1,
                                     label.position = "top", override.aes = list(size=.8)))+
  scale_fill_manual(values = pal, guide = "none")+
  coord_cartesian(ylim=c(-2,3))+
  scale_x_continuous(limits = c(32.625,58.125), breaks = c(35,40,45,50,55),expand = c(0,0)) +
  labs(x = expression(paste("Latitude (",degree,")")), 
       y = expression(paste("Summer Mean Temperature Anomalies (",degree,"C)")))+
  theme(legend.justification = c(1,-.06), legend.position = c(1,-.06))
LatSummerPlot

###############
# Save as a pdf
pdf("Figures/SST/ZonalSummerTemp.1998-2015.pdf", width=5.5, height=3.5) 
LatSummerPlot
dev.off()




##########################################
##     REGIONAL MONTHLY TEMP ANOMALIES  ##
##########################################

#colours
pal <- colorRampPalette(c("#FFE100", "#fd8d3c" , "#E33030",  "#910D0D"), bias = 1.2, space = c("rgb", "Lab"))(6)
barplot(height = rep(1,6), col =pal)


regPlot <- basePlot +
  geom_hline(yintercept = 0, size=.1, linetype = "dashed")+
  geom_point(data = monthly.reg, aes(x = factor(Year), y = mean, colour = factor(Month)), 
             position = position_dodge(width = .6), size =1.5) +
  geom_errorbar(data = monthly.reg, aes(x = factor(Year), 
              ymin = mean-sd, ymax = mean+sd, colour = factor(Month)), 
              position = position_dodge(width = .6), size =.6, width =.4) +
  scale_colour_manual(values = pal, name = "", 
                      labels = c("Apr","May", "Jun", "Jul", "Aug","Sep"),
                      guide = guide_legend(direction = "horizontal", nrow = 1,
                                     keywidth = .9,  keyheight = .3,
                                     label.position = "top"))+
  coord_cartesian()+
  labs(x = expression(paste("Year")), 
       y = expression(paste("Regional Temperature Anomaly (",degree,"C)")))+
  theme(legend.justification = c(-.04,-.05), legend.position = c(-.04,-.05))
regPlot

###############
# Save as a pdf
pdf("Figures/SST/RegionalMonthlyTemp.1998-2015.pdf", width=6, height=3.5) 
regPlot
dev.off()



##########################################
##     REGIONAL COUNTRY TEMP ANOMALIES  ##
##########################################

#colours
pal <- c("#377eb8", "#ff7f00")

countryPlot <- basePlot +
  geom_hline(yintercept = 0, size=.1, linetype = "dashed")+
  facet_grid(Month~.)+
  geom_point(data = summer.cntry, aes(x = factor(Year), 
                                      y = mean, 
                                      colour = factor(Country)), 
             position = position_dodge(width = .5), size =1.5) +
  geom_errorbar(data = summer.cntry, aes(x = factor(Year), 
                                         ymin = mean-sd, ymax = mean+sd, 
                                         colour = factor(Country)), 
                position = position_dodge(width = .5), size =.6, width =.4) +
  scale_colour_manual(values = pal, name = "", labels = c("Canada", "US"), guide = 
                        guide_legend(direction = "horizontal", keywidth = .9, 
                                     keyheight = .3, label.position = "top"))+
  coord_cartesian()+
  labs(x = expression(paste("Year")), 
       y = expression(paste("Regional Temperature Anomaly (",degree,"C)")))+
  theme(legend.justification = c(-.05,1.04), legend.position = c(-.05,1.04))
countryPlot

###############
# Save as a pdf
pdf("Figures/SST/RegionalCountryTemp.1998-2015.pdf", width=5, height=4.5) 
countryPlot
dev.off()


