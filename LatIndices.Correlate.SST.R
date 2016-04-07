require(plyr)
require(gridExtra)
require(reshape2)

source("base.plot.R")
load("Data/LatIndices.rda") #indices
load("Data/Track.1998-2015.rda") #track
load("Data/SST.Track.1998-2015.qd.rda") #sst.bb


#################################################################
##   Mean anomalies at different temporal and spatial scales   ##
#################################################################


## zonal mean monthly temp anomalies (grouped by 1 deg latitude)
sst.bb$Latitude <- round(sst.bb$Latitude)
zonal.anom <- ddply(sst.bb, .(Latitude, Month, Year), 
                    summarise, anom = mean(anom))
colnames(zonal.anom)[1]<- "Location"


## regional mean temp anomalies (grouped by track region)
reg.anom <- ddply(sst.bb, .(Month, Year), 
                  summarise, anom = mean(anom))
reg.anom$Location <- "Region"
reg.anom <- reg.anom[c(5,1,2,3,4)]


## regional mean temp anomalies (grouped by country)
sst.bb$Country <- "Canada"
sst.bb$Country[sst.bb$Latitude <= 48] <- "US"
cntry.anom <- ddply(sst.bb, .(Country, Month, Year), 
                    summarise, anom = mean(anom))
colnames(cntry.anom)[1]<- "Location"


## combine all sst dataframes and indices data
anom <- rbind(zonal.anom, reg.anom, cntry.anom)
regions <- anom[anom$Location %in% c("Canada", "US", "Region"),]

## cast
ct.anom <- acast(data = regions, Year ~ Location ~ Month, value.var = "anom")
 



#################################################################
##         Correlate anomalies with hake index Pcan            ##
#################################################################


## correlation between mean track sst and indices
## correlation coefficient and p-values 
cor.ind <- NULL
for (i in unique(as.character(regions$Month))){
  for (j in names(indices)[-1]){
    tmp <-  data.frame(
      cor = round(cor.test(ct.anom[,2,i], indices[,j],  method = "spearman")$estimate, 2),
      p = round(cor.test(ct.anom[,2,i], indices[,j],  method = "spearman")$p.value, 3), 
      grp = i,  
      ind = j)
    cor.ind <- rbind(cor.ind, tmp)
  }
}

## only significant correlations
sig.cor <- cor.ind[cor.ind$p < 0.05,]
sig.cor[order(sig.cor$ind),]





#############################################
##   Plot spring and summer correlations   ##
#############################################


# data for plot
dat <- merge(regions, indices, by.x = "Year",  by.y = "year")
dat <- dat[dat$Location == "Region",]


#colour palette
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


# plot
pmod <- basePlot +
  geom_point(data = dat, aes(x = anom, y = bCan, 
                        fill =factor(Year)), size=3, pch=21) +
  facet_wrap(~Month, nrow = 2) +
  labs(y = "Percentage of Hake in Canadian waters", 
       x =expression(paste("Mean SST Anomaly (",degree,"C)")))+
  scale_fill_manual(values = pal, name = "Year", guide = 
                        guide_legend(keywidth = .5,  keyheight = .7, 
                          label.position = "right"))+
  theme(legend.justification = c(1,-.06), legend.position = "right")
pmod

################
# Save as a pdf
pdf("Figures/LatIndices/Cor.SST.Ind.1998-2015.pdf", width=4.5, height=3) 
pmod
dev.off()





















