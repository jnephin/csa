require(ggplot2)
require(plyr)
require(reshape2)
source("base.plot.R")

# load migration index
load("Data/AgedMigration.5.Index.rda") #mig5
load("Data/Migration.percent.Index.rda")#pcan
load("Data/Migration.total.Index.rda") #bcanload("Data/Mean.SST.rda") #mean.sst


############################################################################
############################################################################
## upwelling index - NOAA PFEL ##

# load upwelling index
up <- read.table("Data/pfel.index.txt",header=T,skip=1)

# years between 1998 to 2015
upy <- up[up$YEAR %in% 1998:2015,]

# melt
upm <- melt(upy, id = c("LAT", "LONG", "YEAR"))
colnames(upm)[4] <- "Month"

# north and south index
ups <- upm[upm$LAT %in% c("33N"),] #,"36N","39N","42N"
upn <- upm[upm$LAT %in% c("45N"),] #,"48N","51N","54N"



############################################################################
############################################################################

##########   CLIMATOLOGIES   ###########

## calculate monthly climatology (between 1998 to 2015)
ups <- ddply(ups, .(Month), transform, clim = mean(value))
upn <- ddply(upn, .(Month), transform, clim = mean(value))

## calculate anomalies
ups$anom <-  ups$value - ups$clim
upn$anom <-  upn$value - upn$clim


## only years of surveys
years <- c(1998,2001,2003,2005,2007,2009,2011,2012,2013,2015)
ups <- ups[ups$YEAR %in% years,]
upn <- upn[upn$YEAR %in% years,]

## dates
ups$date <- as.Date(paste("01", ups$Month, ups$YEAR), format = "%d %b %Y")
upn$date <- as.Date(paste("01", upn$Month, upn$YEAR), format = "%d %b %Y")

#rbind
upwel <- rbind(ups,upn)

# positive / negative label
upwel$sign <- "p"
upwel$sign[upwel$anom <= 0] <- "n"
upwel$pos <- 1
upwel$pos[upwel$value <= 0] <- 0

# strip labels
labs <- data.frame(LAT = c("33N","45N"),x = rep("1998-06-01",2), y = rep(350,2))
laba <- data.frame(LAT = c("33N","45N"),x = rep("1998-06-01",2), y = rep(120,2))

############################################################################
############################################################################

##########   PLOT   ###########

## value
vplot <- basePlot +
  facet_wrap(~LAT, nrow = 2)+
  geom_vline(xintercept = seq(12.5,120,12),size = .1, colour = "grey80")+
  geom_hline(yintercept = 0, size = .5, linetype = 3)+
  geom_point(data = upwel, aes(x = factor(date), y = value),
             size =1, show.legend = FALSE)+
  geom_line(data = upwel, aes(x = factor(date), y = value, group = 1),
            show.legend = FALSE)+
  geom_text(data = labs, aes(x = factor(x), y =  y, label = LAT), size = 3)+
  labs(x = "", y = "Upwelling index")+
  scale_x_discrete(breaks = factor(ups$date[51:60]),labels = years)+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"))
vplot

# Save as a pdf
pdf("Figures/Indices/UpwelIndex_mean.pdf", width=7, height=5) 
vplot
dev.off()


## anomaly
aplot <- basePlot +
  facet_wrap(~LAT, nrow = 2)+
  geom_vline(xintercept = seq(12.5,120,12),size = .1, colour = "grey80")+
  geom_bar(data = upwel, aes(x = factor(date), y = anom, fill = factor(sign)),
           width = .75, show.legend = FALSE, stat = "identity")+
  geom_text(data = laba, aes(x = factor(x), y =  y, label = LAT), size = 3)+
  labs(x = "", y = "Upwelling Index anomaly")+
  scale_x_discrete(breaks = factor(ups$date[51:60]),labels = years)+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"))
aplot

# Save as a pdf
pdf("Figures/Indices/UpwelIndex_anomly.pdf", width=6, height=3) 
aplot
dev.off()



#################################################################
##       Correlate upwel with hake migration index            ##
#################################################################


## correlation between upwel anomaly and migration index
## correlation coefficient and p-values 
cor.up <- NULL
for (i in unique(as.character(upwel$Month))){
  for (j in c("33N","45N")){
    for (k in c("pcan","bcan","mig5")){
      loc <- upwel[upwel$LAT == j,]
      test <- cor.test(get(k)$anom, loc$anom[loc$Month == i], method = "spearman")
      tmp <-  data.frame(
        cor = round(test$estimate, 2),
        p = round(test$p.value, 3), 
        month = i,  
        ind = k,
        area = j)
      cor.up <- rbind(cor.up, tmp)
    }
  }
}

## label significant correlations
cor.up$sig <- "n"
cor.up$sig[cor.up$p < 0.05] <- "y"

## label strata
cor.up$ind <- as.character(cor.up$ind)
cor.up$ind[cor.up$ind == "mig5"] <- "Percent age-5 biomass"
cor.up$ind[cor.up$ind == "bcan"] <- "Total biomass"
cor.up$ind[cor.up$ind == "pcan"] <- "Percent biomass"

# months: change text to number
cor.up$month <-rep(1:12,each=6)

# plot migration index upwelling correltion
migup <- basePlot +
  geom_point(data = cor.up, 
             aes(x = factor(month), y = cor, shape = sig, colour = area),
             size = 1.5)+
  geom_line(data = cor.up, size = .5,
            aes(x = factor(month), y = cor, group = area, colour = area))+
  facet_grid(~ind)+
  geom_hline(yintercept = 0, linetype = 3, size = .4, colour = "grey30")+
  labs(x = "Months", y = "Correlation coefficient")+
  scale_shape_manual(values = c(1,15), guide = FALSE)+
  scale_colour_manual(values = c("grey30", "grey60"), 
                      name = "", label = c("South","North"))+ 
  theme(legend.position = c(.65,1.1), legend.justification = c(0,1),
        panel.margin = unit(.2, "lines"))
migup

# Save as a pdf
pdf("Figures/Correlate/Upwel.Migration.pdf", width=7, height=2.5) 
migup
dev.off()







############################################################################
############################################################################
# april, may june anomaly index

upind <- upwel[upwel$Month %in% c("JUN","JUL","AUG"),] 
upind <- aggregate(anom ~ LAT + YEAR, mean, data = upind)

# save south and north index
supwel <- upind[upind$LAT == "33N",-1]
save(supwel, file = "Data/South.Upwel.Index.rda")
nupwel <- upind[upind$LAT == "45N",-1]
save(nupwel, file = "Data/North.Upwel.Index.rda")









