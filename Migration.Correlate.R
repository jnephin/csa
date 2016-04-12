require(ggplot2)
source("base.plot.R")

# load migration index and mean sst and chlo data
load("Data/Aged.Migration.Index.rda") #amig
load("Data/LatIndices.rda") #indices$bCan
indices <- indices[,c(1,3)]
load("Data/Mean.SST.Can.rda") #can.sst
load("Data/Mean.SST.US.rda") #us.sst
load("Data/Mean.chl.rda") #mean.chl


#################################################################
##       Correlate Chloro with hake migration index            ##
#################################################################

## correlation between mean chloro a and migration index
## correlation coefficient and p-values 
cor.chl <- NULL
for (i in unique(as.character(can.sst$Month))){
  for (k in c("amig","indices")){
    test <- cor.test(get(k)[,2], mean.chl$chl.anom[mean.chl$Month == i], method = "spearman")
    tmp <-  data.frame(
      cor = round(test$estimate, 2),
      p = round(test$p.value, 3), 
      grp = i,  
      ind = k)
    cor.chl <- rbind(cor.chl, tmp)
  }
}


## label significant correlations
cor.chl$sig <- "n"
cor.chl$sig[cor.chl$p < 0.05] <- "y"

## label strata
cor.chl$ind <- as.character(cor.chl$ind)
cor.chl$ind[cor.chl$ind == "amig"] <- "Age-adjusted migration index"
cor.chl$ind[cor.chl$ind == "indices"] <- "Biomass in Canadian waters"


# plot migration index sst correltion
migchl <- basePlot +
  geom_point(data = cor.chl, aes(x = grp, y = cor, shape = sig), 
             size = 2)+
  geom_line(data = cor.chl, aes(x = grp, y = cor, group = 1))+
  facet_wrap(~ind)+
  geom_hline(yintercept = 0, linetype = 3, size = .4, colour = "grey30")+
  labs(x = "Month", y = "Correlation coefficient")+
  scale_shape_manual(values = c(1,15), guide = FALSE)+
  theme(legend.position = c(.1,.1), legend.justification = c(.1,.1))
migchl


# Save as a pdf
pdf("Figures/Correlations/CHL.Migration.Correlate.pdf", width=7, height=3) 
migchl
dev.off()




#################################################################
##         Correlate SST with hake migration index             ##
#################################################################


## correlation between mean can and us sst and migration index
## correlation coefficient and p-values 
cor.sst <- NULL
for (i in unique(as.character(can.sst$Month))){
  for (j in c("can.sst","us.sst")){
    for (k in c("amig","indices")){
      sst <- get(j)
      test <- cor.test(get(k)[,2], sst$temp.anom[sst$Month == i],  method = "spearman")
      tmp <-  data.frame(
        cor = round(test$estimate, 2),
        p = round(test$p.value, 3), 
        grp = i,  
        sst = j,
        ind = k)
      cor.sst <- rbind(cor.sst, tmp)
    }
  }
}


## label significant correlations
cor.sst$sig <- "n"
cor.sst$sig[cor.sst$p < 0.05] <- "y"

## label strata
cor.sst$ind <- as.character(cor.sst$ind)
cor.sst$ind[cor.sst$ind == "amig"] <- "Age-adjusted migration index"
cor.sst$ind[cor.sst$ind == "indices"] <- "Biomass in Canadian waters"


# plot migration index sst correltion
migsst <- basePlot +
  geom_point(data = cor.sst, aes(x = grp, y = cor, shape = sig, colour = sst), 
             size = 2)+
  geom_line(data = cor.sst, aes(x = grp, y = cor, group = sst, colour = sst))+
  facet_wrap(~ind)+
  geom_hline(yintercept = 0, linetype = 3, size = .4, colour = "grey30")+
  labs(x = "Month", y = "Correlation coefficient")+
  scale_colour_manual(values = c("#56B4E9", "#E69F00"), name = "", label = c("Canada","US"))+ 
  scale_shape_manual(values = c(1,15), guide = FALSE)+
  scale_y_continuous(limits=c(-.05,.8))+
  theme(legend.position = c(.1,.1), legend.justification = c(.1,.1))
migsst


# Save as a pdf
pdf("Figures/Correlations/SST.Migration.Correlate.pdf", width=7, height=3) 
migsst
dev.off()


