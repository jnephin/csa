require(ggplot2)
source("base.plot.R")

# load migration,  sst and chl indices 
load("Data/AgedMigration.5.Index.rda") #mig5
load("Data/Migration.percent.Index.rda")#pcan
load("Data/Migration.total.Index.rda") #bcan
load("Data/Mean.SST.rda") #mean.sst
load("Data/Mean.chl.rda") #mean.chl



############################################################################
############################################################################
##########   PLOTS   ###########

## dates
mean.sst$date <- as.Date(paste("01", mean.sst$Month, mean.sst$Year), format = "%d %m %Y")
mean.chl$date <- as.Date(paste("01", mean.chl$Month, mean.chl$Year), format = "%d %m %Y")
years <- c(1998,2001,2003,2005,2007,2009,2011,2012,2013,2015)

# positive / negative label
mean.sst$sign <- "p"
mean.sst$sign[mean.sst$anom <= 0] <- "n"
mean.chl$sign <- "p"
mean.chl$sign[mean.chl$anom <= 0] <- "n"

# strip labels
lab.sst <- data.frame(grp = c("Canada","US"),x = rep("1998-04-01",2), y = rep(1.5,2))
lab.chl <- data.frame(grp = c("Canada","US"),x = rep("1998-04-01",2), y = rep(3.3,2))



############################################################################
# SST anomaly

splot <- basePlot +
  facet_wrap(~grp, nrow = 2)+
  geom_vline(xintercept = seq(6.5,60,6),size = .1, colour = "grey80")+
  geom_bar(data = mean.sst, aes(x = factor(date), y = anom, fill = factor(sign)),
           show.legend = FALSE, stat = "identity", width=.75)+
  geom_text(data = lab.sst, aes(x = factor(x), y =  y, label = grp), 
            size = 3, hjust = c(0,-.7))+
  labs(x = "Years", y = "Monthly SST anomaly")+
  scale_x_discrete(breaks = factor(mean.sst$date[21:30]),labels = years)+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"))
splot

# Save as a pdf
pdf("Figures/Indices/SSTIndex_anomly.pdf", width=6, height=3) 
splot
dev.off()


############################################################################
# Chloro anomaly

cplot <- basePlot +
  facet_wrap(~grp, nrow = 2)+
  geom_vline(xintercept = seq(6.5,60,6),size = .1, colour = "grey80")+
  geom_bar(data = mean.chl, aes(x = factor(date), y = anom, fill = factor(sign)),
           show.legend = FALSE, stat = "identity", width=.75)+
  geom_text(data = lab.chl, aes(x = factor(x), y =  y, label = grp), 
            size = 3, hjust = c(0,-.7))+
  labs(x = "Years", y = "Monthly Chlorophyll anomaly")+
  scale_x_discrete(breaks = factor(mean.chl$date[21:30]),labels = years)+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"))
cplot

# Save as a pdf
pdf("Figures/Indices/CHLIndex_anomly.pdf", width=6, height=3) 
cplot
dev.off()




#################################################################
##       Correlate sst/chl with hake migration index            ##
#################################################################

## correlation between mean sst with migration index
## correlation coefficient and p-values 
cor.sst <- NULL
for (i in unique(as.character(mean.sst$Month))){
  for (j in c("Canada","US")){
    for (k in c("pcan","bcan","mig5")){
      loc <- mean.sst[mean.sst$grp == j,]
      test <- cor.test(get(k)$anom, loc$anom[loc$Month == i], method = "spearman")
      tmp <-  data.frame(
        cor = round(test$estimate, 2),
        p = round(test$p.value, 3), 
        month = i,  
        ind = k,
        area = j)
      cor.sst <- rbind(cor.sst, tmp)
    }
  }
}


## correlation between mean chl with migration index
## correlation coefficient and p-values 
cor.chl <- NULL
for (i in unique(as.character(mean.chl$Month))){
  for (j in c("Canada","US")){
    for (k in c("pcan","bcan","mig5")){
      loc <- mean.chl[mean.chl$grp == j,]
      test <- cor.test(get(k)$anom, loc$anom[loc$Month == i], method = "spearman")
      tmp <-  data.frame(
        cor = round(test$estimate, 2),
        p = round(test$p.value, 3), 
        month = i,  
        ind = k,
        area = j)
      cor.chl <- rbind(cor.chl, tmp)
    }
  }
}



####################################################################
## sst cor plot

## label significant correlations
cor.sst$sig <- "n"
cor.sst$sig[cor.sst$p < 0.05] <- "y"

## label strata
cor.sst$ind <- as.character(cor.sst$ind)
cor.sst$ind[cor.sst$ind == "mig5"] <- "Percent age-5 biomass"
cor.sst$ind[cor.sst$ind == "bcan"] <- "Total biomass"
cor.sst$ind[cor.sst$ind == "pcan"] <- "Percent biomass"


# plot migration index sst correltion
migsst <- basePlot +
  geom_point(data = cor.sst,   size = 1.5,
             aes(x = month, y = cor, shape = sig, colour = area))+
  geom_line(data = cor.sst, size = .5,
            aes(x = month, y = cor, group = area, colour = area))+
  facet_wrap(~ind)+
  geom_hline(yintercept = 0, linetype = 3, size = .4, colour = "grey30")+
  labs(x = "Months", y = "Correlation coefficient")+
  scale_colour_manual(values = c("grey60", "grey30"), name = "", 
                      label = c("Canada","US"))+ 
  scale_shape_manual(values = c(1,15), guide = FALSE)+
  theme(legend.position = c(.67,.1), legend.justification = c(.01,.1))
migsst


# Save as a pdf
pdf("Figures/Correlate/SST.Migration.pdf", width=7, height=2.5) 
migsst
dev.off()



####################################################################
## chl cor plot

## label significant correlations
cor.chl$sig <- "n"
cor.chl$sig[cor.chl$p < 0.05] <- "y"

## label strata
cor.chl$ind <- as.character(cor.chl$ind)
cor.chl$ind[cor.chl$ind == "mig5"] <- "Percent age-5 biomass"
cor.chl$ind[cor.chl$ind == "bcan"] <- "Total biomass"
cor.chl$ind[cor.chl$ind == "pcan"] <- "Percent biomass"


# plot migration index chl correltion
migchl <- basePlot +
  geom_point(data = cor.chl, size = 1.5,
             aes(x = month, y = cor, shape = sig, colour = area))+
  geom_line(data = cor.chl,  size = .5,
            aes(x = month, y = cor, group = area, colour = area))+
  facet_wrap(~ind)+
  geom_hline(yintercept = 0, linetype = 3, size = .4, colour = "grey30")+
  labs(x = "Months", y = "Correlation coefficient")+
  scale_colour_manual(values = c("grey60", "grey30"), name = "", 
                      label = c("Canada","US"))+ 
  scale_shape_manual(values = c(1,15), guide = FALSE)+
  theme(legend.position = c(.67,-.1), legend.justification = c(-.01,-.1))
migchl


# Save as a pdf
pdf("Figures/Correlate/CHL.Migration.pdf", width=7, height=2.5) 
migchl
dev.off()




############################################################################
############################################################################
# april, may june anomaly index

chl.can <- mean.chl[mean.chl$grp == "Canada",-1]
chl.us <- mean.chl[mean.chl$grp == "US",-1]
chl.can <- chl.can[chl.can$Month == "08",2:4] 
chl.us <- chl.us[chl.us$Month == "05",2:4] 

# sst group by country, choose month
sst.can <- mean.sst[mean.sst$grp == "Canada",-1]
sst.us <- mean.sst[mean.sst$grp == "US",-1]
sst.can <- sst.can[sst.can$Month == "08",2:4] 
sst.us <- sst.us[sst.us$Month == "05",2:4] 

# save indices

save(chl.can, file = "Data/Can.Chl.Index.rda")
save(chl.us, file = "Data/US.Chl.Index.rda")

save(sst.can, file = "Data/Can.SST.Index.rda")
save(sst.us, file = "Data/US.SST.Index.rda")


