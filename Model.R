require(ggplot2)
require(MASS)
require(car)
require(xtable)
require(reshape2)
require(gridExtra)
source("base.plot.R")

# response
load("Data/Migration.percent.Index.rda") #pcan
load("Data/Migration.total.Index.rda") #bcan
load("Data/AgedMigration.5.Index.rda") #mig5
load("Data/AgedMigration.10.Index.rda") #mig10

# predictors (explanatory)
load("Data/A5.Length.Index.rda") #mla
load("Data/Mean.Age.rda") #meanage

load("Data/Can.SST.Index.rda") #sst.can
load("Data/US.SST.Index.rda") #sst.us
load("Data/Can.Chl.Index.rda") #chl.can
load("Data/US.Chl.Index.rda") #chl.us
load("Data/South.Upwel.Index.rda") #supwel
load("Data/North.Upwel.Index.rda") #nupwel




####################################################################
# bind data together
years <- as.numeric(as.character(bcan[,1]))
dat <- data.frame(mla = mla[,4], mage = meanage[,3],
                  sstcan = sst.can[,3], sstus = sst.us[,3], chlcan = chl.can[,3],
                  chlus = chl.us[,3], sup = supwel[,2], nup = nupwel[,2]) 
                  
response <- data.frame(pcan = pcan[,3], bcan = bcan[,3], 
                       amig5 = mig5[,3], amig10 = mig10[,3])

####################################################################
# check for non-orthonal variables
# slim number of predictor varibles accordingly

plot(dat)
cor.dat <- cor(dat)
cor.dat[cor.dat < 0.5] <- 0
cor.dat # mla - mage, sstus - sstcan



####################################################################
# pcan regression

# percent in canada all data
pcanmodel <- lm(response$pcan ~  dat$mage+dat$sstcan+dat$sstus+dat$chlcan+
                  dat$chlus+dat$sup+dat$nup) 
pcanmodel <- lm(response$pcan ~  dat$sstcan + dat$chlus + dat$chlcan + dat$sup) 
pcanmodel <- lm(response$pcan ~  dat$sstcan + dat$chlus + dat$sup) 
summary(pcanmodel)
anova(pcanmodel)

# model plots
par(mfrow=c(2,2))
plot(pcanmodel)

#plot residuals by year and fitted verus actual
par(mfrow=c(1,2))
plot(years,pcanmodel$residuals)
plot(response$pcan,pcanmodel$fitted.values)

# partial regression plots
avPlots(pcanmodel)

# model without influencial points
ind <- c(3)
pcanmodel.up <- lm(response$pcan[-ind] ~  dat$sstcan[-ind] + dat$chlus[-ind] + dat$sup[-ind])
summary(pcanmodel.up)
anova(pcanmodel.up)
avPlots(pcanmodel.up)




####################################################################
# pcan regression

# percent in canada all data
bcanmodel <- lm(response$bcan ~  dat$mage+dat$sstcan+dat$chlcan+
                  dat$chlus+dat$sup+dat$nup) 
bcanmodel <- lm(response$bcan ~  dat$sstcan+dat$chlcan+ dat$chlus+dat$nup) 
summary(bcanmodel)
anova(bcanmodel)

# model plots
par(mfrow=c(2,2))
plot(bcanmodel)

#plot residuals by year and fitted verus actual
par(mfrow=c(1,2))
plot(years,bcanmodel$residuals)
plot(response$pcan,bcanmodel$fitted.values)

# partial regression plots
avPlots(bcanmodel)

# model without influencial points
ind <- c(2,5)
bcanmodel.up <- lm(response$bcan[-ind] ~  dat$sstcan[-ind] + dat$chlcan[-ind] + dat$chlus[-ind] +dat$nup[-ind])
summary(bcanmodel.up)
avPlots(bcanmodel.up)



####################################################################
# mig5 regression

# percent in canada all data
mig5model <- lm(response$amig5 ~  dat$sstcan+dat$chlcan+
                  dat$chlus+dat$sup+dat$nup) 
mig5model <- lm(response$amig5 ~  dat$sstcan+dat$chlus+dat$sup) 
summary(mig5model)
anova(mig5model)

# model plots
par(mfrow=c(2,2))
plot(mig5model)

#plot residuals by year and fitted verus actual
par(mfrow=c(1,2))
plot(years,mig5model$residuals)
plot(response$pcan,mig5model$fitted.values)

# partial regression plots
avPlots(mig5model)


# model without influencial points (not sig)
ind <- c(1,4)
mig5model.up <- lm(response$amig5[-ind] ~  dat$sstcan[-ind] + dat$chlus[-ind] + dat$sup[-ind])
summary(mig5model.up)
avPlots(mig5model.up)

####################################################################
# mig10 regression

# no sig correlation



####################################################################
####################################################################
# summary outputs


# latex table
xtable(summary(pcanmodel))

# partial regression plots
part <- as.data.frame(cbind(avPlots(pcanmodel)[[1]] ,avPlots(pcanmodel)[[2]],
                        avPlots(pcanmodel)[[3]]))
names(part) <- sub(".*[$]","",names(part))
colnames(part)[c(1,3,5)] <- c("SST_Can", "Chloro_US", "Upwel_33")

#plot
for (i in c(1,3,5)){
  tmp <- part[,i:(i+1)]
  val <- names(tmp[1])
  name <- gsub("_"," ",val)
  parplot <- basePlot + geom_smooth(data =  tmp, 
                                    aes_string(x = val, y = "pcan"), 
                method = "lm", se = FALSE, size = .5, colour =  "#D60019")
    parplot <- parplot + geom_point(data =  tmp, 
                                    aes_string(x = val, y = "pcan"), 
                                    size=1.5, pch=19)
    parplot <- parplot + labs(x =paste0(name, " | Other"), 
                              y ="Response | Other")
    parplot <- parplot + theme(plot.margin = unit(c(.4,.2,.2,.2), "lines"),
                          axis.title.y= element_text(margin=margin(r = -2)))
  assign(val,parplot)
}

###############
# Save as a pdf
pdf("Figures/Model/PercenCan.PartialReg.pdf", width=7.5, height=2.7) 
grid.arrange(SST_Can,Chloro_US,Upwel_33, nrow=1)
dev.off()


