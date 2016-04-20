require(ggplot2)
require(MASS)
require(car)
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
pcanmodel <- lm(response$pcan ~  dat$sup + dat$sstus) 
pcanmodel <- lm(response$pcan ~  dat$sstcan + dat$sup + dat$chlus) 
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



####################################################################
# pcan regression

# percent in canada all data
bcanmodel <- lm(response$bcan ~  dat$sstcan)
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





####################################################################
# mig5 regression

# percent in canada all data
mig5model <- lm(response$amig5 ~ dat$sstcan + dat$sup)
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



####################################################################
# mig10 regression

# no sig correlation
