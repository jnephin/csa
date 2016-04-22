require(ggplot2)
require(plyr)
require(reshape2)
source("base.plot.R")

# response
load("Data/Migration.percent.Index.rda") #pcan
load("Data/Migration.total.Index.rda") #bcan
load("Data/AgedMigration.5.Index.rda") #mig5
load("Data/AgedMigration.10.Index.rda") #mig10

# predictors (explanatory)
load("Data/A5.Length.Index.rda") #mla
load("Data/Mean.Age.rda") #meanage
load("Data/Mean.Age.Can.rda") #meanagecan
load("Data/Patchiness.Index.rda") #patch
load("Data/Aggregated.Index.rda") #aggr
load("Data/StockBiomass.Index.rda") #stock



####################################################################
# bind data together
years <- as.numeric(as.character(bcan[,1]))
dat <- data.frame(mla = mla[,4], mage = meanage[,3], magecan = meanagecan[,3],
                  patch = patch[,3], aggr = aggr[,3], stock = stock[,3]) 
response <- data.frame(pcan = pcan[,3], bcan = bcan[,3], amig5 = mig5[,3])



####################################################################
# check for non-orthonal variables and response

plot(dat)
cor.dat <- cor(dat)
cor.dat[cor.dat < 0.5] <- 0
cor.dat #mla - mage, patch - aggr, magecan - patch, aggr - stock

plot(response)
cor.response <- cor(response)
cor.response[cor.response < 0.5] <- 0
cor.response



####################################################################
# plot correlation between response variables



# response data
response.df <- data.frame(x = c(response$pcan,response$pcan,response$bcan),
                          y =c(response$bcan, response$amig5, response$amig5),
                          grp =  c(rep("Percent - Total",10),
                                   rep("Percent - Percent Age 5",10),
                                   rep("Total - Percent Age 5",10)))
response.df$grp <- factor(response.df$grp, levels = c("Percent - Total",
                                                      "Percent - Percent Age 5",
                                                      "Total - Percent Age 5"))
# correlation function
cors <- function(df){
 rho = round(cor.test(df$x,df$y)$estimate,2)
 p = round(cor.test(df$x,df$y)$p.value,3)
 return(cbind(rho,p))
}

# apply cor function for each group
cor.res <- as.data.frame.matrix(ddply(response.df, .(grp), cors))
cor.res$x <- ddply(response.df, .(grp), summarise, x = min(x))[2]*.7
cor.res$y <- ddply(response.df, .(grp), summarise, y = max(y))[2]*.95
cor.res$rho <- paste("r = ", cor.res$rho)
cor.res$grp <- factor(cor.res$grp, levels = c("Percent - Total",
                                                      "Percent - Percent Age 5",
                                                      "Total - Percent Age 5"))
#plot
resplot <- basePlot +
  facet_wrap(~grp, scale = "free")+
  geom_point(data =  response.df, aes(x = x, y = y), size=1.5, pch=19) +
  geom_text(data = cor.res, aes(x = x, y = y, label = rho),
            size = 3)+
  theme(axis.title = element_blank(),
        panel.margin = unit(.1,"cm"))
resplot


###############
# Save as a pdf
pdf("Figures/Correlate/Migration.Indices.pdf", width=7.5, height=3) 
resplot
dev.off()






######################################################################
## plot percent in canada against age


# pcan v age data
dat.melt <- melt(dat)
age.df <- data.frame(x = rep(response$pcan, 3),
                          y = dat.melt[1:30,2],
                          grp = as.character(dat.melt[1:30,1]), stringsAsFactors = FALSE)
age.df$grp[age.df$grp == "mage"] <- "Mean Age"
age.df$grp[age.df$grp == "magecan"] <- "Mean Age in Canada"
age.df$grp[age.df$grp == "mla"] <- "Mean Length at Age 5"
age.df$sig <- "n"
age.df$sig[age.df$grp == "Mean Age in Canada"] <- "y"

# apply cor function for each group
age.cor <- as.data.frame.matrix(ddply(age.df, .(grp), cors))
age.cor$x <- ddply(age.df, .(grp), summarise, x = max(x))[2]*.8
age.cor$y <- ddply(age.df, .(grp), summarise, y = max(y))[2]*.95
age.cor$rho <- paste("r = ", age.cor$rho)


#plot
ageplot <- basePlot +
  facet_wrap(~grp, scale = "free")+
  geom_smooth(data =  age.df, aes(x = x, y = y, colour = sig), 
              method = "lm", se = FALSE, size = .5) +
  geom_point(data =  age.df, aes(x = x, y = y), size=1.5, pch=19) +
  geom_text(data = age.cor, aes(x = x, y = y, label = rho),size = 3) +
  scale_colour_manual(values=c("white","black"), guide = FALSE)+
  theme(axis.title = element_blank(),
        panel.margin = unit(.1,"cm"))
ageplot


###############
# Save as a pdf
pdf("Figures/Correlate/Migration.Age.Indices.pdf", width=7.5, height=3) 
ageplot
dev.off()




######################################################################
## plot percent in canada against stock indices


# pcan v dist data
dat.melt <- melt(dat)
stock.df <- data.frame(x = rep(response$pcan, 3),
                     y = dat.melt[31:60,2],
                     grp = as.character(dat.melt[31:60,1]), stringsAsFactors = FALSE)
stock.df$grp[stock.df$grp == "patch"] <- "Patchiness Index"
stock.df$grp[stock.df$grp == "aggr"] <- "Density Index"
stock.df$grp[stock.df$grp == "stock"] <- "Stock Biomass (kmt)"
stock.df$sig <- "n"
stock.df$sig[stock.df$grp == "Patchiness Index"] <- "y"

# apply cor function for each group
stock.cor <- as.data.frame.matrix(ddply(stock.df, .(grp), cors))
stock.cor$x <- ddply(stock.df, .(grp), summarise, x = max(x))[2]*.8
stock.cor$y <- ddply(stock.df, .(grp), summarise, y = max(y))[2]*.95
stock.cor$rho <- paste("r = ", stock.cor$rho)


#plot
stockplot <- basePlot +
  facet_wrap(~grp, scale = "free")+
  geom_smooth(data =  stock.df, aes(x = x, y = y, colour = sig), 
              method = "lm", se = FALSE, size = .5) +
  geom_point(data =  stock.df, aes(x = x, y = y), size=1.5, pch=19) +
  geom_text(data = stock.cor, aes(x = x, y = y, label = rho),size = 3) +
  scale_colour_manual(values=c("white","black"), guide = FALSE)+
  theme(axis.title = element_blank(),
        panel.margin = unit(.1,"cm"))
stockplot


###############
# Save as a pdf
pdf("Figures/Correlate/Migration.Stock.Indices.pdf", width=7.5, height=3) 
stockplot
dev.off()

