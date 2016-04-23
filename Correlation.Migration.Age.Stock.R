require(ggplot2)
require(plyr)
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

# correlation function
cors <- function(df){
 rho = round(cor.test(df$x,df$y)$estimate,2)
 p = round(cor.test(df$x,df$y)$p.value,3)
 return(cbind(rho,p))
}

# apply cor function for each group
cor.res <- as.data.frame.matrix(ddply(response.df, .(grp), cors))
cor.res$x <- ddply(response.df, .(grp), summarise, x = min(x))[2]*.8
cor.res$y <- ddply(response.df, .(grp), summarise, y = max(y))[2]*.95
cor.res$rho <- paste("r = ", cor.res$rho)



#plot
for (i in unique(response.df$grp)){
  tmp <- response.df[response.df$grp == i,]
  tmpcor <- cor.res[cor.res$grp == i,]
  xlab <- gsub(" -.*","",i)
  ylab <- gsub(".*- ","",i)
  name <- gsub(" |-","",i)
  resplot <- basePlot + geom_point(data =  tmp, aes(x = x, y = y), 
                                   size=1.3, pch=19)
    resplot <- resplot + geom_text(data = tmpcor, aes(x = x, y = y, label = rho),
                                   size = 3, hjust = 0)
    resplot <- resplot + geom_smooth(data = tmp, aes(x = x, y = y),
                                   colour = "black", se = F, method = "lm", size=.1)
    resplot <- resplot + theme(plot.margin = unit(c(.3,.2,.3,.2), "lines"),
                               axis.title.y= element_text(margin=margin(r = -2)))
    resplot <- resplot + labs(x=xlab,y=ylab)
  assign(name,resplot)
}

###############
# Save as a pdf
pdf("Figures/Correlate/Cor.Migration.Indices.pdf", width=7, height=2) 
grid.arrange(PercentTotal,PercentPercentAge5,TotalPercentAge5, nrow=1)
dev.off()






######################################################################
## plot percent in canada against age and stock indices


# pcan v other data
dat.melt <- melt(dat)
df <- data.frame(x = rep(response$pcan, 6),
                 y = dat.melt[,2],
                 grp = as.character(dat.melt[,1]), 
                 stringsAsFactors = FALSE)

#rename groups
df$grp[df$grp == "mage"] <- "Mean Age"
df$grp[df$grp == "magecan"] <- "Mean Age Canada"
df$grp[df$grp == "mla"] <- "Age-5 Mean Length"
df$grp[df$grp == "patch"] <- "Patchiness"
df$grp[df$grp == "aggr"] <- "Biomass Density"
df$grp[df$grp == "stock"] <- "Stock Biomass (kmt)"
df$grp <- factor(df$grp, levels=c("Mean Age","Mean Age Canada",
                                  "Age-5 Mean Length","Patchiness",
                                  "Biomass Density","Stock Biomass (kmt)"))

# apply cor function for each group
cor.df <- as.data.frame.matrix(ddply(df, .(grp), cors))
cor.df$x <- ddply(df, .(grp), summarise, x = max(x))[2]*.95
cor.df$y <- ddply(df, .(grp), summarise, y = max(y))[2]*.95
cor.df$rho <- paste("r = ", cor.df$rho)
cor.df$grp <- factor(cor.df$grp, levels=c("Mean Age","Mean Age Canada",
                                          "Age-5 Mean Length","Patchiness",
                                          "Biomass Density","Stock Biomass (kmt)"))

# show lm on plot yes/no
df$type <- "n"
df$type[df$grp == "Mean Age Canada"] <- "y"
df$type[df$grp == "Patchiness"] <- "y"


# bottom plots
predplot <- basePlot +
  facet_wrap(~grp, nrow=2, scale = "free_y")+
  geom_smooth(data = df, aes(x = x, y = y, colour = type),
              se = F, method = "lm", size=.1)+
  geom_point(data =  df, aes(x = x, y = y), 
             size=1.3, pch=19)+
  geom_text(data = cor.df, aes(x = x, y = y, label = rho),
            size = 3, hjust = 1)+
  labs(y="Indices",x="Biomass in Canada (%) anomaly")+
  scale_colour_manual(values = c("white","black"), guide = FALSE)+
  theme(panel.margin = unit(0, "lines"),
        axis.title.y= element_text(margin=margin(r = -1)))
predplot


###############
# Save as a pdf
pdf("Figures/Correlate/Age.Stock.Indices.pdf", width=6.5, height=4) 
predplot
dev.off()



