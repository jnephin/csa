require(ggplot2)
require(plyr)
require(reshape2)
require(gridExtra)
source("base.plot.R")

#load age length key
load("Data/Age.Length.Key.rda") #alk

#load length by strata data
load("Data/Length.Strata.rda") #mlen





############################################################################
############################################################################

# --------------------------     Age Index    ----------------------------#

############################################################################
############################################################################




############################################################################
############################################################################
# apply age length key to morpho data

# remove records with zero counts
mlen <- mlen[!mlen$count == 0,]

# compare lengths
table(mlen$LENGTH)
row.names(alk)

# age length key missing lengths between 4 are 13
# add age 0 for lengths 4 to 13
vec <- c(1,rep(0,21))
add <- NULL
for(i in 1:8){add <- rbind(add,vec)}
colnames(add) <- c(0:20,22)
row.names(add) <- c(4,7:13)
alk <- rbind(add,alk)

# age length key missing length 78
# get average for 77 and 78
add78 <- t(as.data.frame(apply(alk[73:74,], 2, mean)))
row.names(add78) <- 78
alk <- rbind(alk, add78)


# merge - don't inlcude age = 0)
aged <- merge(mlen, alk[,-1],  by.x = "LENGTH", by.y = "row.names")

# counts per age class
mult <- aged$count * aged[,5:25]
aged_counts <- cbind(aged[,2:3], mult)



############################################################################
############################################################################
# calculate the proportion of ages in each strata

# sum aged counts by strata 
agestrata <- ddply(aged_counts[,-2], .(strata), numcolwise(sum))

# melt
meltstrata <- melt(agestrata, id = "strata")
meltstrata$age <- as.numeric(as.character(meltstrata$variable))


# proportion of each age class by strata
perstrata <- ddply(meltstrata, .(strata), transform, 
                  percent = value/sum(value))[-2]

# mean age per strata
perstrata$mean <- perstrata$age * perstrata$percent
meanage_strata <- aggregate(mean ~ strata, sum, data = perstrata)


# load strata and merge for strata names
strata <- read.csv("Data/Strata.csv",header=T)
perstrata <- merge(perstrata, strata, by = "strata")
perstrata$age <- as.numeric(perstrata$age)
perstrata$name <- factor(perstrata$name, 
                        levels =c("Monterey","Eureka","South Columbia",
                                  "North Columbia","Vancouver","Haida"))




############################################################################
############################################################################
# plot distribution of ages by strata

#colours
cbPalette <- c("#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2",  "#CC79A7")


# plot age distribution by strata
stplot <- basePlot +
  geom_line(data = perstrata, 
            aes(x = age, y = percent*100,colour = name, group = name),
            size = .7)+
  labs(x = "Age", y = "Frequency (%)")+
  scale_y_continuous(expand = c(0,0), limits = c(-.5,47))+
  scale_x_continuous(expand = c(0,0), breaks = seq(2,20,2), limits = c(1,18))+
  scale_colour_manual(values = cbPalette, name = "", guide = 
                        guide_legend(direction = "vertical", 
                                     keywidth = .8,  keyheight = .8,
                                     label.position = "right",
                                     override.aes = list(size = 1)))+ 
  theme(legend.justification = c(1,1.04), legend.position = c(1,1.04))
stplot


###############
# Save as a pdf
pdf("Figures/Ages/FreqAge.Curves.Strata.pdf", width=4, height=2.5) 
stplot
dev.off()






############################################################################
############################################################################
# calculate the mean ages in each year


# sum aged counts by year 
ageyr <- ddply(aged_counts[,-1], .(year), numcolwise(sum))

# melt
meltyr <- melt(ageyr, id = "year")
meltyr$age <- as.numeric(as.character(meltyr$variable))

# proportion of each age class by year
peryr <- ddply(meltyr, .(year), transform, 
                 percent = value/sum(value))

# mean age per year
peryr$mean <- peryr$age * peryr$percent
meanage <- aggregate(mean ~ year, sum, data = peryr)
meanage$anom <- meanage$mean - mean(meanage$mean)

# save mean age index
save(meanage, file = "Data/Mean.Age.rda")


####----------------------------------------------####


# sum aged counts by year 
ageyrus <- ddply(aged_counts[aged_counts$strata %in% 1:4,-1], .(year), numcolwise(sum))

# melt
meltyrus <- melt(ageyrus, id = "year")
meltyrus$age <- as.numeric(as.character(meltyrus$variable))

# proportion of each age class by year
peryrus <- ddply(meltyrus, .(year), transform, 
               percent = value/sum(value))

# mean age per year
peryrus$mean <- peryrus$age * peryrus$percent
meanageus <- aggregate(mean ~ year, sum, data = peryrus)
meanageus$anom <- meanageus$mean - mean(meanageus$mean)

# save mean age index
save(meanageus, file = "Data/Mean.Age.US.rda")


####----------------------------------------------####

# sum aged counts by year in canada
ageyrcan <- ddply(aged_counts[aged_counts$strata %in% 5:6,-1], .(year), numcolwise(sum))

# melt
meltyrcan <- melt(ageyrcan, id = "year")
meltyrcan$age <- as.numeric(as.character(meltyrcan$variable))

# proportion of each age class by year in canada
peryrcan <- ddply(meltyrcan, .(year), transform, 
               percent = value/sum(value))

# mean age per strata
peryrcan$mean <- peryrcan$age * peryrcan$percent
meanagecan <- aggregate(mean ~ year, sum, data = peryrcan)
meanagecan$anom <- meanagecan$mean - mean(meanagecan$mean)

# save mean age index
save(meanagecan, file = "Data/Mean.Age.Can.rda")











############################################################################
############################################################################

# --------------------------     Size Index   ----------------------------#

############################################################################
############################################################################




############################################################################
############################################################################

# load specimen age and length data
data.list <- list.files(path = "Data/biological", pattern = "specimen*")
spec <- NULL
for(i in data.list){
  tmp <- read.csv(file.path("Data/biological",i),header=T)
  tmp <- tmp[,c("HAUL","SPECIES_CODE","SEX","LENGTH","WEIGHT","AGE")]
  name <- sub(".csv", "", i)
  id <- strsplit(name, "_")[[1]]
  tmp$year <- id[2]
  spec <- rbind(spec, tmp)
}

# remove all non hake species (hake species code: 22500)
spec <- spec[spec$SPECIES_CODE == 22500,]

# remove NA age's
spec <- spec[!is.na(spec$AGE),]

# round lengths to the nearest centimeter
spec$LENGTH <- round(spec$LENGTH)

# save specimen data
save(spec, file = "Data/Specimen.rda")


############################################################################
############################################################################
# size index 

# load haul strata data
load("Data/Haul.Strata.rda") #haul

# merge specimen and haul by strata data
xyspec <- merge(spec, haul, by = c("HAUL","year"), all.x = T)

# group by country
xyspec$grp <- "US"
xyspec$grp[xyspec$strata %in% 5:6] <- "Canada"


# mean length at age 5
mla <- ddply(spec[spec$AGE == 5,], .(year), summarise, 
             mean = mean(LENGTH, na.rm=T),
             se = sd(LENGTH, na.rm=T)/sqrt(length(unique(HAUL)))) # of trawls
mlacan <- ddply(xyspec[xyspec$AGE == 5,], .(year, grp), summarise, 
                mean = mean(LENGTH, na.rm=T),
                se = sd(LENGTH, na.rm=T)/sqrt(length(unique(HAUL)))) # of trawls
mlacan <- mlacan[mlacan$grp == "Canada",-2]

# anomaly
mla$anom <- mla$mean - mean(mla$mean)
mlacan$anom <- mlacan$mean - mean(mlacan$mean)

# save length at age 5 size index
save(mla,file="Data/A5.Length.Index.rda")




############################################################################
############################################################################

# --------------------------     Plot Index    ----------------------------#

############################################################################
############################################################################




############################################################################
# age indices

# combine all age indices
ageind <- rbind(meanage,meanagecan)
ageind$grp <- c(rep("Total stock",nrow(meanage)),rep("Canada",nrow(meanagecan)))

# positive / negative label
ageind$sign <- "p"
ageind$sign[ageind$anom <= 0] <- "n"

# plot age anomaly indices
aplot <- basePlot +
  facet_wrap(~grp, nrow = 1)+
  geom_bar(data = ageind, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  labs(x = "", y = "Mean age anomaly")+
  scale_y_continuous(expand=c(0.02,0))+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        panel.margin = unit(.2, "lines"),
        plot.margin = unit(c(.2,.2,0,.2), "lines"))
aplot


############################################################################
# size indices

# combine all age indices
mlaind <- rbind(mla,mlacan)
mlaind$grp <- c(rep("Total stock",nrow(mla)),rep("Canada",nrow(mlacan)))

# positive / negative label
mlaind$sign <- "p"
mlaind$sign[mlaind$anom <= 0] <- "n"

# x axis
yrs <- c("98","01","03","05","07","09","11","12","13","15")


# plot size anomaly indices
sizeplot <- basePlot +
  facet_wrap(~grp, nrow = 1)+
  geom_bar(data = mlaind, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  labs(x = "Years", y = "Age-5 fork length anomaly")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  scale_x_discrete(labels = yrs)+
  scale_y_continuous(expand=c(0.02,0))+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"),
        plot.margin = unit(c(.1,.2,.2,.2), "lines"))
sizeplot


# Save as a pdf
pdf("Figures/Indices/AgeSizeIndex_anomaly.pdf", width=4.8, height=4.3) 
grid.arrange(aplot,sizeplot)
dev.off()








############################################################################
############################################################################

# ----------------------   Age Distribution Plots    ----------------------#

############################################################################
############################################################################




############################################################################
############################################################################
# plot distribution of ages by year


# convert age frequencies counts into age raw counts
rages <- NULL
for (j in 1:nrow(meltyr)){
  if(round(meltyr$value[j]) > 0){
    df <- data.frame(Age = rep(meltyr$age[j], round(meltyr$value[j])))
    df$year <- meltyr$year[j]
    rages <- rbind(rages, df)
  }
}


# convert age frequencies counts into age raw counts for canada
ragescan <- NULL
for (j in 1:nrow(meltyrcan)){
  if(round(meltyrcan$value[j]) > 0){
    df <- data.frame(Age = rep(meltyrcan$age[j], round(meltyrcan$value[j])))
    df$year <- meltyrcan$year[j]
    ragescan <- rbind(ragescan, df)
  }
}


#colours
pal <- c("#0072B2", "#56B4E9","#FFA200", "#F0E442", "#DE3335", "#F7819B", "#984ea3", "#B68FC9",  "#0D851C","#46B361","#b15928")


# plot age distribution by year
yrplot <- basePlot +
  geom_jitter(data = rages,
              aes(x =  factor(year), y = Age),
              size = .1, width = .8, pch=1, colour = "grey80")+
  geom_point(data = meanage,
             aes(x =  factor(year), y = mean),
             size = 2,  pch=16, colour = "red")+
  geom_violin(data = rages, show.legend = FALSE, adjust = 3,
              aes(x =  factor(year), y = Age), scale = 'width',
              size = .5, fill = NA, colour = "grey20", width = .8)+
  labs(x = "Year", y = "Age")+
  scale_y_continuous(expand = c(0,0), limits = c(0.5,15))
yrplot


###############
# Save as a pdf
pdf("Figures/Ages/FreqAge.Violin.pdf", width=6, height=4) 
yrplot
dev.off()


# plot bubbles
yrbubble <- basePlot +
  geom_point(data = peryr, show.legend = FALSE,
             aes(x =  factor(year), y = age, size = value),
             pch=1, colour = "black")+
  geom_point(data = meanage,
             aes(x =  factor(year), y = mean),
             size = 2,  pch=16, colour = "red")+
  scale_size_area(max_size = 8)+
  labs(x = "Year", y = "Age")
yrbubble

###############
# Save as a pdf
pdf("Figures/Ages/FreqAge.Bubble.pdf", width=6, height=5) 
yrbubble
dev.off()




# plot age distribution by year in canada
yrcanplot <- basePlot +
  geom_jitter(data = ragescan,
              aes(x =  factor(year), y = Age),
              size = .1, width = .8, pch=1, colour = "grey80")+
  geom_point(data = meanagecan,
             aes(x =  factor(year), y = mean),
             size = 2,  pch=16, colour = "red")+
  geom_violin(data = ragescan, adjust = 3,
              aes(x =  factor(year), y = Age), scale = 'width',
              size = .5, fill = NA, colour = "grey20", width = .8)+
  labs(x = "Year", y = "Age")+
  scale_y_continuous(expand = c(0,0), limits = c(0.5,20))+
  scale_colour_manual(values = pal, name = "")+ 
  theme(legend.justification = c(1,1.04), legend.position = c(1,1.04))
yrcanplot


###############
# Save as a pdf
pdf("Figures/Ages/FreqAge.Can.Violin.pdf", width=6, height=4) 
yrcanplot
dev.off()



# plot bubbles in canada
yrbubblecan <- basePlot +
  geom_point(data = peryrcan, show.legend = FALSE,
             aes(x =  factor(year), y = age, size = value),
             pch=1, colour = "black")+
  geom_point(data = meanagecan,
             aes(x =  factor(year), y = mean),
             size = 2,  pch=16, colour = "red")+
  scale_size_area(max_size = 8)+
  labs(x = "Year", y = "Age")
yrbubblecan

###############
# Save as a pdf
pdf("Figures/Ages/FreqAge.Can.Bubble.pdf", width=6, height=5) 
yrbubblecan
dev.off()

