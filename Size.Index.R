require(ggplot2)
require(plyr)
source("base.plot.R")



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

# combine all age indices
mlaind <- rbind(mla,mlacan)
mlaind$grp <- c(rep("Total stock",nrow(mla)),rep("Canada",nrow(mlacan)))

# positive / negative label
mlaind$sign <- "p"
mlaind$sign[mlaind$anom <= 0] <- "n"


# strip labels
labstrp <- data.frame(grp = c("Canada","Total stock"),x = rep("2007",2), 
                      y = rep(2.7,2))


# plot size anomaly indices
sizeplot <- basePlot +
  facet_wrap(~grp, nrow = 1)+
  geom_bar(data = mlaind, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  geom_text(data = labstrp, aes(x = factor(x), y =  y, label = grp), 
            size = 3, hjust = 0)+
  labs(x = "", y = "Mean fork length (cm) at age 5 anomaly")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))+
  theme(strip.text = element_blank(),
        panel.margin = unit(.2, "lines"))
sizeplot


# Save as a pdf
pdf("Figures/Indices/LengthIndex_anomaly.pdf", width=7, height=3) 
sizeplot
dev.off()
