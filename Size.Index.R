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

# mean length at age 5
mla <- ddply(spec[spec$AGE == 5,], .(year), summarise, 
             mean = mean(LENGTH, na.rm=T),
             se = sd(LENGTH, na.rm=T)/sqrt(length(unique(HAUL)))) # number of trawls
# anomaly
mla$anom <- mla$mean - mean(mla$mean)

# save length at age 5 size index
save(mla,file="Data/A5.Length.Index.rda")

# positive / negative label
mla$sign <- "p"
mla$sign[mla$anom <= 0] <- "n"

# plot size anomaly indices
sizeplot <- basePlot +
  geom_bar(data = mla, aes(x = factor(year), y = anom, fill = factor(sign)),
           width = .7, show.legend = FALSE, stat = "identity")+
  geom_hline(yintercept = 0, size = .1, linetype = 1, colour= "black")+
  labs(x = "", y = "Mean fork length (cm) at age 5 anomaly")+
  scale_fill_manual(values = c("#377eb8","#e41a1c"))
sizeplot


# Save as a pdf
pdf("Figures/Indices/LengthIndex_anomaly.pdf", width=4, height=3) 
sizeplot
dev.off()

