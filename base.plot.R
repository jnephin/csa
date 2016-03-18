require(ggplot2)
require(grid)

basePlot <- ggplot(data = NULL) +
  theme(panel.border = element_rect(fill=NA, colour="black", size = .1),
        panel.background = element_rect(fill="white",colour="white"),
        strip.background = element_blank(),
        axis.ticks = element_line(colour="black", size = .1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text = element_text(size=8, colour = "black"),
        strip.text = element_text(size=9, colour = "black", vjust = .3),
        axis.title.x = element_text(size=9, colour = "black"),
        axis.title.y = element_text(size=9, colour = "black", vjust = .7),
        legend.text = element_text(size=8),
        legend.title = element_text(size=9, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.justification = c(0,1), legend.position = c(0,1), 
        plot.margin = unit(c(.5,.5,.5,.5), "lines")) 
# top, right, bottom, and left 