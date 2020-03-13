# Figure 3 script
library(ggplot2)
library(cowplot)
library(reshape2)
#setwd("C:/Users/ndn8/Dropbox/Figures_de_Courson_Nettle/CSV/Fig.3")

#First panel
d1=read.csv("heatmap1.csv", header=F)
d1A=melt(d1)
d1A$sigma=c(rep(1, times=5), rep(2, times=5), rep(3, times=5), rep(4, times=5), rep(5, times=5), rep(6, times=5))
d1A$pi=rep(c(5, 10, 15, 20, 25), times=6)
fig3A=ggplot(d1A, aes(x=sigma, y=pi, fill=factor(value))) + 
  labs(x=expression(paste("Inequality ", sigma)),y=expression(paste("Punishment  ", pi)), z="Equilibrium") + 
  geom_raster() + 
  scale_fill_manual(name="Equilibrium", 
                    labels = c("Non-cooperative", "Cooperative"), 
                    values=c("grey", "blue")) + 
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6)) + 
  scale_y_continuous(breaks=c(5, 10, 15, 20, 25)) + 
  guides(fill=F)
fig3A

#Second panel
d2=read.csv("heatmap2.csv", header=F)
d2A=melt(d2)
d2A$sigma=c(rep(1, times=5), rep(2, times=5), rep(3, times=5), rep(4, times=5), rep(5, times=5), rep(6, times=5))
d2A$r=rep(c(0.1, 0.3, 0.5, 0.7, 0.9), times=6)
fig3B=ggplot(d2A, aes(x=sigma, y=r, fill=factor(value))) + 
  labs(x=expression(paste("Inequality ", sigma)),y = "Social mobility r", z="Equilibrium") + 
  geom_raster() + 
  scale_fill_manual(name="Equilibrium", 
                    labels = c("Non-cooperative", "Cooperative"), 
                    values=c("grey", "blue")) + 
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6)) + 
  scale_y_continuous(breaks=c(0.1, 0.3, 0.5, 0.7, 0.9))
fig3B

# Assemble figure
fig3=plot_grid(fig3A, fig3B, rel_widths = c(0.42, 0.58), labels=c("A", "B"), scale=0.9)
png("figure3.png", res=300, units="in", width=12, height=5)
fig3
dev.off()