# Figure 3 script
library(ggplot2)
library(cowplot)
theme_set(theme_bw())

#Panel A: Severity of punishment
d1A=read.csv("figure3Adata.csv")
fig3A=ggplot(d1A, aes(x=sigma, y=pi, fill=factor(value))) + 
  labs(x=expression(paste("Inequality ", sigma)),y=expression(paste("Punishment severity ", pi)), z="Equilibrium") + 
  geom_raster() + 
  scale_fill_manual(name="Equilibrium", 
                    labels = c("Non-cooperative", "Cooperative"), 
                    values=c("grey", "blue")) + 
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6)) + 
  scale_y_continuous(breaks=c(5, 10, 15, 20, 25)) + 
  guides(fill=F)
fig3A

#Panel B: Probability of punishment
d1B=read.csv("figure3Bdata.csv")
fig3B=ggplot(d1B, aes(x=sigma, y=gamma, fill=factor(value))) + 
  labs(x=expression(paste("Inequality ", sigma)),y=expression(paste("Punishment probability ", gamma)), z="Equilibrium") + 
  geom_raster() + 
  scale_fill_manual(name="Equilibrium", 
                    labels = c("Non-cooperative", "Cooperative"), 
                    values=c("grey", "blue")) + 
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6)) + 
  scale_y_continuous(breaks=c(0.1, 0.3, 0.5, 0.7, 0.9)) + 
  guides(fill=F)
fig3B

# Panel C: Social mobility
d3C=read.csv(file="figure3Cdata.csv")
fig3C=ggplot(d3C, aes(x=sigma, y=r, fill=factor(value))) + 
  labs(x=expression(paste("Inequality ", sigma)),y = "Social mobility r", z="Equilibrium") + 
  geom_raster() + 
  scale_fill_manual(name="Equilibrium", 
                    labels = c("Non-cooperative", "Cooperative"), 
                    values=c("grey", "blue")) + 
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6)) + 
  scale_y_continuous(breaks=c(0.1, 0.3, 0.5, 0.7, 0.9))
fig3C

# Assemble figure
fig3=plot_grid(fig3A, fig3B, fig3C, ncol=3, rel_widths = c(0.29, 0.29, 0.42), labels=c("A", "B", "C"), scale=0.9)
png("figure3.png", res=300, units="in", width=14, height=4)
fig3
dev.off()