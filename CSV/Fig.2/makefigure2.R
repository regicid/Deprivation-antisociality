# Script to make figure 2
library(ggplot2)
library(cowplot)
library(reshape2)

#setwd("C:/Users/Daniel/Dropbox/Figures_de_Courson_Nettle/CSV/Fig.2")
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="brown", geom=geom, size = 3, ...)
}
stat_sum_single_green <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="darkgreen", geom=geom, size = 3, ...)
}

# Virtuous circle
d2A=read.csv("Actionsv.csv")
fig2A=ggplot(d2A, aes(x=Time, y=Freq, fill=Action)) + 
  geom_col(width=1) + 
  theme_bw() + 
  xlab("Time step") + 
  ylab("Proportion of actions") + 
  scale_fill_manual(values=c("blue", "red", "grey"))
fig2A
# Panel B
d2B=read.csv("Coop_states.csv", header=F)
d2B$Time=1:length(d2B$V1)
d2Bl=melt(d2B, id.var="Time")

fig2B=ggplot(d2Bl, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Resources") + 
  stat_sum_single_green(mean)
fig2B

# Panel C
d2C=read.csv("Coop_trust.csv", header=F)
d2C$Time=1:length(d2C$V1)
d2Cl=melt(d2C, id.var="Time")

fig2C=ggplot(d2Cl, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Trust") + 
  stat_sum_single(mean) + 
  coord_cartesian(ylim=c(0, 1))
fig2C

extrasv=plot_grid(fig2C, fig2B, labels=c("B2", "B3"), ncol=1)

v=plot_grid(fig2A, extrasv, labels=c("B1", ""), rel_widths = c(0.6, 0.4))

# Poverty trap
d22A=read.csv("Actionsp.csv")
fig22A=ggplot(d22A, aes(x=Time, y=Frequency, fill=Action)) + 
  geom_col(width=1) + 
  theme_bw() + 
  xlab("Time step") + 
  ylab("Proportion of actions") + 
  scale_fill_manual(values=c("blue", "red", "grey"))
fig22A
# Panel B
d22B=read.csv("Poverty_trap_states.csv", header=F)
d22B$Time=1:length(d22B$V1)
d22Bl=melt(d22B, id.var="Time")

fig22B=ggplot(d22Bl, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Resources") + 
  stat_sum_single_green(mean)
fig22B

# Panel C
d22C=read.csv("Poverty_trap_trust.csv", header=F)
d22C$Time=1:length(d22C$V1)
d22Cl=melt(d22C, id.var="Time")

fig22C=ggplot(d22Cl, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Trust") + 
  stat_sum_single(mean) + 
  coord_cartesian(ylim=c(0, 1))
fig22C

extrasp=plot_grid(fig22C, fig22B, labels=c("A2", "A3"), ncol=1)

p=plot_grid(fig22A, extrasp, labels=c("A1", ""), rel_widths = c(0.6, 0.4))

png("figure2.png", res=300, units="in", width=12, height=10)
plot_grid(p, v, ncol=1, scale=0.9) 
dev.off()