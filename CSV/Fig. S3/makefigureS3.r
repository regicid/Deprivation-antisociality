# Script to make phase transition figure (figure S3)

library(ggplot2)
library(cowplot)
library(reshape2)
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="brown", geom=geom, size = 3, ...)
}
stat_sum_single_green <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="darkgreen", geom=geom, size = 3, ...)
}

d2=read.csv("Actions.csv")
figA=ggplot(d2, aes(x=Time, y=Frequency, fill=Action)) + 
  geom_col(width=1) + 
  theme_bw() + 
  xlab("Time step") + 
  ylab("Proportion of actions") + 
  scale_fill_manual(values=c("blue", "red", "grey")) + 
  geom_vline(xintercept=16)

figA

# Panel B
d2B=read.csv("Phase_transition_states.csv", header=F)
d2B$Time=1:length(d2B$V1)
d2Bl=melt(d2B, id.var="Time")

fig2B=ggplot(d2Bl, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Resources") + 
  stat_sum_single_green(mean) + 
  geom_vline(xintercept=16)
fig2B

# Panel C
d2C=read.csv("Phase_transition_trust.csv", header=F)
d2C$Time=1:length(d2C$V1)
d2Cl=melt(d2C, id.var="Time")

fig2C=ggplot(d2Cl, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Trust") + 
  stat_sum_single(mean) + 
  coord_cartesian(ylim=c(0, 1)) + 
  geom_vline(xintercept=16)
fig2C

extrasp=plot_grid(fig2C, fig2B, labels=c("B", "C"), ncol=1)

p=plot_grid(figA, extrasp, labels=c("A", ""), rel_widths = c(0.6, 0.4))

png("phasetransition.png", res=300, units="in", width=12, height=5)
p
dev.off()

pdf("phasetransition.pdf", width=12, height=5)
p
dev.off()
