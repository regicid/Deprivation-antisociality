# Script to make figure 2
library(ggplot2)
library(cowplot)
library(reshape2)

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="brown", geom=geom, size = 3, ...)
}
stat_sum_single_green <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="darkgreen", geom=geom, size = 3, ...)
}

# Panels A
d2A=read.csv("simulationA_actions.csv")
fig2A1=ggplot(d2A, aes(x=Time, y=Frequency, fill=Action)) + 
  geom_col(width=1) + 
  theme_bw() + 
  xlab("Time step") + 
  ylab("Proportion of actions") + 
  scale_fill_manual(values=c("blue", "red", "grey"))
fig2A1

# Panel A2
d2A2=read.csv("simulationA_trust.csv", header=F)
d2A2$Time=1:length(d2A2$V1)
d2A2=melt(d2A2, id.var="Time")

fig2A2=ggplot(d2A2, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Trust") + 
  stat_sum_single(mean) + 
  coord_cartesian(ylim=c(0, 1))
fig2A2

# Panel A3
d2A3=read.csv("simulationA_states.csv", header=F)
d2A3$Time=1:length(d2A3$V1)
d2A3=melt(d2A3, id.var="Time")

fig2A3=ggplot(d2A3, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Resources") + 
  stat_sum_single_green(mean)
fig2A3

# Assemble panel A
extrasA=plot_grid(fig2A2, fig2A3, labels=c("A2", "A3"), ncol=1, hjust = -0.5, vjust = -0.5)
fig2A=plot_grid(fig2A1, extrasA, labels=c("A1", ""), rel_widths = c(0.6, 0.4), hjust = -0.5, vjust = -0.5)
fig2A

# Panels B
d2B=read.csv("simulationB_actions.csv")
fig2B1=ggplot(d2B, aes(x=Time, y=Frequency, fill=Action)) + 
  geom_col(width=1) + 
  theme_bw() + 
  xlab("Time step") + 
  ylab("Proportion of actions") + 
  scale_fill_manual(values=c("blue", "red", "grey"))
fig2B1

# Panel B2
d2B2=read.csv("simulationB_trust.csv", header=F)
d2B2$Time=1:length(d2B2$V1)
d2B2=melt(d2B2, id.var="Time")

fig2B2=ggplot(d2B2, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Trust") + 
  stat_sum_single(mean) + 
  coord_cartesian(ylim=c(0, 1))
fig2B2

# Panel A3
d2B3=read.csv("simulationB_states.csv", header=F)
d2B3$Time=1:length(d2B3$V1)
d2B3=melt(d2B3, id.var="Time")

fig2B3=ggplot(d2B3, aes(x=Time, y=value)) + 
  geom_violin(aes(group=Time), fill="lightgrey") + 
  xlab("Time step") +
  ylab("Resources") + 
  stat_sum_single_green(mean)
fig2B3

# Assemble panel B
extrasB=plot_grid(fig2B2, fig2B3, labels=c("B2", "B3"), hjust = -0.5, vjust = -0.5, ncol=1)
fig2B=plot_grid(fig2B1, extrasB, labels=c("B1", ""), hjust = -0.5, vjust = -0.5, rel_widths = c(0.6, 0.4))
fig2B


png("figure2.png", res=300, units="in", width=10, height=8)
plot_grid(fig2A, fig2B, ncol=1, scale=0.85) 
dev.off()