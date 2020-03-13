# Benoit figures
library(cowplot)
library(ggplot2)
library(reshape2)

#setwd("C:/Users/Daniel/Dropbox/Figures_de_Courson_Nettle/CSV/Fig.1")

###Figure 1#####
# Main panel 
d1A=read.csv("Figure1.csv")
#Need to just take the middle 501 columns and reverse rows
f1A=as.matrix(d1A[1:nrow(d1A) , 250: 750])
# Now melt and make a dataframe
fl1A=melt(f1A)
fl1A$t=(1-fl1A$Var1/(800))
fl1A$s=as.numeric(fl1A$Var2)/10 -25
fig1A=ggplot(fl1A, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                      labels = c("Exploit", "Forage alone", "Cooperate"), 
                    values=c("red", "grey", "blue"))
fig1A

png("fig1A.png", res=300, units="in", width=6, height=4)
fig1A
dev.off()

# Top left (efficiency of cooperation)
# Panel 1B
d1B=read.csv("Decisions1.csv")
#Need to just take the middle 501 columns and reverse rows
f1B=as.matrix(d1B[1:nrow(d1B) , 250: 750])
# Now melt and make a dataframe
fl1B=melt(f1B)
hist(fl1B$Var1)
fl1B$t=(1-fl1B$Var1/(400)) + 0
fl1B$s=as.numeric(fl1B$Var2)/10 -25
fig1B=ggplot(fl1B, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(alpha, " = 1.05")))
fig1B

# Panel 1C
d1C=read.csv("Decisions3.csv")
#Need to just take the middle 501 columns and reverse rows
f1C=as.matrix(d1C[1:nrow(d1C) , 250: 750])
# Now melt and make a dataframe
fl1C=melt(f1C)
hist(fl1C$Var1)
fl1C$t=(1-fl1C$Var1/(400)) + 0
fl1C$s=as.numeric(fl1C$Var2)/10 -25
fig1C=ggplot(fl1C, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(alpha, " = 1.30")))
fig1C

topleft=plot_grid(fig1B, fig1C)

#Top right (social mobility)
# Panel 1D
d1D=read.csv("Decisions5.csv")
#Need to just take the middle 501 columns and reverse rows
f1D=as.matrix(d1D[1:nrow(d1D) , 250: 750])
# Now melt and make a dataframe
fl1D=melt(f1D)
hist(fl1D$Var1)
fl1D$t=(1-fl1D$Var1/(400)) + 0
fl1D$s=as.numeric(fl1D$Var2)/10 -25
fig1D=ggplot(fl1D, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle("r = 0.5")
fig1D

# Panel 1E
d1E=read.csv("Decisions4.csv")
#Need to just take the middle 501 columns and reverse rows
f1E=as.matrix(d1E[1:nrow(d1E) , 250: 750])
# Now melt and make a dataframe
fl1E=melt(f1E)
hist(fl1E$Var1)
fl1E$t=(1-fl1E$Var1/(400)) + 0
fl1E$s=as.numeric(fl1E$Var2)/10 -25
fig1E=ggplot(fl1E, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle("r = 0.9")
fig1E

topright=plot_grid(fig1D, fig1E)
topright
png("redist.png", res=300, units="in", width=8, height=4)
topright
dev.off()

# Bottom left (severity of punishment)
# Panel 1F
d1F=read.csv("Decisions8.csv")
#Need to just take the middle 501 columns and reverse rows
f1F=as.matrix(d1F[1:nrow(d1F) , 250: 750])
# Now melt and make a dataframe
fl1F=melt(f1F)
hist(fl1F$Var1)
fl1F$t=(1-fl1F$Var1/(400)) + 0
fl1F$s=as.numeric(fl1F$Var2)/10 -25
fig1F=ggplot(fl1F, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(pi, " = 15")))
fig1F

# Panel 1G
d1G=read.csv("Decisions9.csv")
#Need to just take the middle 501 columns and reverse rows
f1G=as.matrix(d1G[1:nrow(d1G) , 250: 750])
# Now melt and make a dataframe
fl1G=melt(f1G)
fl1G$t=(1-fl1G$Var1/(400)) + 0
fl1G$s=as.numeric(fl1G$Var2)/10 -25
fig1G=ggplot(fl1G, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(pi, " = 20")))
fig1G

bottomleft=plot_grid(fig1F, fig1G)
bottomleft
png("sevpun.png", res=300, units="in", width=8, height=4)
bottomleft
dev.off()
# Bottom right, probability of punishment
# Panel 1H
d1H=read.csv("Decisions11.csv")
#Need to just take the middle 501 columns and reverse rows
f1H=as.matrix(d1H[1:nrow(d1H) , 250: 750])
# Now melt and make a dataframe
fl1H=melt(f1H)
fl1H$t=(1-fl1H$Var1/(400)) + 0
fl1H$s=as.numeric(fl1H$Var2)/10 -25
fig1H=ggplot(fl1H, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(gamma, " = 2/3")))
fig1H

# Panel 1J
d1J=read.csv("Decisions12.csv")
#Need to just take the middle 501 columns and reverse rows
f1J=as.matrix(d1J[1:nrow(d1J) , 250: 750])
# Now melt and make a dataframe
fl1J=melt(f1J)
fl1J$t=(1-fl1J$Var1/(400)) + 0
fl1J$s=as.numeric(fl1J$Var2)/10 -25
fig1J=ggplot(fl1J, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(gamma, " = 9/10")))
fig1J

bottomright=plot_grid(fig1H, fig1J)

fig1right=plot_grid(topleft, topright, bottomleft, bottomright, labels=c("B", "C", "D", "E"), scale=0.9)
fig1right

fig1=plot_grid(fig1A, fig1right, labels=c("A", ""), rel_widths =c(0.45, 0.55))

png("fig1.png",res=300, units="in", width=20, height=6)
fig1
dev.off()
