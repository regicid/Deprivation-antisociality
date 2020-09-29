# de Courson and Nettle figure 1
library(cowplot)
library(ggplot2)
library(reshape2)
theme_set(theme_bw())

###Figure 1#####
# Main panel 
d1A=read.csv("fig1A.csv")
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


# Panel 1B
d1B1=read.csv("fig1B1.csv")
#Need to just take the middle 501 columns and reverse rows
f1B1=as.matrix(d1B1[1:nrow(d1B1) , 250: 750])
# Now melt and make a dataframe
fl1B1=melt(f1B1)
fl1B1$t=(1-fl1B1$Var1/(400)) + 0
fl1B1$s=as.numeric(fl1B1$Var2)/10 -25
fig1B1=ggplot(fl1B1, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(alpha, " = 1.05")))
fig1B1
d1B2=read.csv("fig1B2.csv")
#Need to just take the middle 501 columns and reverse rows
f1B2=as.matrix(d1B2[1:nrow(d1B2) , 250: 750])
# Now melt and make a dataframe
fl1B2=melt(f1B2)
fl1B2$t=(1-fl1B2$Var1/(400)) + 0
fl1B2$s=as.numeric(fl1B2$Var2)/10 -25
fig1B2=ggplot(fl1B2, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(alpha, " = 1.30")))
fig1B2
topleft=plot_grid(fig1B1, fig1B2)
topleft

# Panel 1C
d1C1=read.csv("fig1C1.csv")
#Need to just take the middle 501 columns and reverse rows
f1C1=as.matrix(d1C1[1:nrow(d1C1) , 250: 750])
# Now melt and make a dataframe
fl1C1=melt(f1C1)
fl1C1$t=(1-fl1C1$Var1/(400)) + 0
fl1C1$s=as.numeric(fl1C1$Var2)/10 -25
fig1C1=ggplot(fl1C1, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle("r = 0.8")
fig1C1
d1C2=read.csv("fig1C2.csv")
#Need to just take the middle 501 columns and reverse rows
f1C2=as.matrix(d1C2[1:nrow(d1C2) , 250: 750])
# Now melt and make a dataframe
fl1C2=melt(f1C2)
fl1C2$t=(1-fl1C2$Var1/(400)) + 0
fl1C2$s=as.numeric(fl1C2$Var2)/10 -25
fig1C2=ggplot(fl1C2, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle("r = 1.0")
fig1C2

topright=plot_grid(fig1C1, fig1C2)
topright

# Bottom left (severity of punishment)
d1d1=read.csv("fig1D1.csv")
#Need to just take the middle 501 columns and reverse rows
f1d1=as.matrix(d1d1[1:nrow(d1d1) , 250: 750])
# Now melt and make a dataframe
fl1d1=melt(f1d1)
fl1d1$t=(1-fl1d1$Var1/(400)) + 0
fl1d1$s=as.numeric(fl1d1$Var2)/10 -25
fig1d1=ggplot(fl1d1, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(pi, " = 15")))
fig1d1
d1d2=read.csv("fig1D2.csv")
#Need to just take the middle 501 columns and reverse rows
f1d2=as.matrix(d1d2[1:nrow(d1d2) , 250: 750])
# Now melt and make a dataframe
fl1d2=melt(f1d2)
fl1d2$t=(1-fl1d2$Var1/(400)) + 0
fl1d2$s=as.numeric(fl1d2$Var2)/10 -25
fig1d2=ggplot(fl1d2, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(pi, " = 20")))
fig1d2
bottomleft=plot_grid(fig1d1, fig1d2)
bottomleft

# Bottom right, probability of punishment
d1e1=read.csv("fig1E1.csv")
#Need to just take the middle 501 columns and reverse rows
f1e1=as.matrix(d1e1[1:nrow(d1e1) , 250: 750])
# Now melt and make a dataframe
fl1e1=melt(f1e1)
fl1e1$t=(1-fl1e1$Var1/(400)) + 0
fl1e1$s=as.numeric(fl1e1$Var2)/10 -25
fig1e1=ggplot(fl1e1, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(gamma, " = 2/3")))
fig1e1

d1e2=read.csv("fig1E2.csv")
#Need to just take the middle 501 columns and reverse rows
f1e2=as.matrix(d1e2[1:nrow(d1e2) , 250: 750])
# Now melt and make a dataframe
fl1e2=melt(f1e2)
fl1e2$t=(1-fl1e2$Var1/(400)) + 0
fl1e2$s=as.numeric(fl1e2$Var2)/10 -25
fig1e2=ggplot(fl1e2, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Cooperate", "Forage alone"), 
                    values=c("red", "grey", "blue")) + 
  guides(fill=FALSE)  +
  ggtitle(expression(paste(gamma, " = 9/10")))
fig1e2

bottomright=plot_grid(fig1e1, fig1e2)
bottomright

fig1right=plot_grid(topleft, topright, bottomleft, bottomright, labels=c("B", "C", "D", "E"), scale=0.9)
fig1right

fig1=plot_grid(fig1A, fig1right, labels=c("A", ""), rel_widths =c(0.45, 0.55))

png("fig1.png",res=300, units="in", width=20, height=6)
fig1
dev.off()
