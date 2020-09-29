# de Courson and Nettle figure S3
library(cowplot)
library(ggplot2)
library(reshape2)
theme_set(theme_bw())

###Figure 1#####
# Main panel 
dS3=read.csv("figS3.csv")
#Need to just take the middle 501 columns and reverse rows
fS3=as.matrix(dS3[1:nrow(dS3) , 250: 750])
# Now melt and make a dataframe
flS3=melt(fS3)
flS3$t=(1-flS3$Var1/(800))
flS3$s=as.numeric(flS3$Var2)/10 -25
figS3=ggplot(flS3, aes(x=s, y=t, fill=factor(value))) + 
  labs(x="Resources (s)",y="Trustworthiness (1-p)", z="Action") + 
  geom_raster() + 
  scale_fill_manual(name="Optimal action", 
                    labels = c("Exploit", "Forage alone", "Cooperate"), 
                    values=c("red", "grey", "blue"))
figS3
pdf("figureS3.pdf", width=7, height=5)
figS3
dev.off()
