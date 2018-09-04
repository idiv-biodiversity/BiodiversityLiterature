#####################################
# Figures for niche overlap         #
#####################################

require(ggplot2)
require(dplyr)
require(reshape2)

##########
# Data   #
##########

nett<-read.csv("Cleaned_Data/RAtopic_bipartite_indices.csv",header=T)

nett$TimeWindow<-as.character(nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="1990_1995","1990 - 1995",nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="1996_2000","1996 - 2000",nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="2001_2005","2001 - 2005",nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="2006_2012","2006 - 2012",nett$TimeWindow)
nett$TimeWindow<-as.factor(nett$TimeWindow)

nett$TimeWindow <- factor(nett$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))
nett$Index <- factor(nett$Index, levels = c("niche.overlap.LL","niche.overlap.HL"))

# make figure

ee<-ggplot(data=nett,aes(x=TimeWindow,y=obs,group=Index, colour=Index))+
  
  geom_line(size=1)+
  geom_point(shape=20,size=3)+
  scale_colour_manual(name="",labels=c("niche.overlap.HL"="Subdisciplines","niche.overlap.LL"= "Concepts"),
                      values = c("niche.overlap.HL"="#67a9cf","niche.overlap.LL" = "#ef8a62"))+
  labs(x = "", y = "Niche overlap")+scale_y_continuous(limits=c(0,0.73), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7))


Niche<-ee+ theme_bw()+theme(axis.title.x=element_blank(),
                            axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                            axis.text.y=element_text(colour="black",face="bold",size=8),
                            axis.text.x=element_text(colour="black",face="bold",size=7),
                            axis.ticks.y=element_line(colour="transparent"),
                            legend.background=element_rect(fill="transparent"),
                            legend.key=element_rect(fill="transparent"),
                            legend.position=c(0.12,0.98),legend.direction="vertical",
                            legend.text=element_text(size=6),
                            panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

png(filename="Figures/Integr_NicheOverlap.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=4,  
    pointsize=2, 
    res=1000)

Niche

dev.off()
