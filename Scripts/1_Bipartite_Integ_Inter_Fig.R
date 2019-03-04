#####################################
# Figures for bi-partite analysis   #
# per 'trophic level', i.e. for     #
# topics and research areas         #
#####################################

require(ggplot2)
require(dplyr)
require(ggthemes)

##########
# Data   #
##########

nett<-read.csv("Cleaned_Data/group_bipartite_indices.csv",header=T)

nett$TimeWindow<-as.character(nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="1990_1995","1990 - 1995",nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="1996_2000","1996 - 2000",nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="2001_2005","2001 - 2005",nett$TimeWindow)
nett$TimeWindow<-ifelse(nett$TimeWindow=="2006_2012","2006 - 2012",nett$TimeWindow)
nett$TimeWindow<-as.factor(nett$TimeWindow)

nett$TimeWindow <- factor(nett$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))
nett$TrophicLevel <- factor(nett$TrophicLevel, levels = c("Most_Probable_Topic","ResArea_Orig"))

#################
# Niche overlap #
#################

ee<-ggplot(data=nett,aes(x=TimeWindow,y=niche.overlap,group=TrophicLevel, colour=TrophicLevel,shape=TrophicLevel))+
  geom_line(size=1)+
  geom_point(size=3)+
  scale_colour_colorblind(name="",labels=c("ResArea_Orig"="Subdisciplines","Most_Probable_Topic"= "Concepts"))+

  scale_shape_manual(name="",labels=c("ResArea_Orig"="Subdisciplines","Most_Probable_Topic"= "Concepts"),
                                      values=c("ResArea_Orig"=17, "Most_Probable_Topic"=19))+
  labs(x = "", y = "Niche overlap")

Niche<-ee+ theme_bw()+theme(axis.title.x=element_blank(),
                            axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                            axis.text.y=element_text(colour="black",face="bold",size=8),
                            axis.text.x=element_text(colour="black",face="bold",size=7),
                            axis.ticks.y=element_line(colour="transparent"),
                            legend.background=element_rect(fill="transparent"),
                            legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                            legend.text=element_text(size=9),
                            legend.text.align = 0.5,
                            legend.position="top",legend.direction="horizontal",
                            panel.background = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor=element_blank())

ggsave(filename = file.path("Figures", "Bipartite_Concept_Interdiscp.png"), 
       width    = 4, 
       height   = 4, 
       units    = "in")

Niche

dev.off()