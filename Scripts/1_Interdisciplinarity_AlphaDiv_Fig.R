#################################################
# Interdisciplinarity
#################################################
# Figures for alpha diversity (S, Spie, Sn)  #
#################################################

require(rms)
require(ggplot2)
require(dplyr)
require(tidyr)
require(reshape2)
require(ggthemes)

###########
# Data ####
###########

alpha_div<-read.csv("Cleaned_Data/interdisciplinarity_AlphaDiv_orders_mobr.csv",stringsAsFactors = FALSE)

# calculate means and bootstrapped CIs

alpha_divs<- alpha_div %>%
  group_by(TimeWindow, group) %>%
  do(data.frame(rbind(smean.cl.boot(.$Value, B=1000))))

# arrange data for figures

alpha_divs$TimeWindow<-as.character(alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="1990_1995","1990 - 1995",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="1996_2000","1996 - 2000",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="2001_2005","2001 - 2005",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="2006_2012","2006 - 2012",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-as.factor(alpha_divs$TimeWindow)
alpha_divs$TimeWindow <- factor(alpha_divs$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

alpha_divs<-filter(alpha_divs, group!="N" & group!="S_asymp")

Sn<-filter(alpha_divs, group=="S_n")

Alpha_divs2<-filter(alpha_divs, group=="S"|group=="S_PIE")

#######################
# Figure: S and Spie  #
#######################

Alpha_divs2$group<-as.factor(Alpha_divs2$group)
Alpha_divs2$group <- factor(Alpha_divs2$group, levels = c("S","S_PIE"))
Alpha_divs2$group<-droplevels(Alpha_divs2$group)
Alpha_divs2$group<-as.factor(Alpha_divs2$group)

pd <- position_dodge(width = 0.1)

bb<-ggplot(data=Alpha_divs2,aes(x=TimeWindow,y=Mean,group=group,colour=group,shape=group))+
  geom_point(size=2, position = pd) +
  geom_errorbar(data=Alpha_divs2,aes(ymin=Lower,ymax=Upper),width=0.1,position = pd)+
  geom_line(size=0.5,position = pd)+
  scale_colour_colorblind(name="",
                          labels=c("S"="Species Richness","S_PIE"= expression("ENS"[PIE])))+
  scale_shape_manual(name="", values=c("S"=17, "S_PIE"=19),
                     labels=c("S"="Species Richness","S_PIE"= expression("ENS"[PIE])))+
  labs(x = "", y = "Interdisciplinarity")

Inter_DivParts<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
                               axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                               axis.text.y=element_text(colour="black",face="bold",size=8),
                               axis.text.x=element_text(colour="black",face="bold",size=7),
                               axis.ticks.y=element_line(colour="transparent"),
                               legend.background=element_rect(fill="transparent"),
                               legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                               legend.text=element_text(size=9),
                               legend.text.align = 0.5,
                               legend.position=c(0.92,0.95),legend.direction="vertical",
                               panel.background =element_rect(fill="transparent",colour="black"),
                               panel.grid.minor=element_blank())
save(Inter_DivParts, file="Cleaned_Data/Interdisc_Div.RData")

####################################
# just rarefied species richness   #
####################################

cc<-ggplot(data=Sn,aes(x=TimeWindow,y=Mean,group=group))+
  geom_point(shape=20,size=2,colour="black") +
  geom_errorbar(data=Sn,aes(ymin=Lower,ymax=Upper),width=0.1, colour="black")+
  geom_line(size=0.5,colour="black")+
  labs(x = "", y = expression(paste("Interdisciplinarity (S"[25],")")))

DivParts_n<-cc+ theme_bw()+theme(axis.title.x=element_blank(),
                                 axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                                 axis.text.y=element_text(colour="black",face="bold",size=8),
                                 axis.text.x=element_text(colour="black",face="bold",size=7),
                                 axis.ticks.y=element_line(colour="transparent"),
                                 legend.background=element_rect(fill="transparent"),
                                 legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                                 legend.text=element_text(size=9),
                                 legend.text.align = 0.5,
                                 legend.position=c(0.92,0.95),legend.direction="vertical",
                                 panel.background =element_rect(fill="transparent",colour="black"),
                                 panel.grid.minor=element_blank(),
                                 plot.margin=unit(c(0.3,0.3,0.3,0.3), "cm"))

png(filename="Figures/Interdisp_Alpha_RarefiedRichness_final.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=4,  
    pointsize=2, 
    res=1000)

DivParts_n


dev.off()