#################################################
# Alpha diversity/Integration
# sensitivity analysis
# Remove research areas with fewer than 25 articles
#################################################

require(rms)
require(ggplot2)
require(dplyr)
require(tidyr)
require(reshape2)
require(palettetown)

###########
# Data ####
###########

alpha_div<-read.csv("Cleaned_Data/integration_AlphaDiv_orders_mobr.csv",stringsAsFactors = FALSE)

alpha_div<-spread(alpha_div, group, Value, fill = NA, convert = FALSE)
alpha_divv<-filter(alpha_div, N>=50)

alpha_divv<-alpha_divv%>%
      unite("time_ResAreas", c("TimeWindow","ResArea_Orig"),sep = ":",remove=FALSE)

alpha_divv<-alpha_divv%>%
           gather(group, Value, N:S_PIE)

# calculate means and bootstrapped CIs

alpha_divs<- alpha_divv %>%
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

bb<-ggplot(data=Alpha_divs2,aes(x=TimeWindow,y=Mean,group=group,colour=group))+
  geom_point(shape=20,size=2) +
  geom_errorbar(data=Alpha_divs2,aes(ymin=Lower,ymax=Upper),width=0.1)+
  geom_line(size=0.5)+
  scale_colour_poke(pokemon = 42, spread = 2, name="",labels=c("S"="S","S_PIE"= expression("S"[PIE])))+
  labs(x = "", y = "Concept diversity")

DivParts<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
                               axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                               axis.text.y=element_text(colour="black",face="bold",size=8),
                               axis.text.x=element_text(colour="black",face="bold",size=7),
                               axis.ticks.y=element_line(colour="transparent"),
                               legend.background=element_rect(fill="transparent"),
                               legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                               legend.text=element_text(size=9),
                               legend.text.align = 0.5,
                               legend.position=c(0.92,0.95),legend.direction="vertical",
                               panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank(),
                               plot.margin=unit(c(0.3,0.3,0.3,0.3), "cm"))

png(filename="Figures/Integr_Alpha_morethan50.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=4,  
    pointsize=2, 
    res=1000)

DivParts


dev.off()

####################################
# just rarefied species richness   #
####################################

cc<-ggplot(data=Sn,aes(x=TimeWindow,y=Mean,group=group,colour=group))+
  geom_point(shape=20,size=2) +
  geom_errorbar(data=Sn,aes(ymin=Lower,ymax=Upper),width=0.1)+
  geom_line(size=0.5)+
  scale_colour_poke(pokemon = 42, spread = 1, name="",labels=c("S_n"= expression("S"[25])))+
  labs(x = "", y = "Concept diversity")

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
                                 panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank(),
                                 plot.margin=unit(c(0.3,0.3,0.3,0.3), "cm"))

png(filename="Figures/Integr_Alpha_RarefiedRichness_morethan50.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=4,  
    pointsize=2, 
    res=1000)

DivParts_n


dev.off()