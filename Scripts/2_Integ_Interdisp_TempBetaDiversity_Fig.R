########################################
# Figures for temporal beta diversity ##
########################################
# integration & interdisciplinarity ####
########################################

require(ggplot2)
require(dplyr)
require(reshape2)
require(rms)
require(ggthemes)

###########
# Data ####
###########

tempbeta_integ<-read.csv("Cleaned_Data/TemporalBetaDiv_RAstopic.csv",stringsAsFactors = FALSE)
tempbeta_inter<-read.csv("Cleaned_Data/TemporalBetaDiv_topicRAs.csv",stringsAsFactors = FALSE)

########################
# 'integration' first  #
########################

# arrange data for figures

tempbeta_div<-melt(tempbeta_integ, id.vars=c("TBI","TimeWindow","ResAreas_Orig"),measure.vars = 2:4,value.name = "Value",variable.name = "BetaDiv_Components")

tempbeta_divv<-tempbeta_div %>%
               group_by(TimeWindow,TBI,BetaDiv_Components) %>%
               do(data.frame(rbind(smean.cl.boot(.$Value, B=1000))))

tempbeta_divv$TimeWindow<-as.character(tempbeta_divv$TimeWindow)
tempbeta_divv$TimeWindow<-ifelse(tempbeta_divv$TimeWindow=="Time1_2"," 1990/1995 -\n1996/2000",tempbeta_divv$TimeWindow)
tempbeta_divv$TimeWindow<-ifelse(tempbeta_divv$TimeWindow=="Time2_3"," 1996/2000 -\n2001/2005",tempbeta_divv$TimeWindow)
tempbeta_divv$TimeWindow<-ifelse(tempbeta_divv$TimeWindow=="Time3_4"," 2001/2005 -\n2006/2012",tempbeta_divv$TimeWindow)
tempbeta_divv$TimeWindow<-as.factor(tempbeta_divv$TimeWindow)

tempbeta_divv$TimeWindow <- factor(tempbeta_divv$TimeWindow, levels = c(" 1990/1995 -\n1996/2000"," 1996/2000 -\n2001/2005"," 2001/2005 -\n2006/2012"))

tempbeta_divv$BetaDiv_Components<-factor(tempbeta_divv$BetaDiv_Components,levels=c("D","C_den","B_den"))

tempbeta_divv$TBI<-as.factor(tempbeta_divv$TBI)
levels(tempbeta_divv$TBI)[levels(tempbeta_divv$TBI)=="Jaccard"] <- "a)          Pres./Abs. (Jaccard)"
levels(tempbeta_divv$TBI)[levels(tempbeta_divv$TBI)=="Ruzicka"]   <- "b)          Abundance (Ruzicka)"

# figure

bb<-ggplot(data=tempbeta_divv,aes(x=TimeWindow,y=Mean,group=BetaDiv_Components,colour=BetaDiv_Components,shape=BetaDiv_Components))+
  geom_point(size=2) +
  geom_errorbar(data=tempbeta_divv,aes(ymin=Lower,ymax=Upper),width=0.1)+
  geom_line(size=0.5)+
  #scale_colour_manual(name="",labels=c("B_den"="Losses","C_den"= "Gains","D"= expression(paste("Temporal ",beta))),
   #                   values = c("D"="#67a9cf","C_den" = "#ef8a62","B_den" ="gray40"))+
  scale_colour_colorblind(name="",
                     labels=c("B_den"="Losses","C_den"= "Gains","D"= expression(paste("Temporal ",beta))))+
  scale_shape_manual(name="", values=c("B_den"=4, "C_den"=17, "D"=19),
                     labels=c("B_den"="Losses","C_den"= "Gains","D"= expression(paste("Temporal ",beta))))+
  
  labs(x = "", y = expression(paste("Temporal ",beta," concept diversity")))+
  facet_wrap(.~TBI)

TempBetaDiv_integ<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
                                        axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                                        axis.text.y=element_text(colour="black",face="bold",size=8),
                                        axis.text.x=element_text(colour="black",face="bold",size=7),
                                        axis.ticks.y=element_line(colour="black"),
                                        legend.background=element_rect(fill="transparent"),
                                        legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                                        legend.text=element_text(size=9),
                                        legend.text.align = 0.5,
                                        legend.position="top",legend.direction="horizontal",
                                        strip.background = element_rect(fill="transparent"),
                                        strip.text = element_text(colour="black",face="bold",size=7,hjust=0),
                                        panel.background =element_rect(fill="transparent",colour="black"),
                                        panel.grid.minor=element_blank())

########################
# interdisciplinarity  #
########################

# arrange data for figures

tempbeta_interr<-melt(tempbeta_inter, id.vars=c("TBI","TimeWindow","Most_Probable_Topic"),measure.vars = 2:4,value.name = "Value",variable.name = "BetaDiv_Components")

tempbeta_interr<-tempbeta_interr %>%
  group_by(TimeWindow,TBI,BetaDiv_Components) %>%
  do(data.frame(rbind(smean.cl.boot(.$Value, B=1000))))

tempbeta_interr$TimeWindow<-as.character(tempbeta_interr$TimeWindow)
tempbeta_interr$TimeWindow<-ifelse(tempbeta_interr$TimeWindow=="Time1_2"," 1990/1995 -\n1996/2000",tempbeta_interr$TimeWindow)
tempbeta_interr$TimeWindow<-ifelse(tempbeta_interr$TimeWindow=="Time2_3"," 1996/2000 -\n2001/2005",tempbeta_interr$TimeWindow)
tempbeta_interr$TimeWindow<-ifelse(tempbeta_interr$TimeWindow=="Time3_4"," 2001/2005 -\n2006/2012",tempbeta_interr$TimeWindow)
tempbeta_interr$TimeWindow<-as.factor(tempbeta_interr$TimeWindow)

tempbeta_interr$TimeWindow <- factor(tempbeta_interr$TimeWindow, levels = c(" 1990/1995 -\n1996/2000"," 1996/2000 -\n2001/2005"," 2001/2005 -\n2006/2012"))

tempbeta_interr$BetaDiv_Components<-factor(tempbeta_interr$BetaDiv_Components,levels=c("D","C_den","B_den"))

tempbeta_interr$TBI<-as.factor(tempbeta_interr$TBI)
levels(tempbeta_interr$TBI)[levels(tempbeta_interr$TBI)=="Jaccard"] <- "c)          Pres./Abs. (Jaccard)"
levels(tempbeta_interr$TBI)[levels(tempbeta_interr$TBI)=="Ruzicka"]   <- "d)          Abundance (Ruzicka)"

# figure

cc<-ggplot(data=tempbeta_interr,aes(x=TimeWindow,y=Mean,group=BetaDiv_Components,colour=BetaDiv_Components,shape=BetaDiv_Components))+
  geom_point(size=2) +
  geom_errorbar(data=tempbeta_interr,aes(ymin=Lower,ymax=Upper),width=0.1)+
  geom_line(size=0.5)+
  #scale_colour_manual(name="",labels=c("B_den"="Losses","C_den"= "Gains","D"= expression(paste("Temporal ",beta))),
  #                   values = c("D"="#67a9cf","C_den" = "#ef8a62","B_den" ="gray40"))+
  scale_colour_colorblind(name="",
                          labels=c("B_den"="Losses","C_den"= "Gains","D"= expression(paste("Temporal ",beta))))+
  scale_shape_manual(name="", values=c("B_den"=4, "C_den"=17, "D"=19),
                          labels=c("B_den"="Losses","C_den"= "Gains","D"= expression(paste("Temporal ",beta))))+
  
  scale_y_continuous(limits=c(0.10, 0.81),breaks=c(0.2,0.4,0.6,0.8))+
  
  
  labs(x = "", y = expression(paste("Temporal ",beta," subdiscipline diversity")))+
  facet_wrap(.~TBI)

TempBetaDiv_inter<-cc+ theme_bw()+theme(axis.title.x=element_blank(),
                                  axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                                  axis.text.y=element_text(colour="black",face="bold",size=8),
                                  axis.text.x=element_text(colour="black",face="bold",size=7),
                                  axis.ticks.y=element_line(colour="black"),
                                  legend.background=element_rect(fill="transparent"),
                                  legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                                  legend.text=element_text(size=9),
                                  legend.text.align = 0.5,
                                  legend.position="top",legend.direction="horizontal",
                                  strip.background = element_rect(fill="transparent"),
                                  strip.text = element_text(colour="black",face="bold",size=7,hjust=0),
                                  panel.background =element_rect(fill="transparent",colour="black"),
                                  panel.grid.minor=element_blank())


########################
######################## 

require(cowplot)

beta_tog<-plot_grid(TempBetaDiv_integ+theme(axis.text.x=element_blank(),
                                            panel.background = element_blank(),
                                            panel.grid.major = element_blank()),
                   TempBetaDiv_inter +theme(legend.position="none",
                                            panel.background = element_blank(),
                                            panel.grid.major = element_blank()),
                   align="vh",ncol=1)


ggsave(filename = file.path("Figures", "Integr_Inter_TempBetaDiv.png"), 
    units="in", 
    width=6.5, 
    height=6.5)

beta_tog

dev.off()
