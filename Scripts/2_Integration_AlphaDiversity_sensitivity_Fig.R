#################################################
# Alpha diversity/Integration
# sensitivity analysis
# Remove research areas with fewer than 25 articles
# Concept and subdisdiscipline diversity 
#################################################

require(rms)
require(ggplot2)
require(dplyr)
require(tidyr)
require(reshape2)
require(ggthemes)

#######################
# 1. Concept diversity#
#######################

alpha_div<-read.csv("Cleaned_Data/integration_AlphaDiv_orders_mobr.csv",stringsAsFactors = FALSE)

alpha_div<-spread(alpha_div, group, Value, fill = NA, convert = FALSE)
alpha_divv<-filter(alpha_div, N>=25) 

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

pd <- position_dodge(width = 0.1)

bb<-ggplot(data=Alpha_divs2,aes(x=TimeWindow,y=Mean,group=group,colour=group,shape=group))+
  geom_point(size=2, position = pd) +
  geom_errorbar(data=Alpha_divs2,aes(ymin=Lower,ymax=Upper),width=0.1,position = pd)+
  geom_line(size=0.5,position = pd)+
  #scale_colour_poke(pokemon = 19, spread = 2, name="",labels=c("S"="S","S_PIE"= expression("S"[PIE])))+
  #scale_colour_grey(name="",labels=c("S"="S","S_PIE"= expression("S"[PIE])), n=2, start=0.5, end=0.9)+
  scale_colour_colorblind(name="",labels=c("S"="Species Richness","S_PIE"= expression("ENS"[PIE])))+
  scale_shape_manual(name="",labels=c("S"="Species Richness","S_PIE"= expression("ENS"[PIE])), values=c("S"=17, "S_PIE"=19))+
  labs(x = "", y = "Concept diversity")

DivParts_conc<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
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

##################################
### 2. Subdiscipline diversity   #
##################################

alpha_div_sub<-read.csv("Cleaned_Data/interdisciplinarity_AlphaDiv_orders_mobr.csv",stringsAsFactors = FALSE)

alpha_div_subb<-spread(alpha_div_sub, group, Value, fill = NA, convert = FALSE)
alpha_div_subb<-filter(alpha_div_subb, N>=25)

alpha_div_subb<-alpha_div_subb%>%
  unite("time_Topic", c("TimeWindow","Most_Probable_Topic"),sep = ":",remove=FALSE)

alpha_div_subb<-alpha_div_subb%>%
  gather(group, Value, N:S_PIE)

# calculate means and bootstrapped CIs

alpha_divs_subbb <- alpha_div_subb %>%
  group_by(TimeWindow, group) %>%
  do(data.frame(rbind(smean.cl.boot(.$Value, B=1000))))

# arrange data for figures

alpha_divs_subbb$TimeWindow<-as.character(alpha_divs_subbb$TimeWindow)
alpha_divs_subbb$TimeWindow<-ifelse(alpha_divs_subbb$TimeWindow=="1990_1995","1990 - 1995",alpha_divs_subbb$TimeWindow)
alpha_divs_subbb$TimeWindow<-ifelse(alpha_divs_subbb$TimeWindow=="1996_2000","1996 - 2000",alpha_divs_subbb$TimeWindow)
alpha_divs_subbb$TimeWindow<-ifelse(alpha_divs_subbb$TimeWindow=="2001_2005","2001 - 2005",alpha_divs_subbb$TimeWindow)
alpha_divs_subbb$TimeWindow<-ifelse(alpha_divs_subbb$TimeWindow=="2006_2012","2006 - 2012",alpha_divs_subbb$TimeWindow)
alpha_divs_subbb$TimeWindow<-as.factor(alpha_divs_subbb$TimeWindow)
alpha_divs_subbb$TimeWindow <- factor(alpha_divs_subbb$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

alpha_divs_subbb<-filter(alpha_divs_subbb, group!="N" & group!="S_asymp")

Sn<-filter(alpha_divs_subbb, group=="S_n")

alpha_divs_subbb2<-filter(alpha_divs_subbb, group=="S"|group=="S_PIE")

#######################
# Figure: S and Spie  #
#######################

alpha_divs_subbb2$group<-as.factor(alpha_divs_subbb2$group)
alpha_divs_subbb2$group <- factor(alpha_divs_subbb2$group, levels = c("S","S_PIE"))
alpha_divs_subbb2$group<-droplevels(alpha_divs_subbb2$group)
alpha_divs_subbb2$group<-as.factor(alpha_divs_subbb2$group)

pd2 <- position_dodge(width = 0.1)

cc<-ggplot(data=alpha_divs_subbb2,aes(x=TimeWindow,y=Mean,group=group,colour=group,shape=group))+
  geom_point(size=2, position = pd2) +
  geom_errorbar(data=alpha_divs_subbb2,aes(ymin=Lower,ymax=Upper),width=0.1,position = pd)+
  geom_line(size=0.5,position = pd)+
  #scale_colour_poke(pokemon = 19, spread = 2, name="",labels=c("S"="S","S_PIE"= expression("S"[PIE])))+
  #scale_colour_grey(name="",labels=c("S"="S","S_PIE"= expression("S"[PIE])), n=2, start=0.5, end=0.9)+
  scale_colour_colorblind(name="",labels=c("S"="Species Richness","S_PIE"= expression("ENS"[PIE])))+
  scale_shape_manual(name="",labels=c("S"="Species Richness","S_PIE"= expression("ENS"[PIE])), values=c("S"=17, "S_PIE"=19))+
  labs(x = "", y = "Subdiscipline diversity")

DivParts_sub<-cc+ theme_bw()+theme(axis.title.x=element_blank(),
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

####################
# Combine figures  #
####################

require(cowplot)

int_tog<-plot_grid(DivParts_conc + theme(legend.position="none", 
                                      panel.background = element_blank(),
                                      panel.grid.major = element_blank()),
                   DivParts_sub +theme(legend.position="none", 
                                     panel.background = element_blank(),
                                     panel.grid.major = element_blank()),
                   labels=c("a)","b)"),label_size=6,align="vh",ncol=2)

legend <- get_legend(DivParts_conc + theme(legend.position="top",legend.direction="horizontal", legend.text=element_text(size=6)))

int_togg <- plot_grid(legend,int_tog,rel_heights = c(0.27,5),ncol=1)

ggsave(filename = file.path("Figures", "Concept_Interdiscp_alpha_Sensitivity_morethan50.png"), 
       width    = 6.5, 
       height   = 4, 
       units    = "in")

int_togg

dev.off()


png(filename="Figures/Integr_Alpha_RarefiedRichness_morethan50.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=4,  
    pointsize=2, 
    res=1000)

DivParts_n


dev.off()