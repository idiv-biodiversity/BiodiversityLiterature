####################################
# Classify topics as:              #
# increasing, decreasing, no change#
# in interdisciplinarity           #
####################################

require(tidyr)
require(dplyr)
require(rms)
require(ggplot2)
require(reshape2)
require(ggthemes)

# data 

alpha_div<-read.csv("Cleaned_Data/interdisciplinarity_AlphaDiv_orders_mobr.csv",stringsAsFactors = FALSE)

###########################################################
# Classify topic into groups of interdisciplinarity       #
# Groups: increasing (upper 90%), stable, and             #
#         decreasing (lower 10 %)                         #
###########################################################

# for S only

alpha_div_S<-filter(alpha_div, group=="S") 
alpha_div_Ss<- alpha_div_S%>%
        spread(TimeWindow, Value,fill=NA,convert=FALSE)

alpha_div_Ss[is.na(alpha_div_Ss)] <- 0
alpha_div_Ss$'1990_1995'<-alpha_div_Ss$'1990_1995'+0.00000001
alpha_div_Ss$'1996_2000'<-alpha_div_Ss$'1996_2000'+0.00000001
alpha_div_Ss$'2001_2005'<-alpha_div_Ss$'2001_2005'+0.00000001
alpha_div_Ss$'2006_2012'<-alpha_div_Ss$'2006_2012'+0.00000001

alpha_div_Ss$RelativeChange<- (alpha_div_Ss$"2006_2012"-alpha_div_Ss$"1990_1995")/alpha_div_Ss$"2006_2012"

alpha_div_Ss<-arrange(alpha_div_Ss, -RelativeChange)

alpha_div_Ss<-alpha_div_Ss %>%
  mutate(Quantile = findInterval(RelativeChange, 
                                 quantile(RelativeChange, probs=c(0.10,0.90))))

options("scipen"=100, "digits"=4)
alpha_div_Ss$Quantile<-as.factor(alpha_div_Ss$Quantile)

alpha_div_Ss<-alpha_div_Ss%>%
  unite("group", c("Most_Probable_Topic", "Quantile"), sep="_")

alpha_div_Ss<-select(alpha_div_Ss,-RelativeChange)

alpha_div_Ss<-alpha_div_Ss%>%
  gather(TimeWindow,S,"1990_1995":"2006_2012")

alpha_div_Ss<-alpha_div_Ss%>%
  separate(group, into=c("Most_Probable_Topic", "Quantile"), sep="_")

# calculate means and bootstrapped CIs

alpha_div_SS<- alpha_div_S %>%
  group_by(TimeWindow) %>%
  do(data.frame(rbind(smean.cl.boot(.$Value, B=1000))))

colnames(alpha_div_SS)[2]<-"S"
# arrange data for figures

alpha_div_SS$TimeWindow<-as.character(alpha_div_SS$TimeWindow)
alpha_div_SS$TimeWindow<-ifelse(alpha_div_SS$TimeWindow=="1990_1995","1990 - 1995",alpha_div_SS$TimeWindow)
alpha_div_SS$TimeWindow<-ifelse(alpha_div_SS$TimeWindow=="1996_2000","1996 - 2000",alpha_div_SS$TimeWindow)
alpha_div_SS$TimeWindow<-ifelse(alpha_div_SS$TimeWindow=="2001_2005","2001 - 2005",alpha_div_SS$TimeWindow)
alpha_div_SS$TimeWindow<-ifelse(alpha_div_SS$TimeWindow=="2006_2012","2006 - 2012",alpha_div_SS$TimeWindow)
alpha_div_SS$TimeWindow<-as.factor(alpha_div_SS$TimeWindow)
alpha_div_SS$TimeWindow <- factor(alpha_div_SS$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))


alpha_div_Ss$TimeWindow<-as.character(alpha_div_Ss$TimeWindow)
alpha_div_Ss$TimeWindow<-ifelse(alpha_div_Ss$TimeWindow=="1990_1995","1990 - 1995",alpha_div_Ss$TimeWindow)
alpha_div_Ss$TimeWindow<-ifelse(alpha_div_Ss$TimeWindow=="1996_2000","1996 - 2000",alpha_div_Ss$TimeWindow)
alpha_div_Ss$TimeWindow<-ifelse(alpha_div_Ss$TimeWindow=="2001_2005","2001 - 2005",alpha_div_Ss$TimeWindow)
alpha_div_Ss$TimeWindow<-ifelse(alpha_div_Ss$TimeWindow=="2006_2012","2006 - 2012",alpha_div_Ss$TimeWindow)
alpha_div_Ss$TimeWindow<-as.factor(alpha_div_Ss$TimeWindow)
alpha_div_Ss$TimeWindow <- factor(alpha_div_Ss$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

#################
# Make figure ###
#################

S_interdisp<-ggplot(data=alpha_div_Ss, aes(x=TimeWindow,y=S))+
  geom_line(data=alpha_div_Ss, aes(x=TimeWindow,y=S,group=Most_Probable_Topic),colour="gray80")+
  geom_line(data=alpha_div_Ss, aes(x=TimeWindow,y=S,group=Most_Probable_Topic,colour=Quantile))+
  scale_colour_manual(name="",labels=c("0"="Decreasing","1"="Stable","2"="Increasing"),
                      values=c("1"="gray80","0"="#56B4E9","2"="#E69F00"))+
  geom_point(data=alpha_div_SS, aes(x=TimeWindow, y=S, group=TimeWindow), size=0.5, colour="gray30")+
  geom_errorbar(data=alpha_div_SS,aes(ymin=Lower,ymax=Upper, group=TimeWindow),width = 0.05, colour="gray30")+
  
  labs(x="",y="Subdiscipline diversity (species richness)")+
  scale_x_discrete(name="",expand=c(0,0.15))+
  theme_bw()+theme_bw()+theme(axis.title.x=element_blank(),
                              axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                              axis.text.y=element_text(colour="black",face="bold",size=8),
                              axis.text.x=element_text(colour="black",face="bold",size=7),
                              axis.ticks.y=element_line(colour="transparent"),
                              legend.background=element_rect(fill="transparent"),
                              legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                              legend.text=element_text(size=9),
                              legend.text.align = 0.5,
                              legend.position=c(0.92,0.95),legend.direction="vertical",
                              panel.background = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor=element_blank())

###################
# for ENSpie only #
###################

alpha_div_Spie<-filter(alpha_div, group=="S_PIE") # for ENSpie only
alpha_div_SSpie<- alpha_div_Spie%>%
  spread(TimeWindow, Value,fill=NA,convert=FALSE)

# alpha_div_SSpie[is.na(alpha_div_SSpie)] <- 0
# alpha_div_SSpie$'1990_1995'<-alpha_div_SSpie$'1990_1995'+0.00000001
# alpha_div_SSpie$'1996_2000'<-alpha_div_SSpie$'1996_2000'+0.00000001
# alpha_div_SSpie$'2001_2005'<-alpha_div_SSpie$'2001_2005'+0.00000001
# alpha_div_SSpie$'2006_2012'<-alpha_div_SSpie$'2006_2012'+0.00000001

alpha_div_SSpie$RelativeChange<- (alpha_div_SSpie$"2006_2012"-alpha_div_SSpie$"1990_1995")/alpha_div_SSpie$"2006_2012"

alpha_div_SSpie<-arrange(alpha_div_SSpie, -RelativeChange)

alpha_div_SSpie<-alpha_div_SSpie %>%
  mutate(Quantile = findInterval(RelativeChange, 
                                 quantile(RelativeChange, probs=c(0.10,0.90),na.rm=TRUE)))

options("scipen"=100, "digits"=4)
alpha_div_SSpie$Quantile<-as.factor(alpha_div_SSpie$Quantile)

alpha_div_SSpie<-alpha_div_SSpie%>%
  unite("group", c("Most_Probable_Topic", "Quantile"), sep="_")

alpha_div_SSpie<-select(alpha_div_SSpie,-RelativeChange)

alpha_div_SSpie<-alpha_div_SSpie%>%
  gather(TimeWindow,S_PIE,"1990_1995":"2006_2012")

alpha_div_SSpie<-alpha_div_SSpie%>%
  separate(group, into=c("Most_Probable_Topic", "Quantile"), sep="_")

alpha_div_SSpie<-filter(alpha_div_SSpie, !is.na(S_PIE)==TRUE)
alpha_div_SSpie<-filter(alpha_div_SSpie, !(Quantile)=="NA")

# calculate means and bootstrapped CIs

alpha_div_Spiepie<- alpha_div_Spie %>%
  group_by(TimeWindow) %>%
  do(data.frame(rbind(smean.cl.boot(.$Value, B=1000))))

colnames(alpha_div_Spiepie)[2]<-"S_PIE"

# arrange data for figures

alpha_div_SSpie$TimeWindow<-as.character(alpha_div_SSpie$TimeWindow)
alpha_div_SSpie$TimeWindow<-ifelse(alpha_div_SSpie$TimeWindow=="1990_1995","1990 - 1995",alpha_div_SSpie$TimeWindow)
alpha_div_SSpie$TimeWindow<-ifelse(alpha_div_SSpie$TimeWindow=="1996_2000","1996 - 2000",alpha_div_SSpie$TimeWindow)
alpha_div_SSpie$TimeWindow<-ifelse(alpha_div_SSpie$TimeWindow=="2001_2005","2001 - 2005",alpha_div_SSpie$TimeWindow)
alpha_div_SSpie$TimeWindow<-ifelse(alpha_div_SSpie$TimeWindow=="2006_2012","2006 - 2012",alpha_div_SSpie$TimeWindow)
alpha_div_SSpie$TimeWindow<-as.factor(alpha_div_SSpie$TimeWindow)
alpha_div_SSpie$TimeWindow <- factor(alpha_div_SSpie$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

alpha_div_Spiepie$TimeWindow<-as.character(alpha_div_Spiepie$TimeWindow)
alpha_div_Spiepie$TimeWindow<-ifelse(alpha_div_Spiepie$TimeWindow=="1990_1995","1990 - 1995",alpha_div_Spiepie$TimeWindow)
alpha_div_Spiepie$TimeWindow<-ifelse(alpha_div_Spiepie$TimeWindow=="1996_2000","1996 - 2000",alpha_div_Spiepie$TimeWindow)
alpha_div_Spiepie$TimeWindow<-ifelse(alpha_div_Spiepie$TimeWindow=="2001_2005","2001 - 2005",alpha_div_Spiepie$TimeWindow)
alpha_div_Spiepie$TimeWindow<-ifelse(alpha_div_Spiepie$TimeWindow=="2006_2012","2006 - 2012",alpha_div_Spiepie$TimeWindow)
alpha_div_Spiepie$TimeWindow<-as.factor(alpha_div_Spiepie$TimeWindow)
alpha_div_Spiepie$TimeWindow <- factor(alpha_div_Spiepie$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

#################
# Make figure ###
#################

SPIE_interdisp<-ggplot(data=alpha_div_SSpie, aes(x=TimeWindow,y=S_PIE))+
  geom_line(data=alpha_div_SSpie, aes(x=TimeWindow,y=S_PIE,group=Most_Probable_Topic),colour="gray80")+
  geom_line(data=alpha_div_SSpie, aes(x=TimeWindow,y=S_PIE,group=Most_Probable_Topic,colour=Quantile))+
  scale_colour_manual(name="",labels=c("0"="Decreasing","1"="Stable","2"="Increasing"),
                      values=c("1"="transparent","0"="#56B4E9","2"="#E69F00"))+
  geom_point(data=alpha_div_Spiepie, aes(x=TimeWindow, y=S_PIE, group=TimeWindow), size=0.5, colour="gray30")+
  geom_errorbar(data=alpha_div_Spiepie,aes(ymin=Lower,ymax=Upper, group=TimeWindow),width = 0.05, colour="gray30")+
  
  labs(x="",y=expression(bold(paste("Subdiscipline diversity (ENS"[PIE],")"))))+
  scale_x_discrete(name="",expand=c(0,0.15))+
  theme_bw()+theme_bw()+theme(axis.title.x=element_blank(),
                              axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                              axis.text.y=element_text(colour="black",face="bold",size=8),
                              axis.text.x=element_text(colour="black",face="bold",size=7),
                              axis.ticks.y=element_line(colour="transparent"),
                              legend.background=element_rect(fill="transparent"),
                              legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                              legend.text=element_text(size=9),
                              legend.text.align = 0.5,
                              legend.position=c(0.92,0.95),legend.direction="vertical",
                              panel.background = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor=element_blank())

################################
# save topics + quantile info  #
################################

inter_S_quants<-dplyr::summarize(group_by(alpha_div_Ss, Quantile, Most_Probable_Topic), meanS=mean(S,na.rm=T))

inter_SPIE_quants<-dplyr::summarize(group_by(alpha_div_SSpie, Quantile, Most_Probable_Topic), meanSPIE=mean(S_PIE,na.rm=T))

save(inter_S_quants, inter_SPIE_quants, file="Cleaned_Data/Concepts_Quantile_S_ENSPIE.RData")

###############
# combine #####
###############

require(cowplot)

int_tog<-plot_grid(S_interdisp + theme(legend.position="none"),
                   SPIE_interdisp +theme(legend.position="none"),
                   labels=c("a)","b)"),label_size=6,align="vh",ncol=2)

legend <- get_legend(S_interdisp + theme(legend.position="top",
                                         legend.direction="horizontal", 
                                         legend.text=element_text(size=6)))

int_togg <- plot_grid(legend,int_tog,rel_heights = c(0.27,5),ncol=1)

ggsave(filename = file.path("Figures", "Concept_Interdiscp_alpha_IndConcepts_SIfig.png"), 
       width    = 6.5, 
       height   = 4, 
       units    = "in")

int_togg

dev.off()