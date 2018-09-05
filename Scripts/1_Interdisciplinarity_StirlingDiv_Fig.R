#######################
## Interdisciplinarity#
#  Stirling-diversity #
# Figure              #  
#######################

require(tidyr)
require(dplyr)
require(ggplot2)
require(reshape2)
library(boot)

##########
# data ###
##########

# Rao-Stirling calculated over time

load("Cleaned_Data/Interdisciplinarity_StirlingDiversity.RData")

##########
# Jaccard#
##########

top_time<-unique(select(sd_jaccard, Most_Probable_Topic, TimeWindow))
top_time$PA<-1

topp<-dcast(top_time, Most_Probable_Topic~TimeWindow, value.var="PA",length)
topp<-melt(topp, id.vars="Most_Probable_Topic", measure.vars=2:5,variable.name="TimeWindow",value.name="Place_Holder")
topp<-select(topp,-Place_Holder)

##########

coss<-merge(topp,sd_jaccard ,by.y=c("Most_Probable_Topic","TimeWindow"),all.x=TRUE)

coss$RaoStirling<-ifelse(is.na(coss$RaoStirling)==TRUE, 0,coss$RaoStirling)

coss$TimeWindow<-as.character(coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="1990_1995", "1990 - 1995",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="1996_2000", "1996 - 2000",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="2001_2005", "2001 - 2005",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="2006_2012", "2006 - 2012",coss$TimeWindow)

###############################
# calculate bootstrapped 95%  #
###############################

outt<-list()

for(i in 1:4){
  test<-subset(coss, coss$TimeWindow==(unique(coss$TimeWindow))[i])  
  
  Medianboot <- boot(test$RaoStirling,
                     function(x,i) median(x[i]),
                     R=1000)
  
  a<-boot.ci(Medianboot,conf = 0.95,type = c("norm", "basic" ,"perc", "bca"))
  aa<-cbind.data.frame(a$t0, a$percent[4],a$percent[5])
  aa<-dplyr::select(aa,Median="a$t0",l95="a$percent[4]",h95="a$percent[5]")
  aa$TimeWindow<-as.character(unique(test$TimeWindow))
  
  outt[[i]]<-rbind.data.frame(aa)
}

cos_S<-do.call(rbind.data.frame, outt)
cos_S<-select(cos_S, TimeWindow,RaoStirling=Median,l95,h95)

cos_summ_jacc<-cos_S

###########################################################
# Classify topic into groups of interdisciplinarity       #
# Groups: increasing (upper 90%), stable, and             #
#         decreasing (lower 10 %)                         #
###########################################################

cos_w<-dcast(coss, Most_Probable_Topic~TimeWindow, value.var="RaoStirling",mean)

cos_w$'2006 - 2012'<-cos_w$'2006 - 2012'+0.00000001
cos_w$'1996 - 2000'<-cos_w$'1996 - 2000'+0.00000001
cos_w$'2001 - 2005'<-cos_w$'2001 - 2005'+0.00000001
cos_w$'1990 - 1995'<-cos_w$'1990 - 1995'+0.00000001

cos_w$RelativeChange<- (cos_w[,5]-cos_w[,2])/cos_w[,5]
cos_w<-arrange(cos_w, -RelativeChange)

cos_w<-cos_w %>%
  mutate(Quantile = findInterval(RelativeChange, 
                                 quantile(RelativeChange, probs=c(0.10,0.90))))

cos_w$Quantile<-as.factor(cos_w$Quantile)

cos_w<-select(cos_w, Most_Probable_Topic, Quantile)

coss<-merge(coss, cos_w,by.y=c("Most_Probable_Topic"))

#################
# Make figure ###
#################

RS_jac<-ggplot(data=coss, aes(x=TimeWindow,y=RaoStirling))+
  geom_line(data=coss, aes(x=TimeWindow,y=RaoStirling,group=Most_Probable_Topic),colour="gray80")+
  geom_line(data=coss, aes(x=TimeWindow,y=RaoStirling,group=Most_Probable_Topic,colour=Quantile))+
  scale_colour_manual(name="",labels=c("0"="Decreasing","1"="Stable","2"="Increasing"),
                      values=c("1"="transparent","0"="#67a9cf","2"="#ef8a62"))+
  
  geom_point(data=cos_S, aes(x=TimeWindow, y=RaoStirling, group=TimeWindow), size=0.5, colour="gray30")+
  geom_errorbar(data=cos_S,aes(ymin=l95,ymax=h95, group=TimeWindow),width = 0.05, colour="gray30")+
  
  labs(x="",y="Interdisciplinarity (Rao-Stirling)")+
  scale_x_discrete(name="",expand=c(0,0.15))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.key=element_rect(fill="transparent"),
                   legend.text=element_text(size=6),
                   legend.position="none",
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

ggsave(filename = file.path("Figures", 
                            "Interdisciplinarity_RS_jaccard.png"), 
       width    = 10, 
       height   = 10, 
       units    = "cm")

RS_jac

dev.off()

#############
# data out  #
#############

coss_jacc<-select(coss, Topic=Most_Probable_Topic, TimeWindow, Interdisciplinarity=RaoStirling, Quantile)

###############
# Bray-Curtis #
###############

top_time<-unique(select(sd_bray, Most_Probable_Topic, TimeWindow))
top_time$PA<-1

topp<-dcast(top_time, Most_Probable_Topic~TimeWindow, value.var="PA",length)
topp<-melt(topp, id.vars="Most_Probable_Topic", measure.vars=2:5,variable.name="TimeWindow",value.name="Place_Holder")
topp<-select(topp,-Place_Holder)

##########

coss<-merge(topp,sd_bray ,by.y=c("Most_Probable_Topic","TimeWindow"),all.x=TRUE)

coss$RaoStirling<-ifelse(is.na(coss$RaoStirling)==TRUE, 0,coss$RaoStirling)

coss$TimeWindow<-as.character(coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="1990_1995", "1990 - 1995",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="1996_2000", "1996 - 2000",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="2001_2005", "2001 - 2005",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="2006_2012", "2006 - 2012",coss$TimeWindow)

###############################
# calculate bootstrapped 95%  #
###############################

outt<-list()

for(i in 1:4){
  test<-subset(coss, coss$TimeWindow==(unique(coss$TimeWindow))[i])  
  
  Medianboot <- boot(test$RaoStirling,
                     function(x,i) median(x[i]),
                     R=1000)
  
  a<-boot.ci(Medianboot,conf = 0.95,type = c("norm", "basic" ,"perc", "bca"))
  aa<-cbind.data.frame(a$t0, a$percent[4],a$percent[5])
  aa<-dplyr::select(aa,Median="a$t0",l95="a$percent[4]",h95="a$percent[5]")
  aa$TimeWindow<-as.character(unique(test$TimeWindow))
  
  outt[[i]]<-rbind.data.frame(aa)
}

cos_S<-do.call(rbind.data.frame, outt)
cos_S<-select(cos_S, TimeWindow,RaoStirling=Median,l95,h95)

cos_summ_bray<-cos_S

###########################################################
# Classify topic into groups of interdisciplinarity       #
# Groups: increasing (upper 90%), stable, and             #
#         decreasing (lower 10 %)                         #
###########################################################

cos_w<-dcast(coss, Most_Probable_Topic~TimeWindow, value.var="RaoStirling",mean)

cos_w$'2006 - 2012'<-cos_w$'2006 - 2012'+0.00000001
cos_w$'1996 - 2000'<-cos_w$'1996 - 2000'+0.00000001
cos_w$'2001 - 2005'<-cos_w$'2001 - 2005'+0.00000001
cos_w$'1990 - 1995'<-cos_w$'1990 - 1995'+0.00000001

cos_w$RelativeChange<- (cos_w[,5]-cos_w[,2])/cos_w[,5]
cos_w<-arrange(cos_w, -RelativeChange)

cos_w<-cos_w %>%
  mutate(Quantile = findInterval(RelativeChange, 
                                 quantile(RelativeChange, probs=c(0.10,0.90))))

cos_w$Quantile<-as.factor(cos_w$Quantile)

cos_w<-select(cos_w, Most_Probable_Topic, Quantile)

coss<-merge(coss, cos_w,by.y=c("Most_Probable_Topic"))

#################
# Make figure ###
#################

# RS_bray<-ggplot(data=coss, aes(x=TimeWindow,y=RaoStirling))+
#   geom_line(data=coss, aes(x=TimeWindow,y=RaoStirling,group=Most_Probable_Topic),colour="gray80")+
#   geom_line(data=coss, aes(x=TimeWindow,y=RaoStirling,group=Most_Probable_Topic,colour=Quantile))+
#   scale_colour_manual(name="",labels=c("0"="Decreasing","1"="Stable","2"="Increasing"),
#                       values=c("1"="transparent","0"="#67a9cf","2"="#ef8a62"))+
#   
#   geom_point(data=cos_S, aes(x=TimeWindow, y=RaoStirling, group=TimeWindow), size=0.5, colour="gray30")+
#   geom_errorbar(data=cos_S,aes(ymin=l95,ymax=h95, group=TimeWindow),width = 0.05, colour="gray30")+
#   
#   labs(x="",y="Interdisciplinarity (Rao-Stirling)")+
#   scale_x_discrete(name="",expand=c(0,0.15))+
#   scale_y_continuous(limits = c(0,0.15),breaks=c(0,0.05,0.10,0.15))+
#   theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
#                    axis.title.y=element_text(colour="black",face="bold",size=6),
#                    axis.text.x=element_text(colour=c("black"),face="bold",size=6),
#                    axis.text.y=element_text(colour=c("black"),face="bold",size=6),
#                    legend.key=element_rect(fill="transparent"),
#                    legend.text=element_text(size=6),
#                    legend.position="none",
#                    panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())
# 
# 
# ggsave(filename = file.path("Figures", 
#                             "Interdisciplinarity_RS_bray.png"), 
#        width    = 10, 
#        height   = 10, 
#        units    = "cm")
# 
# RS_bray
# 
# dev.off()

#############
# data out  #
#############

coss_bray<-select(coss, Topic=Most_Probable_Topic, TimeWindow, Interdisciplinarity=RaoStirling, Quantile)

###############
# Cosine ######
###############

top_time<-unique(select(sd_cosine, Most_Probable_Topic, TimeWindow))
top_time$PA<-1

topp<-dcast(top_time, Most_Probable_Topic~TimeWindow, value.var="PA",length)
topp<-melt(topp, id.vars="Most_Probable_Topic", measure.vars=2:5,variable.name="TimeWindow",value.name="Place_Holder")
topp<-select(topp,-Place_Holder)

##########

coss<-merge(topp,sd_cosine ,by.y=c("Most_Probable_Topic","TimeWindow"),all.x=TRUE)

coss$RaoStirling<-ifelse(is.na(coss$RaoStirling)==TRUE, 0,coss$RaoStirling)

coss$TimeWindow<-as.character(coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="1990_1995", "1990 - 1995",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="1996_2000", "1996 - 2000",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="2001_2005", "2001 - 2005",coss$TimeWindow)
coss$TimeWindow<-ifelse(coss$TimeWindow=="2006_2012", "2006 - 2012",coss$TimeWindow)

###############################
# calculate bootstrapped 95%  #
###############################

outt<-list()

for(i in 1:4){
  test<-subset(coss, coss$TimeWindow==(unique(coss$TimeWindow))[i])  
  
  Medianboot <- boot(test$RaoStirling,
                     function(x,i) median(x[i]),
                     R=1000)
  
  a<-boot.ci(Medianboot,conf = 0.95,type = c("norm", "basic" ,"perc", "bca"))
  aa<-cbind.data.frame(a$t0, a$percent[4],a$percent[5])
  aa<-dplyr::select(aa,Median="a$t0",l95="a$percent[4]",h95="a$percent[5]")
  aa$TimeWindow<-as.character(unique(test$TimeWindow))
  
  outt[[i]]<-rbind.data.frame(aa)
}

cos_S<-do.call(rbind.data.frame, outt)
cos_S<-select(cos_S, TimeWindow,RaoStirling=Median,l95,h95)

cos_summ_cos<-cos_S

###########################################################
# Classify topic into groups of interdisciplinarity       #
# Groups: increasing (upper 90%), stable, and             #
#         decreasing (lower 10 %)                         #
###########################################################

cos_w<-dcast(coss, Most_Probable_Topic~TimeWindow, value.var="RaoStirling",mean)

cos_w$'2006 - 2012'<-cos_w$'2006 - 2012'+0.00000001
cos_w$'1996 - 2000'<-cos_w$'1996 - 2000'+0.00000001
cos_w$'2001 - 2005'<-cos_w$'2001 - 2005'+0.00000001
cos_w$'1990 - 1995'<-cos_w$'1990 - 1995'+0.00000001

cos_w$RelativeChange<- (cos_w[,5]-cos_w[,2])/cos_w[,5]
cos_w<-arrange(cos_w, -RelativeChange)

cos_w<-cos_w %>%
  mutate(Quantile = findInterval(RelativeChange, 
                                 quantile(RelativeChange, probs=c(0.10,0.90))))

cos_w$Quantile<-as.factor(cos_w$Quantile)

cos_w<-select(cos_w, Most_Probable_Topic, Quantile)

coss<-merge(coss, cos_w,by.y=c("Most_Probable_Topic"))

#################
# Make figure ###
#################

RS_cos<-ggplot(data=coss, aes(x=TimeWindow,y=RaoStirling))+
  geom_line(data=coss, aes(x=TimeWindow,y=RaoStirling,group=Most_Probable_Topic),colour="gray80")+
  geom_line(data=coss, aes(x=TimeWindow,y=RaoStirling,group=Most_Probable_Topic,colour=Quantile))+
  scale_colour_manual(name="",labels=c("0"="Decreasing","1"="Stable","2"="Increasing"),
                      values=c("1"="transparent","0"="#67a9cf","2"="#ef8a62"))+
  
  geom_point(data=cos_S, aes(x=TimeWindow, y=RaoStirling, group=TimeWindow), size=0.5, colour="gray30")+
  geom_errorbar(data=cos_S,aes(ymin=l95,ymax=h95, group=TimeWindow),width = 0.05, colour="gray30")+
  
  labs(x="",y="Interdisciplinarity (Rao-Stirling)")+
  scale_x_discrete(name="",expand=c(0,0.15))+
  scale_y_continuous(limits = c(0,0.32),breaks=c(0,0.10,0.20,0.30))+
  theme_bw()+theme(axis.title.x=element_text(colour="black",face="bold",size=6),
                   axis.title.y=element_text(colour="black",face="bold",size=6),
                   axis.text.x=element_text(colour=c("black"),face="bold",size=6),
                   axis.text.y=element_text(colour=c("black"),face="bold",size=6),
                   legend.key=element_rect(fill="transparent"),
                   legend.text=element_text(size=6),
                   legend.position="none",
                   panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


ggsave(filename = file.path("Figures", 
                            "Interdisciplinarity_RS_cos.png"), 
       width    = 10, 
       height   = 10, 
       units    = "cm")

RS_cos

dev.off()

#############
# data out  #
#############

coss_cos<-select(coss, Topic=Most_Probable_Topic, TimeWindow, Interdisciplinarity=RaoStirling, Quantile)

################

write.table(coss_jacc,"Cleaned_Data/Interdisciplinarity_RS_jacc.csv",sep=",",row.names=F)
write.table(coss_bray,"Cleaned_Data/Interdisciplinarity_RS_bray.csv",sep=",",row.names=F)
write.table(coss_cos,"Cleaned_Data/Interdisciplinarity_RS_cos.csv",sep=",",row.names=F)

# summarized interdisciplinarity data for each disparity matrix

save(cos_summ_jacc,cos_summ_bray, cos_summ_cos,file="Cleaned_Data/Interdisciplinarity_summary.RData")
