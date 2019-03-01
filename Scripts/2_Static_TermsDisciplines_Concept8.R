##################### 
## Figs. 5 & 6      #
# Show how terms    #
# and subdisciplines#
# change over time  #
#####################
# Selected topics: 8 (increasing) & 18 (decreasing)

require(dplyr)
require(reshape2)
require(ggplot2)
require(viridis)
require(gtools)
require(ggthemes)
require(plotrix)
require(reshape)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

################
## data ########
################

togg<-read.delim("Data/Terms_Relevance_Lift_allTW_07092016.csv",sep=",",header=T)

dis<-read.delim("Data/Topics_Disciplines_11082016.csv",sep=",",header=T)

##################################################
# rank by mean relevance in last time window &   #
# include data from previous time windows   ######
##################################################

togg$Topic_Lambda_TW<-paste(togg$Most_Probable_Topic,togg$lambda,togg$TimeWindow,sep="_")

n<-length(unique(togg$Topic_Lambda_TW))

outt=c();
for(i in 1:n){
  
  test=subset(togg, togg$Topic_Lambda_TW==(unique(togg$Topic_Lambda_TW))[i])  
  
  dimm<-dim(test)
  test<-arrange(test,-probability)
  test$rank_prob<-seq(from=1,to=dimm[1])
  
  test<-arrange(test,-Relevance_allTW)
  test$rank_rel<-seq(from=1,to=dimm[1])
  
  outt[[i]]<-rbind.data.frame(test)
  
}


togg6<-as.data.frame(do.call(rbind,outt),stringsAsFactors=FALSE)

togg6<-filter(togg6,TimeWindow=="2006_2012")
togg6<-select(togg6,Most_Probable_Topic,term,lambda,rank_prob,rank_rel)

togg7<-merge(togg,togg6,by.y=c("Most_Probable_Topic","term","lambda"))

togg7$TimeWindow<-as.character(togg7$TimeWindow)
togg7$TimeWindow<-ifelse(togg7$TimeWindow=="1990_1995","1990-1995",togg7$TimeWindow)
togg7$TimeWindow<-ifelse(togg7$TimeWindow=="1996_2000","1996-2000",togg7$TimeWindow)
togg7$TimeWindow<-ifelse(togg7$TimeWindow=="2001_2005","2001-2005",togg7$TimeWindow)
togg7$TimeWindow<-ifelse(togg7$TimeWindow=="2006_2012","2006-2012",togg7$TimeWindow)
togg7$TimeWindow<-as.factor(togg7$TimeWindow)

#################
# Static Figure #
#################

#########################################
# rescale number of articles per topic ##
#########################################

n<-length(unique(dis$Topic))

outt=c();
for(i in 1:n){
  
  test=subset(dis, dis$Topic==(unique(dis$Topic))[i])  
  
  
  test$sWeight<- rescaler(test$W,type="range")
  
  outt[[i]]<-rbind.data.frame(test)
  
}

dis<-as.data.frame(do.call(rbind,outt),stringsAsFactors=FALSE)

#####################
# summarize #########
# 1) mean Relevance #
###  per term #######
### per lambda ######
#####################

dis$Topic_TW<-paste(dis$Topic,dis$TimeWindow,sep="_")

n<-length(unique(dis$Topic_TW))

outt=c();
for(i in 1:n){
  
  test=subset(dis, dis$Topic_TW==(unique(dis$Topic_TW))[i])  
  
  dimm<-dim(test)
  test<-arrange(test,-Weight)
  test$rank_w<-seq(from=1,to=dimm[1])
  
  outt[[i]]<-rbind.data.frame(test)
  
}

diss<-as.data.frame(do.call(rbind,outt),stringsAsFactors=FALSE)

diss<-filter(diss,TimeWindow=="2006_2012")
diss<-select(diss,Topic,Discipline, rank_w)

diss2<-merge(dis,diss,by.y=c("Topic","Discipline"))

diss2$TimeWindow<-as.character(diss2$TimeWindow)
diss2$TimeWindow<-ifelse(diss2$TimeWindow=="1990_1995","1990-1995",diss2$TimeWindow)
diss2$TimeWindow<-ifelse(diss2$TimeWindow=="1996_2000","1996-2000",diss2$TimeWindow)
diss2$TimeWindow<-ifelse(diss2$TimeWindow=="2001_2005","2001-2005",diss2$TimeWindow)
diss2$TimeWindow<-ifelse(diss2$TimeWindow=="2006_2012","2006-2012",diss2$TimeWindow)
diss2$TimeWindow<-as.factor(diss2$TimeWindow)

#######################
## Static: concept 8  #
#######################

### Concept 8:terms

topic_8<-filter(togg7, Most_Probable_Topic==8  & rank_rel <51)

topic_8$term<-as.factor(topic_8$term)
topic_8$term = with(topic_8, factor(term, levels = rev(levels(term))))

f8_s <- ggplot(topic_8, aes(y=term, x=TimeWindow, fill=Relevance_allTW))+
  geom_tile(color="white", size=0.1) +
  #scale_fill_gradientn(name=expression(italic(paste(phi["kw"]))),colours=c("blue", "red"))
  scale_fill_viridis(name="p(Term)", option="D") +
  labs(x="",y="Terms",title="")+
  theme_tufte(base_family = "serif")+
  theme(axis.title.y=element_text(face="bold"),
        plot.title=element_text(face="bold",size=8),
        axis.text.y=element_text(colour="black",face="plain",size=6),
        legend.position="right",legend.key.height=unit(1.5,"cm"),
        legend.key.width=unit(0.5,"cm"),
        legend.text = element_text(size=7))

### Concept 8: disciplines

topic_8s<-filter(diss2,Topic==8 & rank_w<26)

topic_8s$Discipline<-as.factor(topic_8s$Discipline)
topic_8s$Discipline <- with(topic_8s, factor(Discipline, levels = rev(levels(Discipline))))

f8_d <- ggplot(topic_8s, aes(y=Discipline, x=TimeWindow, fill=sWeight))+
  geom_tile(color="white", size=0.1) +
  #scale_fill_gradientn(name="Articles",colours=c("blue", "red"))
  scale_fill_viridis(name="Articles",option="D") + 
  labs(x="",y="Subdisciplines",title="")+
  theme_tufte(base_family = "serif")+
  theme(axis.title.y=element_text(face="bold"),
        axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=6),
        plot.title=element_text(face="bold",size=6),
        legend.position="right",
        legend.key.height=unit(1.5,"cm"),legend.key.width=unit(0.5,"cm"),
        legend.text = element_text(size=7))

####

require(cowplot)

png(filename="Figures/StaticFigure_Concepts_Disci_t8.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=7.1, 
    pointsize=2, 
    res=700)

plot_grid(f8_s, f8_d, labels=c("a)","b)"),label_size=7,ncol=1,align="h",vjust=c(2,1.5),rel_widths=c(2,0.8), rel_heights = c(1.25,0.9))

dev.off()

#############################
#  concept 18: decreasing   #
#############################

topic_18<-filter(togg7, Most_Probable_Topic==18  & rank_rel <51)

topic_18$term<-as.factor(topic_18$term)
topic_18$term = with(topic_18, factor(term, levels = rev(levels(term))))

f18_s <- ggplot(topic_18, aes(y=term, x=TimeWindow, fill=Relevance_allTW))+
  geom_tile(color="white", size=0.1) +
  #scale_fill_gradientn(name=expression(italic(paste(phi["kw"]))),colours=c("blue", "red"))
  scale_fill_viridis(name="p(Term)", option="D") +
  labs(x="",y="Terms",title="")+
  theme_tufte(base_family = "serif")+theme(axis.title.y=element_text(face="bold"),
                                           plot.title=element_text(face="bold",size=8),
                                           axis.text.y=element_text(colour="black",face="plain",size=6),
                                           legend.position="right",legend.key.height=unit(1.5,"cm"),
                                           legend.key.width=unit(0.5,"cm"),
                                           legend.text = element_text(size=7))

### Concept 8: disciplines

topic_18s<-filter(diss2,Topic==18 & rank_w<26)

topic_18s$Discipline<-as.factor(topic_18s$Discipline)
topic_18s$Discipline <- with(topic_18s, factor(Discipline, levels = rev(levels(Discipline))))

f18_d <- ggplot(topic_18s, aes(y=Discipline, x=TimeWindow, fill=sWeight))+
  geom_tile(color="white", size=0.1) +
  #scale_fill_gradientn(name="Articles",colours=c("blue", "red"))
  scale_fill_viridis(name="Articles",option="D") + 
  labs(x="",y="Subdisciplines",title="")+
  theme_tufte(base_family = "serif")+
  theme(axis.title.y=element_text(face="bold"),
        axis.text.x=element_text(size=6),
        axis.text.y=element_text(size=6),
        plot.title=element_text(face="bold",size=6),
        legend.position="right",
        legend.key.height=unit(1.5,"cm"),legend.key.width=unit(0.5,"cm"),
        legend.text = element_text(size=7))


####

require(cowplot)

png(filename="Figures/StaticFigure_Concepts_Disci_t18.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=7.1, 
    pointsize=2, 
    res=700)

plot_grid(f18_s, f18_d, labels=c("a)","b)"),label_size=7,ncol=1,align="h",vjust=c(2,1.5),
          rel_widths=c(2,0.8), rel_heights = c(1.25,0.9))

dev.off()
  