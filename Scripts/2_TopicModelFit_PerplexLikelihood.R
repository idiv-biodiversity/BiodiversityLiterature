###########################################
# Figure: perplexity and log-likelihood   #
###########################################

require(ggplot2)
require(cowplot)

#################
# data ##########
#################

perp<-read.delim("Data/TopicModel_Perplexity.csv",sep=",",header=T)

perp$perplexity<-as.numeric(as.character(perp$perplexity))

perpp<-ggplot(data=perp,aes(x=topic.number,y=perplexity))+
  geom_point(shape=20,size=3)+
  geom_smooth(method="auto",se=FALSE,fullrange=TRUE,color="gray40")+
  labs(x="Number of concepts",y="Perplexity")+
  geom_vline(xintercept=50,color="red")+
  theme_bw()+
  theme(axis.title.x=element_text(colour="black",face=c("bold"),size=8),
        axis.title.y=element_text(colour="black",face=c("bold"),size=8),
        axis.text.y=element_text(colour="black",face="bold",size=8),
        axis.text.x=element_text(colour="black",face="bold",size=8),
        axis.ticks.y=element_line(colour="transparent"),
        panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

likeli<-ggplot(data=perp,aes(x=topic.number,y=log.likelihood))+
  geom_point(shape=20,size=3)+
  geom_smooth(method="auto",se=FALSE,fullrange=TRUE,color="gray40")+
  labs(x="Number of concepts",y="Log-likelihood")+
  geom_vline(xintercept=50,color="red")+
  theme_bw()+
  theme(axis.title.x=element_text(colour="black",face=c("bold"),size=8),
        axis.title.y=element_text(colour="black",face=c("bold"),size=8),
        axis.text.y=element_text(colour="black",face="bold",size=8),
        axis.text.x=element_text(colour="black",face="bold",size=8),
        axis.ticks.y=element_line(colour="transparent"),
        panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


## save figure

png(filename="Figures/TopicModelFit_Perplex_Likeli.png", 
    type="cairo",
    units="in", 
    width=7, 
    height=5,  
    pointsize=2, 
    res=500)

plot_grid(perpp,likeli,labels=c("a)","b)"),label_size=8,ncol=2)

dev.off()
