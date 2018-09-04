#################################################
# Figures for alpha diversity (orders 0, 1, 2)  #
#################################################

require(rms)
require(ggplot2)
require(dplyr)
require(reshape2)

###########
# Data ####
###########

alpha_div<-read.csv("Cleaned_Data/integration_AlphaDiv_orders.csv",stringsAsFactors = FALSE)

# calculate means and bootstrapped CIs

alpha_divs<- alpha_div %>%
  group_by(TimeWindow, Order) %>%
  do(data.frame(rbind(smean.cl.boot(.$Estimator, B=1000))))


# arrange data for figures

alpha_divs$TimeWindow<-as.character(alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="1990_1995","1990 - 1995",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="1996_2000","1996 - 2000",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="2001_2005","2001 - 2005",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-ifelse(alpha_divs$TimeWindow=="2006_2012","2006 - 2012",alpha_divs$TimeWindow)
alpha_divs$TimeWindow<-as.factor(alpha_divs$TimeWindow)
alpha_divs$TimeWindow <- factor(alpha_divs$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

alpha_divs$Order<-as.factor(alpha_divs$Order)
alpha_divs$Order <- factor(alpha_divs$Order, levels = c("0","1","2"))

colnames(alpha_divs)[3]<-"Estimator"

# figure

bb<-ggplot(data=alpha_divs,aes(x=TimeWindow,y=Estimator,group=Order,colour=Order))+
   geom_point(shape=20,size=2) +
  geom_errorbar(data=alpha_divs,aes(ymin=Lower,ymax=Upper),width=0.1)+
  geom_line(size=0.5)+
  scale_colour_manual(name="",labels=c("0"=expression(paste(""^0,"D")),"1"= expression(paste(""^1,"D")),"2"= expression(paste(""^2,"D"))),
                      values = c("0"="#67a9cf","1" = "#ef8a62","2" ="gray40"))+
  labs(x = "", y = "Concept diversity")
 
DivParts<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
                               axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                               axis.text.y=element_text(colour="black",face="bold",size=8),
                               axis.text.x=element_text(colour="black",face="bold",size=7),
                               axis.ticks.y=element_line(colour="transparent"),
                               legend.background=element_rect(fill="transparent"),
                               legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                               legend.text=element_text(size=9),
                               legend.position=c(0.92,0.95),legend.direction="vertical",
                               panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank(),
                               plot.margin=unit(c(0.3,0.3,0.3,0.3), "cm"))

png(filename="Figures/Integr_Alpha_orders_final.png", 
    type="cairo",
    units="in", 
    width=4, 
    height=4,  
    pointsize=2, 
    res=1000)

DivParts


dev.off()