#####################################
# Figures for beta diversity        #
#####################################

require(ggplot2)
require(dplyr)
require(reshape2)

###########
# Data ####
###########

beta_div<-read.csv("Cleaned_Data/Integration_BetaDiversity_Legendre.csv",stringsAsFactors = FALSE)

# arrange data for figures

beta_div<-melt(beta_div, id.vars=c("Index","TimeWindow"),measure.vars = 1:5,value.name = "Value",variable.name = "BetaDiv_Components")
beta_div$TimeWindow<-as.character(beta_div$TimeWindow)
beta_div$TimeWindow<-ifelse(beta_div$TimeWindow=="1990_1995","1990 - 1995",beta_div$TimeWindow)
beta_div$TimeWindow<-ifelse(beta_div$TimeWindow=="1996_2000","1996 - 2000",beta_div$TimeWindow)
beta_div$TimeWindow<-ifelse(beta_div$TimeWindow=="2001_2005","2001 - 2005",beta_div$TimeWindow)
beta_div$TimeWindow<-ifelse(beta_div$TimeWindow=="2006_2012","2006 - 2012",beta_div$TimeWindow)
beta_div$TimeWindow<-as.factor(beta_div$TimeWindow)
beta_div$TimeWindow <- factor(beta_div$TimeWindow, levels = c("1990 - 1995","1996 - 2000","2001 - 2005","2006 - 2012"))

beta_div<-filter(beta_div, BetaDiv_Components=="BDtotal"|BetaDiv_Components=="Repl"|BetaDiv_Components=="RichDif")

beta_div$BetaDiv_Components<-as.factor(beta_div$BetaDiv_Components)
beta_div$BetaDiv_Components <- factor(beta_div$BetaDiv_Components, levels = c("BDtotal","Repl","RichDif"))

beta_div$Index<-as.factor(beta_div$Index)
levels(beta_div$Index)[levels(beta_div$Index)=="Jaccard_pa"] <- "a)          Pres./Abs. (Jaccard)"
levels(beta_div$Index)[levels(beta_div$Index)=="Ruzicka_abund"]   <- "b)          Abundance (Ruzicka)"

# figure

bb<-ggplot(data=beta_div,aes(x=TimeWindow,y=Value,group=BetaDiv_Components,colour=BetaDiv_Components))+
  geom_point(shape=20,size=2) +
  geom_line(size=0.5)+
  scale_colour_manual(name="",labels=c("Repl"="Repl","RichDif"= "Rich/Abund Diff","BDtotal"= expression(paste("Total ",beta))),
                      values = c("BDtotal"="#67a9cf","RichDif" = "#ef8a62","Repl" ="gray40"))+
  labs(x = "", y = expression(paste(beta," concept diversity")))+
  facet_wrap(.~Index)

BetaDiv<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
                               axis.title.y=element_text(colour="black",face=c("bold"),size=8),
                               axis.text.y=element_text(colour="black",face="bold",size=8),
                               axis.text.x=element_text(colour="black",face="bold",size=7),
                               axis.ticks.y=element_line(colour="transparent"),
                               legend.background=element_rect(fill="transparent"),
                               legend.key=element_rect(fill="transparent"),legend.key.size = unit(0.25, "cm"),
                               legend.text=element_text(size=9),
                               legend.text.align = 0.5,
                               legend.position="top",legend.direction="horizontal",
                               strip.background = element_rect(fill="transparent"),
                               strip.text = element_text(colour="black",face="bold",size=7, hjust=0),
                               panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())

png(filename="Figures/Integr_BetaDiv_orders.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=4,  
    pointsize=2, 
    res=1000)

BetaDiv

dev.off()
