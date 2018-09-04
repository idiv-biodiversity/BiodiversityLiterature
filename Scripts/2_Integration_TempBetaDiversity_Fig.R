########################################
# Figures for temporal beta diversity ##
########################################

require(ggplot2)
require(dplyr)
require(reshape2)
require(rms)

###########
# Data ####
###########

tempbeta_div<-read.csv("Cleaned_Data/TemporalBetaDiv_RAstopic.csv",stringsAsFactors = FALSE)

# arrange data for figures

tempbeta_div<-melt(tempbeta_div, id.vars=c("TBI","TimeWindow","ResAreas_Orig"),measure.vars = 2:4,value.name = "Value",variable.name = "BetaDiv_Components")

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
levels(tempbeta_divv$TBI)[levels(tempbeta_divv$TBI)=="Jaccard"] <- "Pres./Abs. (Jaccard)"
levels(tempbeta_divv$TBI)[levels(tempbeta_divv$TBI)=="Ruzicka"]   <- "Abundance (Ruzicka)"

# figure

bb<-ggplot(data=tempbeta_divv,aes(x=TimeWindow,y=Mean,group=BetaDiv_Components,colour=BetaDiv_Components))+
  geom_point(shape=20,size=2) +
  geom_errorbar(data=tempbeta_divv,aes(ymin=Lower,ymax=Upper),width=0.1)+
  geom_line(size=0.5)+
  scale_colour_manual(name="",labels=c("B_den"="Loss","C_den"= "Gain","D"= expression(paste("Total ",beta))),
                      values = c("D"="#67a9cf","C_den" = "#ef8a62","B_den" ="gray40"))+
  labs(x = "", y = expression(paste("Temporal ",beta," concept diversity")))+
  facet_wrap(.~TBI)

TempBetaDiv<-bb+ theme_bw()+theme(axis.title.x=element_blank(),
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
                              strip.text = element_text(colour="black",face="bold",size=7),
                              panel.background =element_rect(fill="transparent",colour="black"),panel.grid.minor=element_blank())


png(filename="Figures/Integr_TempBetaDiv.png", 
    type="cairo",
    units="in", 
    width=6, 
    height=4,  
    pointsize=2, 
    res=1000)

TempBetaDiv

dev.off()
