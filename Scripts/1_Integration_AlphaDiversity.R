#####################################################
#  Integration:                                     #
# alpha        diversity                            #
# across & within biodiversity sub-disciplines      # 
# species = concepts (results of LDA topic model)   #
# communities = biodiversity sub-disciplines        #
# abundance = number of published articles          #
#####################################################

require(dplyr)
require(reshape2)
require(iNEXT)

################
# DATA         #
# and clean-up #
################

ras<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T,stringsAsFactors = FALSE)
ras$P_A<-1 # for abundance
rass<-summarize(group_by(ras, TimeWindow, ResArea_Orig,Most_Probable_Topic), Abundance=sum(P_A))
rass$ResArea_Orig<-as.factor(rass$ResArea_Orig)

tw1<-"1990_1995"
tw2<-"1996_2000"
tw3<-"2001_2005"
tw4<-"2006_2012"

# create separate data sets for each time window

ras1<-filter(rass,TimeWindow=="1990_1995")
rass1<-dcast(ras1, Most_Probable_Topic~ResArea_Orig, value.var="Abundance",sum,na.rm=T)
rownames(rass1)<-as.character(rass1$Most_Probable_Topic)
rass1<-select(rass1,-Most_Probable_Topic)
rass1<-data.matrix(rass1)

ras2<-filter(rass,TimeWindow=="1996_2000")
rass2<-dcast(ras2, Most_Probable_Topic~ResArea_Orig, value.var="Abundance",sum)
rownames(rass2)<-rass2$Most_Probable_Topic
rass2<-select(rass2,-Most_Probable_Topic)
rass2<-data.matrix(rass2)

ras3<-filter(rass,TimeWindow=="2001_2005")
rass3<-dcast(ras3, Most_Probable_Topic~ResArea_Orig, value.var="Abundance",sum)
rownames(rass3)<-rass3$Most_Probable_Topic
rass3<-select(rass3,-Most_Probable_Topic)
rass3<-data.matrix(rass3)

ras4<-filter(rass,TimeWindow=="2006_2012")
rass4<-dcast(ras4, Most_Probable_Topic~ResArea_Orig, value.var="Abundance",sum)
rownames(rass4)<-rass4$Most_Probable_Topic
rass4<-select(rass4,-Most_Probable_Topic)
rass4<-data.matrix(rass4)

################################################################################
# Estimate alpha diversity using Chao asymptotic estimator (Chao et al. 2014)  #
################################################################################

# Time Window 1

tw1_q0<-ChaoRichness(rass1)
tw1_q0$ResArea_Orig<-rownames(tw1_q0)
tw1_q0$TimeWindow<-tw1
tw1_q0$Order<-0

tw1_q1<-ChaoShannon(rass1,transform=TRUE)
tw1_q1$ResArea_Orig<-rownames(tw1_q1)
tw1_q1$TimeWindow<-tw1
colnames(tw1_q1)[3]<-"Est_s.e."
tw1_q1$Order<-1

tw1_q2<-ChaoSimpson(rass1,transform=TRUE)
tw1_q2$ResArea_Orig<-rownames(tw1_q2)
tw1_q2$TimeWindow<-tw1
tw1_q2$Order<-2

tw1_qq<-rbind.data.frame(tw1_q0,tw1_q1,tw1_q2)

# Time Window 2

tw2_q0<-ChaoRichness(rass2)
tw2_q0$ResArea_Orig<-rownames(tw2_q0)
tw2_q0$TimeWindow<-tw2
tw2_q0$Order<-0

tw2_q1<-ChaoShannon(rass2,transform=TRUE)
tw2_q1$ResArea_Orig<-rownames(tw2_q1)
tw2_q1$TimeWindow<-tw2
colnames(tw2_q1)[3]<-"Est_s.e."
tw2_q1$Order<-1

tw2_q2<-ChaoSimpson(rass2,transform=TRUE)
tw2_q2$ResArea_Orig<-rownames(tw2_q2)
tw2_q2$TimeWindow<-tw2
tw2_q2$Order<-2

tw2_qq<-rbind.data.frame(tw2_q0,tw2_q1,tw2_q2)

# Time Window 3

tw3_q0<-ChaoRichness(rass3)
tw3_q0$ResArea_Orig<-rownames(tw3_q0)
tw3_q0$TimeWindow<-tw3
tw3_q0$Order<-0

tw3_q1<-ChaoShannon(rass3,transform=TRUE)
tw3_q1$ResArea_Orig<-rownames(tw3_q1)
tw3_q1$TimeWindow<-tw3
colnames(tw3_q1)[3]<-"Est_s.e."
tw3_q1$Order<-1

tw3_q2<-ChaoSimpson(rass3,transform=TRUE)
tw3_q2$ResArea_Orig<-rownames(tw3_q2)
tw3_q2$TimeWindow<-tw3
tw3_q2$Order<-2

tw3_qq<-rbind.data.frame(tw3_q0,tw3_q1,tw3_q2)

# Time Window 4

tw4_q0<-ChaoRichness(rass4)
tw4_q0$ResArea_Orig<-rownames(tw4_q0)
tw4_q0$TimeWindow<-tw4
tw4_q0$Order<-0

tw4_q1<-ChaoShannon(rass4,transform=TRUE)
tw4_q1$ResArea_Orig<-rownames(tw4_q1)
tw4_q1$TimeWindow<-tw4
colnames(tw4_q1)[3]<-"Est_s.e."
tw4_q1$Order<-1

tw4_q2<-ChaoSimpson(rass4,transform=TRUE)
tw4_q2$ResArea_Orig<-rownames(tw4_q2)
tw4_q2$TimeWindow<-tw4
tw4_q2$Order<-2

tw4_qq<-rbind.data.frame(tw4_q0,tw4_q1,tw4_q2)

#####################
# write out results #
#####################

alpha_div<-rbind.data.frame(tw1_qq,tw2_qq,tw3_qq,tw4_qq)

write.csv(alpha_div,"Cleaned_Data/integration_AlphaDiv_orders.csv",row.names=F)

