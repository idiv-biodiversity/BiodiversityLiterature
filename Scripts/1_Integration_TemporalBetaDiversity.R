#####################################################
#  Integration:                                     #
#  beta diversity                                   #
#  within biodiversity sub-disciplines OVER TIME    # 
# species = concepts (results of LDA topic model)   #
# communities = biodiversity sub-disciplines        #
# abundance = number of published articles          #
#####################################################

require(dplyr)
require(reshape2)
source("Scripts/TBI_Legendre.R") # Temporal beta diversity indices, following Winegardner et al. 2017

################
# DATA         #
# and clean-up #
################

ras<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T,stringsAsFactors = FALSE)
ras$P_A<-1 # for abundance
rass<-dplyr::summarize(group_by(ras, TimeWindow, ResArea_Orig,Most_Probable_Topic), Abundance=sum(P_A))
rass$ResArea_Orig<-as.factor(rass$ResArea_Orig)

##############################################################################
# create big site x spp data frame for each consecutive pair of time windows #
##############################################################################

#################
# Time Window 1 #
#################

ras12<-filter(rass,TimeWindow=="1990_1995"|TimeWindow=="1996_2000")

ras12<-dcast(ras12, TimeWindow+ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum,na.rm=T)

# get res_areas in common among time windows

res_areas<-select(ras12, TimeWindow, ResArea_Orig)

res_areas1<-filter(res_areas,TimeWindow=="1990_1995")%>%
  select(.,ResArea_Orig)

res_areas11<-as.character(res_areas1$ResArea_Orig) # vector of ResAreas 

res_areas2<-filter(res_areas,TimeWindow=="1996_2000")%>%
            select(.,ResArea_Orig)

res_areas2<- filter(res_areas2,ResArea_Orig %in% res_areas11)
res_areas2<-as.character(res_areas2$ResArea_Orig) # ResAreas in common among both time windows

ras122<-filter(ras12, ResArea_Orig %in% res_areas2 ) # take only research areas in both time windows

ras1<-filter(ras122, TimeWindow=="1990_1995")
ras1<-select(ras1,-TimeWindow)
rownames(ras1)<-ras1$ResArea_Orig
ras1$ResArea_Orig<-NULL

ras2<-filter(ras122, TimeWindow=="1996_2000")
ras2<-select(ras2,-TimeWindow)
rownames(ras2)<-ras2$ResArea_Orig
ras2$ResArea_Orig<-NULL

dim(ras1)==dim(ras2) # this should be true 

#################
# Time Window 2 #
#################

ras22<-filter(rass,TimeWindow=="1996_2000"|TimeWindow=="2001_2005")

ras22<-dcast(ras22, TimeWindow+ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum,na.rm=T)

# get res_areas in common among time windows

res_areas2<-select(ras22, TimeWindow, ResArea_Orig)

res_areas3<-filter(res_areas2,TimeWindow=="1996_2000")%>%
  select(.,ResArea_Orig)

res_areas33<-as.character(res_areas3$ResArea_Orig) # vector of ResAreas

res_areas4<-filter(res_areas2,TimeWindow=="2001_2005")%>%
  select(.,ResArea_Orig)

res_areas22<- filter(res_areas4,ResArea_Orig %in% res_areas33)
res_areas22<-as.character(res_areas22$ResArea_Orig) #ResAreas in common among both time windows

ras222<-filter(ras22, ResArea_Orig %in% res_areas22 ) # take only research areas in both time windows

ras3<-filter(ras222, TimeWindow=="1996_2000")
ras3<-select(ras3,-TimeWindow)
rownames(ras3)<-ras3$ResArea_Orig
ras3$ResArea_Orig<-NULL

ras4<-filter(ras222, TimeWindow=="2001_2005")
ras4<-select(ras4,-TimeWindow)
rownames(ras4)<-ras4$ResArea_Orig
ras4$ResArea_Orig<-NULL

dim(ras3)==dim(ras4) # this should be true 

#################
# Time Window 3 #
#################

ras33<-filter(rass,TimeWindow=="2001_2005"|TimeWindow=="2006_2012")

ras33<-dcast(ras33, TimeWindow+ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum,na.rm=T)

# get res_areas in common among time windows

res_areas4<-select(ras33, TimeWindow, ResArea_Orig)

res_areas5<-filter(res_areas4,TimeWindow=="2001_2005")%>%
  select(.,ResArea_Orig)

res_areas55<-as.character(res_areas5$ResArea_Orig) # vector of ResAreas

res_areas6<-filter(res_areas4,TimeWindow=="2006_2012")%>%
  select(.,ResArea_Orig)

res_areas44<- filter(res_areas6,ResArea_Orig %in% res_areas55)
res_areas44<-as.character(res_areas44$ResArea_Orig)  #ResAreas in common among both time windows

ras333<-filter(ras33, ResArea_Orig %in% res_areas44 ) # take only research areas in both time windows

ras5<-filter(ras333, TimeWindow=="2001_2005")
ras5<-select(ras5,-TimeWindow)
rownames(ras5)<-ras5$ResArea_Orig
ras5$ResArea_Orig<-NULL

ras6<-filter(ras333, TimeWindow=="2006_2012")
ras6<-select(ras6,-TimeWindow)
rownames(ras6)<-ras6$ResArea_Orig
ras6$ResArea_Orig<-NULL

dim(ras5)==dim(ras6) # this should be true 

###############################################
# Calculate TBI, with and without abundance  #
##############################################

# Time Window 1

tbi_pa_tw1 <- TBI(ras1, ras2, method="jaccard",pa.tr=TRUE, nperm=999,
            BCD=TRUE, test.t.perm=FALSE, clock=TRUE)

tbi_pa_BCD_tw1<-cbind.data.frame(ResAreas_Orig=rownames(ras1),tbi_pa_tw1$BCD.mat)
tbi_pa_BCD_tw1<-select(tbi_pa_BCD_tw1, ResAreas_Orig, B_den="B/(A+B+C)" ,C_den="C/(A+B+C)", D="D=(B+C)/(A+B+C)")
tbi_pa_BCD_tw1$TimeWindow<-"Time1_2"

tbi_abund_tw1 <- TBI(ras1, ras2, method="ruzicka",pa.tr=FALSE, nperm=999,
                  BCD=TRUE, test.t.perm=FALSE, clock=TRUE)
tbi_abund_BCD_tw1<-cbind.data.frame(ResAreas_Orig=rownames(ras1),tbi_abund_tw1$BCD.mat)
tbi_abund_BCD_tw1<-select(tbi_abund_BCD_tw1, ResAreas_Orig, B_den="B/(A+B+C)" ,C_den="C/(A+B+C)", D="D=(B+C)/(A+B+C)")
tbi_abund_BCD_tw1$TimeWindow<-"Time1_2"

# Time Window 2

tbi_pa_tw2 <- TBI(ras3, ras4, method="jaccard",pa.tr=TRUE, nperm=999,
                  BCD=TRUE, test.t.perm=FALSE, clock=TRUE)

tbi_pa_BCD_tw2<-cbind.data.frame(ResAreas_Orig=rownames(ras3),tbi_pa_tw2$BCD.mat)
tbi_pa_BCD_tw2<-select(tbi_pa_BCD_tw2, ResAreas_Orig, B_den="B/(A+B+C)" ,C_den="C/(A+B+C)", D="D=(B+C)/(A+B+C)")
tbi_pa_BCD_tw2$TimeWindow<-"Time2_3"

tbi_abund_tw2 <- TBI(ras3, ras4, method="ruzicka",pa.tr=FALSE, nperm=999,
                     BCD=TRUE, test.t.perm=FALSE, clock=TRUE)
tbi_abund_BCD_tw2<-cbind.data.frame(ResAreas_Orig=rownames(ras3),tbi_abund_tw2$BCD.mat)
tbi_abund_BCD_tw2<-select(tbi_abund_BCD_tw2, ResAreas_Orig, B_den="B/(A+B+C)" ,C_den="C/(A+B+C)", D="D=(B+C)/(A+B+C)")
tbi_abund_BCD_tw2$TimeWindow<-"Time2_3"

# Time Window 3

tbi_pa_tw3 <- TBI(ras5, ras6, method="jaccard",pa.tr=TRUE, nperm=999,
                  BCD=TRUE, test.t.perm=FALSE, clock=TRUE)

tbi_pa_BCD_tw3<-cbind.data.frame(ResAreas_Orig=rownames(ras5),tbi_pa_tw3$BCD.mat)
tbi_pa_BCD_tw3<-select(tbi_pa_BCD_tw3, ResAreas_Orig, B_den="B/(A+B+C)" ,C_den="C/(A+B+C)", D="D=(B+C)/(A+B+C)")
tbi_pa_BCD_tw3$TimeWindow<-"Time3_4"

tbi_abund_tw3 <- TBI(ras5, ras6, method="ruzicka",pa.tr=FALSE, nperm=999,
                     BCD=TRUE, test.t.perm=FALSE, clock=TRUE)
tbi_abund_BCD_tw3<-cbind.data.frame(ResAreas_Orig=rownames(ras5),tbi_abund_tw3$BCD.mat)
tbi_abund_BCD_tw3<-select(tbi_abund_BCD_tw3, ResAreas_Orig, B_den="B/(A+B+C)" ,C_den="C/(A+B+C)", D="D=(B+C)/(A+B+C)")
tbi_abund_BCD_tw3$TimeWindow<-"Time3_4"

###################
# Combine results #
###################

tbi_jacc<-rbind.data.frame(tbi_pa_BCD_tw1,tbi_pa_BCD_tw2,tbi_pa_BCD_tw3)
tbi_jacc$TBI<-"Jaccard"

tbi_ruz<-rbind.data.frame(tbi_abund_BCD_tw1,tbi_abund_BCD_tw2,tbi_abund_BCD_tw3)
tbi_ruz$TBI<-"Ruzicka"

tbi_all<-rbind.data.frame(tbi_jacc,tbi_ruz)

write.table(tbi_all, "Cleaned_Data/TemporalBetaDiv_RAstopic.csv",row.names=F)