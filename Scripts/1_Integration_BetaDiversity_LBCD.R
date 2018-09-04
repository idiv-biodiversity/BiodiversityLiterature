#####################################################
#  Integration:                                     #
#  beta diversity                                   #
# across & within biodiversity sub-disciplines      # 
# species = concepts (results of LDA topic model)   #
# communities = biodiversity sub-disciplines        #
# abundance = number of published articles          #
#####################################################

require(dplyr)
require(reshape2)
require(adespatial)

################
# DATA         #
# and clean-up #
################

ras<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T,stringsAsFactors = FALSE)
ras$P_A<-1 # for abundance
rass<-dplyr::summarize(group_by(ras, TimeWindow, ResArea_Orig,Most_Probable_Topic), Abundance=sum(P_A))
rass$ResArea_Orig<-as.factor(rass$ResArea_Orig)

# create separate data sets for each time window

# with abundance
ras1<-filter(rass,TimeWindow=="1990_1995")
rass1<-dcast(ras1, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum,na.rm=T)
rownames(rass1)<-as.character(rass1$ResArea_Orig)
rass1<-select(rass1,-ResArea_Orig)

#with abundance
ras2<-filter(rass,TimeWindow=="1996_2000")
rass2<-dcast(ras2, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum)
rownames(rass2)<-rass2$ResArea_Orig
rass2<-select(rass2,-ResArea_Orig)

# with abundance
ras3<-filter(rass,TimeWindow=="2001_2005")
rass3<-dcast(ras3, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum)
rownames(rass3)<-rass3$ResArea_Orig
rass3<-select(rass3,-ResArea_Orig)

# with abundance
ras4<-filter(rass,TimeWindow=="2006_2012")
rass4<-dcast(ras4, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum)
rownames(rass4)<-rass4$ResArea_Orig
rass4<-select(rass4,-ResArea_Orig)

########################
# LCBD Beta diversity  #
########################

# time point 1
beta_pa_tw1<-beta.div.comp(rass1, coef="J", quant=FALSE, save.abc=FALSE)
beta_pa_tw1<-beta_pa_tw1$part
beta_pa_tw1$TimeWindow<-"1990_1995"
beta_pa_tw1$Index<-"Jaccard_pa"
beta_pa_tw1<-data.frame(beta_pa_tw1)

beta_abund_tw1<-beta.div.comp(rass1, coef="J", quant=TRUE, save.abc=FALSE)
beta_abund_tw1<-beta_abund_tw1$part
beta_abund_tw1$TimeWindow<-"1990_1995"
beta_abund_tw1$Index<-"Ruzicka_abund"
beta_abund_tw1<-data.frame(beta_abund_tw1)

# time point 2
beta_pa_tw2<-beta.div.comp(rass2, coef="J", quant=FALSE, save.abc=FALSE)
beta_pa_tw2<-beta_pa_tw2$part
beta_pa_tw2$TimeWindow<-"1996_2000"
beta_pa_tw2$Index<-"Jaccard_pa"
beta_pa_tw2<-data.frame(beta_pa_tw2)

beta_abund_tw2<-beta.div.comp(rass2, coef="J", quant=TRUE, save.abc=FALSE)
beta_abund_tw2<-beta_abund_tw2$part
beta_abund_tw2$TimeWindow<-"1996_2000"
beta_abund_tw2$Index<-"Ruzicka_abund"
beta_abund_tw2<-data.frame(beta_abund_tw2)

# time point 3
beta_pa_tw3<-beta.div.comp(rass3, coef="J", quant=FALSE, save.abc=FALSE)
beta_pa_tw3<-beta_pa_tw3$part
beta_pa_tw3$TimeWindow<-"2001_2005"
beta_pa_tw3$Index<-"Jaccard_pa"
beta_pa_tw3<-data.frame(beta_pa_tw3)

beta_abund_tw3<-beta.div.comp(rass3, coef="J", quant=TRUE, save.abc=FALSE)
beta_abund_tw3<-beta_abund_tw3$part
beta_abund_tw3$TimeWindow<-"2001_2005"
beta_abund_tw3$Index<-"Ruzicka_abund"
beta_abund_tw3<-data.frame(beta_abund_tw3)

# time point 4
beta_pa_tw4<-beta.div.comp(rass4, coef="J", quant=FALSE, save.abc=FALSE)
beta_pa_tw4<-beta_pa_tw4$part
beta_pa_tw4$TimeWindow<-"2006_2012"
beta_pa_tw4$Index<-"Jaccard_pa"
beta_pa_tw4<-data.frame(beta_pa_tw4)

beta_abund_tw4<-beta.div.comp(rass4, coef="J", quant=TRUE, save.abc=FALSE)
beta_abund_tw4<-beta_abund_tw4$part
beta_abund_tw4$TimeWindow<-"2006_2012"
beta_abund_tw4$Index<-"Ruzicka_abund"
beta_abund_tw4<-data.frame(beta_abund_tw4)

# merge

beta_integration<-rbind.data.frame(beta_abund_tw1,beta_abund_tw2,beta_abund_tw3,beta_abund_tw4,beta_pa_tw1,beta_pa_tw2,beta_pa_tw3,beta_pa_tw4)

write.csv(beta_integration,"Cleaned_Data/Integration_BetaDiversity_Legendre.csv",row.names=F)
