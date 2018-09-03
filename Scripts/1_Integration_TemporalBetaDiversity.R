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
require(betapart)

################
# DATA         #
# and clean-up #
################

ras<-read.delim("/home/dylan/Dropbox (iDiv)/Research_projects/Integrative_Biodiversity/Data_Code_Analysis/RAs_Topics_analyze.csv",sep=",",header=T)
ras$P_A<-1
ras$Comb<-paste(ras$TimeWindow,ras$ResArea_Orig,sep=".")
ras<-filter(ras,TimeWindow!="")

ra_tm1<-select(ras,TimeWindow,Comb,ResArea_Orig,Most_Probable_Topic,P_A)
ra_tm11<-dcast(ra_tm1,TimeWindow+ResArea_Orig~Most_Probable_Topic,value.var="P_A",sum)

#
ras1<-filter(ra_tm11,TimeWindow=="1990_1995")
rownames(ras1)<-ras1$ResArea_Orig
ras1<-ras1[,3:52]

ras2<-filter(ra_tm11,TimeWindow=="1996_2000")
rownames(ras2)<-ras2$ResArea_Orig
ras2<-ras2[,3:52]

ras3<-filter(ra_tm11,TimeWindow=="2001_2005")
rownames(ras3)<-ras3$ResArea_Orig
ras3<-ras3[,3:52]

ras4<-filter(ra_tm11,TimeWindow=="2006_2012")
rownames(ras4)<-ras4$ResArea_Orig
ras4<-ras4[,3:52]


# temporal turnover

rass1<-ras1
rass1$ResArea_Orig<-rownames(rass1)

rass2<-ras2
rass2$ResArea_Orig<-rownames(rass2)

rass3<-ras3
rass3$ResArea_Orig<-rownames(rass3)

rass4<-ras4
rass4$ResArea_Orig<-rownames(rass4)


#TimeWindow1, TimeWindow2

rass1t<-rass1
rass1t<-melt(rass1t,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass1t$P_A<-ifelse(rass1t$P_A>0,1,0)
rass1t$TimeWindow<-"1990_1995"

rass2t<-rass2
rass2t<-melt(rass2t,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass2t$P_A<-ifelse(rass2t$P_A>0,1,0)
rass2t$TimeWindow<-"1996_2000"

rass3t<-rass3
rass3t<-melt(rass3t,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass3t$P_A<-ifelse(rass3t$P_A>0,1,0)
rass3t$TimeWindow<-"2001_2005"

rass4t<-rass4
rass4t<-melt(rass4,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass4t$P_A<-ifelse(rass4t$P_A>0,1,0)
rass4t$TimeWindow<-"2006_2012"

##### Time 1 v 2

t1_RA<-data.frame(unique(rass1t$ResArea_Orig))
colnames(t1_RA)[1]<-"ResArea_Orig"
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
require(betapart)

################
# DATA         #
# and clean-up #
################

ras<-read.delim("/home/dylan/Dropbox (iDiv)/Research_projects/Integrative_Biodiversity/Data_Code_Analysis/RAs_Topics_analyze.csv",sep=",",header=T)
ras$P_A<-1
ras$Comb<-paste(ras$TimeWindow,ras$ResArea_Orig,sep=".")
ras<-filter(ras,TimeWindow!="")

ra_tm1<-select(ras,TimeWindow,Comb,ResArea_Orig,Most_Probable_Topic,P_A)
ra_tm11<-dcast(ra_tm1,TimeWindow+ResArea_Orig~Most_Probable_Topic,value.var="P_A",sum)

#
ras1<-filter(ra_tm11,TimeWindow=="1990_1995")
rownames(ras1)<-ras1$ResArea_Orig
ras1<-ras1[,3:52]

ras2<-filter(ra_tm11,TimeWindow=="1996_2000")
rownames(ras2)<-ras2$ResArea_Orig
ras2<-ras2[,3:52]

ras3<-filter(ra_tm11,TimeWindow=="2001_2005")
rownames(ras3)<-ras3$ResArea_Orig
ras3<-ras3[,3:52]

ras4<-filter(ra_tm11,TimeWindow=="2006_2012")
rownames(ras4)<-ras4$ResArea_Orig
ras4<-ras4[,3:52]


# temporal turnover

rass1<-ras1
rass1$ResArea_Orig<-rownames(rass1)

rass2<-ras2
rass2$ResArea_Orig<-rownames(rass2)

rass3<-ras3
rass3$ResArea_Orig<-rownames(rass3)

rass4<-ras4
rass4$ResArea_Orig<-rownames(rass4)


#TimeWindow1, TimeWindow2

rass1t<-rass1
rass1t<-melt(rass1t,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass1t$P_A<-ifelse(rass1t$P_A>0,1,0)
rass1t$TimeWindow<-"1990_1995"

rass2t<-rass2
rass2t<-melt(rass2t,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass2t$P_A<-ifelse(rass2t$P_A>0,1,0)
rass2t$TimeWindow<-"1996_2000"

rass3t<-rass3
rass3t<-melt(rass3t,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass3t$P_A<-ifelse(rass3t$P_A>0,1,0)
rass3t$TimeWindow<-"2001_2005"

rass4t<-rass4
rass4t<-melt(rass4,id.vars="ResArea_Orig",measure.vars=1:50,variable.name="Topic",value.name="P_A")
rass4t$P_A<-ifelse(rass4t$P_A>0,1,0)
rass4t$TimeWindow<-"2006_2012"

##### Time 1 v 2

t1_RA<-data.frame(unique(rass1t$ResArea_Orig))
colnames(t1_RA)[1]<-"ResArea_Orig"

t2_RA<-data.frame(unique(rass2t$ResArea_Orig))
colnames(t2_RA)[1]<-"ResArea_Orig"

t1t2<-merge(t1_RA,t2_RA) #ResAreas in common in both time windows

rass1tt<-merge(rass1t,t1t2,by.y=c("ResArea_Orig"))
rass2tt<-merge(rass2t,t1t2,by.y=c("ResArea_Orig"))

rass1tt<-dcast(rass1tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass1tt)<-rass1tt$ResArea_Orig 
rass1tt$ResArea_Orig<-NULL

rass2tt<-dcast(rass2tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass2tt)<-rass2tt$ResArea_Orig 
rass2tt$ResArea_Orig<-NULL

t1t2.t.s <- beta.temp(rass1tt, rass2tt, index.family="sor")
t1t2.t.j <- beta.temp(rass1tt, rass2tt, index.family="jac")

t1t2.t<-cbind.data.frame(t1t2.t.s,t1t2.t.j)
t1t2.t$ResAreas_Orig<-rownames(t1t2.t)
t1t2.t$TimeWindows<-"Time1_Time2"

# Time 2 v 3
t2_RA<-data.frame(unique(rass2t$ResArea_Orig))
colnames(t2_RA)[1]<-"ResArea_Orig"

t3_RA<-data.frame(unique(rass3t$ResArea_Orig))
colnames(t3_RA)[1]<-"ResArea_Orig"

t2t3<-merge(t2_RA,t3_RA) #ResAreas in common in both time windows

rass2tt<-merge(rass2t,t2t3,by.y=c("ResArea_Orig"))
rass3tt<-merge(rass3t,t2t3,by.y=c("ResArea_Orig"))

rass3tt<-dcast(rass3tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass3tt)<-rass3tt$ResArea_Orig 
rass3tt$ResArea_Orig<-NULL

rass2tt<-dcast(rass2tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass2tt)<-rass2tt$ResArea_Orig 
rass2tt$ResArea_Orig<-NULL


t2t3.t.s <- beta.temp(rass2tt, rass3tt, index.family="sor")
t2t3.t.j <- beta.temp(rass2tt, rass3tt, index.family="jac")

t2t3.t<-cbind.data.frame(t2t3.t.s,t2t3.t.j)
t2t3.t$ResAreas_Orig<-rownames(t2t3.t)
t2t3.t$TimeWindows<-"Time2_Time3"

#TimeWindow3,TimeWindow4

t3_RA<-data.frame(unique(rass3t$ResArea_Orig))
colnames(t3_RA)[1]<-"ResArea_Orig"

t4_RA<-data.frame(unique(rass4t$ResArea_Orig))
colnames(t4_RA)[1]<-"ResArea_Orig"

t3t4<-merge(t3_RA,t4_RA) #ResAreas in common in both time windows

rass3tt<-merge(rass3t,t3t4,by.y=c("ResArea_Orig"))
rass4tt<-merge(rass4t,t3t4,by.y=c("ResArea_Orig"))

rass3tt<-dcast(rass3tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass3tt)<-rass3tt$ResArea_Orig 
rass3tt$ResArea_Orig<-NULL

rass4tt<-dcast(rass4tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass4tt)<-rass4tt$ResArea_Orig 
rass4tt$ResArea_Orig<-NULL

t3t4.t.s <- beta.temp(rass3tt, rass4tt, index.family="sor")
t3t4.t.j <- beta.temp(rass3tt, rass4tt, index.family="jac")

t3t4.t<-cbind.data.frame(t3t4.t.s,t3t4.t.j)
t3t4.t$ResAreas_Orig<-rownames(t3t4.t)
t3t4.t$TimeWindows<-"Time3_Time4"

###
betatemp_all<-rbind.data.frame(t1t2.t,t2t3.t,t3t4.t)
write.table(betatemp_all,"/home/dylan/ownCloud/documents/Integrative_Biodiversity/Data_Code_Analysis/Betatemp_RAstopic_31052016.csv",sep=",",row.names=F)

t2_RA<-data.frame(unique(rass2t$ResArea_Orig))
colnames(t2_RA)[1]<-"ResArea_Orig"

t1t2<-merge(t1_RA,t2_RA) #ResAreas in common in both time windows

rass1tt<-merge(rass1t,t1t2,by.y=c("ResArea_Orig"))
rass2tt<-merge(rass2t,t1t2,by.y=c("ResArea_Orig"))

rass1tt<-dcast(rass1tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass1tt)<-rass1tt$ResArea_Orig 
rass1tt$ResArea_Orig<-NULL

rass2tt<-dcast(rass2tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass2tt)<-rass2tt$ResArea_Orig 
rass2tt$ResArea_Orig<-NULL

t1t2.t.s <- beta.temp(rass1tt, rass2tt, index.family="sor")
t1t2.t.j <- beta.temp(rass1tt, rass2tt, index.family="jac")

t1t2.t<-cbind.data.frame(t1t2.t.s,t1t2.t.j)
t1t2.t$ResAreas_Orig<-rownames(t1t2.t)
t1t2.t$TimeWindows<-"Time1_Time2"

# Time 2 v 3
t2_RA<-data.frame(unique(rass2t$ResArea_Orig))
colnames(t2_RA)[1]<-"ResArea_Orig"

t3_RA<-data.frame(unique(rass3t$ResArea_Orig))
colnames(t3_RA)[1]<-"ResArea_Orig"

t2t3<-merge(t2_RA,t3_RA) #ResAreas in common in both time windows

rass2tt<-merge(rass2t,t2t3,by.y=c("ResArea_Orig"))
rass3tt<-merge(rass3t,t2t3,by.y=c("ResArea_Orig"))

rass3tt<-dcast(rass3tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass3tt)<-rass3tt$ResArea_Orig 
rass3tt$ResArea_Orig<-NULL

rass2tt<-dcast(rass2tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass2tt)<-rass2tt$ResArea_Orig 
rass2tt$ResArea_Orig<-NULL


t2t3.t.s <- beta.temp(rass2tt, rass3tt, index.family="sor")
t2t3.t.j <- beta.temp(rass2tt, rass3tt, index.family="jac")

t2t3.t<-cbind.data.frame(t2t3.t.s,t2t3.t.j)
t2t3.t$ResAreas_Orig<-rownames(t2t3.t)
t2t3.t$TimeWindows<-"Time2_Time3"

#TimeWindow3,TimeWindow4

t3_RA<-data.frame(unique(rass3t$ResArea_Orig))
colnames(t3_RA)[1]<-"ResArea_Orig"

t4_RA<-data.frame(unique(rass4t$ResArea_Orig))
colnames(t4_RA)[1]<-"ResArea_Orig"

t3t4<-merge(t3_RA,t4_RA) #ResAreas in common in both time windows

rass3tt<-merge(rass3t,t3t4,by.y=c("ResArea_Orig"))
rass4tt<-merge(rass4t,t3t4,by.y=c("ResArea_Orig"))

rass3tt<-dcast(rass3tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass3tt)<-rass3tt$ResArea_Orig 
rass3tt$ResArea_Orig<-NULL

rass4tt<-dcast(rass4tt,ResArea_Orig~Topic,value.var="P_A",sum)  
rownames(rass4tt)<-rass4tt$ResArea_Orig 
rass4tt$ResArea_Orig<-NULL

t3t4.t.s <- beta.temp(rass3tt, rass4tt, index.family="sor")
t3t4.t.j <- beta.temp(rass3tt, rass4tt, index.family="jac")

t3t4.t<-cbind.data.frame(t3t4.t.s,t3t4.t.j)
t3t4.t$ResAreas_Orig<-rownames(t3t4.t)
t3t4.t$TimeWindows<-"Time3_Time4"

###
betatemp_all<-rbind.data.frame(t1t2.t,t2t3.t,t3t4.t)
write.table(betatemp_all,"/home/dylan/ownCloud/documents/Integrative_Biodiversity/Data_Code_Analysis/Betatemp_RAstopic_31052016.csv",sep=",",row.names=F)
