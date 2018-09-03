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
require(boot)

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

# with abundance
ras1<-filter(rass,TimeWindow=="1990_1995")
rass1<-dcast(ras1, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum,na.rm=T)
rownames(rass1)<-as.character(rass1$ResArea_Orig)
rass1<-select(rass1,-ResArea_Orig)

# just p/a

rass1.pa<-rass1
rass1.pa[rass1.pa>=1] <- 1

#with abundance
ras2<-filter(rass,TimeWindow=="1996_2000")
rass2<-dcast(ras2, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum)
rownames(rass2)<-rass2$ResArea_Orig
rass2<-select(rass2,-ResArea_Orig)

# just p/a
rass2.pa<-rass2
rass2.pa[rass2.pa>=1] <- 1

# with abundance
ras3<-filter(rass,TimeWindow=="2001_2005")
rass3<-dcast(ras3, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum)
rownames(rass3)<-rass3$ResArea_Orig
rass3<-select(rass3,-ResArea_Orig)

# just p/a
rass3.pa<-rass3
rass3.pa[rass3.pa>=1] <- 1

# with abundance
ras4<-filter(rass,TimeWindow=="2006_2012")
rass4<-dcast(ras4, ResArea_Orig~Most_Probable_Topic, value.var="Abundance",sum)
rownames(rass4)<-rass4$ResArea_Orig
rass4<-select(rass4,-ResArea_Orig)

# just p/a
rass4.pa<-rass4
rass4.pa[rass4.pa>=1] <- 1


####################################
# Calculate beta diversity indices #
# with and without abundance       #
# included nested/turnover comp.   #
####################################

# Time window 1

tw1.abund.beta<-beta.sample.abund(rass1, index="ruzicka",sites=nrow(rass1)*0.50, samples=1000)
tw1.abund.beta.out<-rbind(smean.cl.boot(tw1.abund.beta$sampled.values$beta.RUZ.BAL,B=1000),smean.cl.boot(tw1.abund.beta$sampled.values$beta.RUZ.GRA,B=1000),
                       smean.cl.boot(tw1.abund.beta$sampled.values$beta.RUZ,B=1000))
tw1.abund.beta.out<-data.frame(tw1.abund.beta.out)
tw1.abund.beta.out$Beta_Part<-c("BAL","GRA","RUZ")


tw1.pa.beta<-beta.sample(rass1.pa, index="jaccard",sites=nrow(rass1.pa)*0.50,samples=1000)
tw1.pa.beta.out<-rbind(smean.cl.boot(tw1.pa.beta$sampled.values$beta.JTU,B=1000),smean.cl.boot(tw1.pa.beta$sampled.values$beta.JNE,B=1000),
         smean.cl.boot(tw1.pa.beta$sampled.values$beta.JAC,B=1000))
tw1.pa.beta.out<-data.frame(tw1.pa.beta.out)
tw1.pa.beta.out$Beta_Part<-c("JTU","JNE","JAC")

# stop here
# Time window 2

tw1.abund.beta<-beta.sample.abund(rass1, index="ruzicka",sites=nrow(rass1)*0.50, samples=1000)

tw1.pa.beta<-beta.sample(rass1.pa, index="jaccard",sites=nrow(rass1.pa)*0.50,samples=1000)

# Time window 3

tw1.abund.beta<-beta.sample.abund(rass1, index="ruzicka",sites=nrow(rass1)*0.50, samples=1000)

tw1.pa.beta<-beta.sample(rass1.pa, index="jaccard",sites=nrow(rass1.pa)*0.50,samples=1000)

# Time window 4

tw1.abund.beta<-beta.sample.abund(rass1, index="ruzicka",sites=nrow(rass1)*0.50, samples=1000)

tw1.pa.beta<-beta.sample(rass1.pa, index="jaccard",sites=nrow(rass1.pa)*0.50,samples=1000)



###
betatemp_all<-rbind.data.frame(t1t2.t,t2t3.t,t3t4.t)
write.table(betatemp_all,"/home/dylan/ownCloud/documents/Integrative_Biodiversity/Data_Code_Analysis/Betatemp_RAstopic_31052016.csv",sep=",",row.names=F)
