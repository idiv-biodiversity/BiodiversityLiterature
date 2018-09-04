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
require(rms)

################
# DATA         #
# and clean-up #
################

ras<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T,stringsAsFactors = FALSE)
ras$P_A<-1 # for abundance
rass<-dplyr::summarize(group_by(ras, TimeWindow, ResArea_Orig,Most_Probable_Topic), Abundance=sum(P_A))
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
tw1.abund.beta.out$TimeWindow<-tw1

tw1.pa.beta<-beta.sample(rass1.pa, index="jaccard",sites=nrow(rass1.pa)*0.50,samples=1000)
tw1.pa.beta.out<-rbind(smean.cl.boot(tw1.pa.beta$sampled.values$beta.JTU,B=1000),smean.cl.boot(tw1.pa.beta$sampled.values$beta.JNE,B=1000),
         smean.cl.boot(tw1.pa.beta$sampled.values$beta.JAC,B=1000))
tw1.pa.beta.out<-data.frame(tw1.pa.beta.out)
tw1.pa.beta.out$Beta_Part<-c("JTU","JNE","JAC")
tw1.pa.beta.out$TimeWindow<-tw1

# Time window 2

tw2.abund.beta<-beta.sample.abund(rass2, index="ruzicka",sites=nrow(rass2)*0.50, samples=1000)
tw2.abund.beta.out<-rbind(smean.cl.boot(tw2.abund.beta$sampled.values$beta.RUZ.BAL,B=1000),smean.cl.boot(tw2.abund.beta$sampled.values$beta.RUZ.GRA,B=1000),
                          smean.cl.boot(tw2.abund.beta$sampled.values$beta.RUZ,B=1000))
tw2.abund.beta.out<-data.frame(tw2.abund.beta.out)
tw2.abund.beta.out$Beta_Part<-c("BAL","GRA","RUZ")
tw2.abund.beta.out$TimeWindow<-tw3

tw2.pa.beta<-beta.sample(rass2.pa, index="jaccard",sites=nrow(rass2.pa)*0.50,samples=1000)
tw2.pa.beta.out<-rbind(smean.cl.boot(tw2.pa.beta$sampled.values$beta.JTU,B=1000),smean.cl.boot(tw2.pa.beta$sampled.values$beta.JNE,B=1000),
                       smean.cl.boot(tw2.pa.beta$sampled.values$beta.JAC,B=1000))
tw2.pa.beta.out<-data.frame(tw2.pa.beta.out)
tw2.pa.beta.out$Beta_Part<-c("JTU","JNE","JAC")
tw2.pa.beta.out$TimeWindow<-tw2


# Time window 3

tw3.abund.beta<-beta.sample.abund(rass3, index="ruzicka",sites=nrow(rass3)*0.50, samples=1000)
tw3.abund.beta.out<-rbind(smean.cl.boot(tw3.abund.beta$sampled.values$beta.RUZ.BAL,B=1000),smean.cl.boot(tw3.abund.beta$sampled.values$beta.RUZ.GRA,B=1000),
                          smean.cl.boot(tw3.abund.beta$sampled.values$beta.RUZ,B=1000))
tw3.abund.beta.out<-data.frame(tw3.abund.beta.out)
tw3.abund.beta.out$Beta_Part<-c("BAL","GRA","RUZ")
tw3.abund.beta.out$TimeWindow<-tw3

tw3.pa.beta<-beta.sample(rass3.pa, index="jaccard",sites=nrow(rass3.pa)*0.50,samples=1000)
tw3.pa.beta.out<-rbind(smean.cl.boot(tw3.pa.beta$sampled.values$beta.JTU,B=1000),smean.cl.boot(tw3.pa.beta$sampled.values$beta.JNE,B=1000),
                       smean.cl.boot(tw3.pa.beta$sampled.values$beta.JAC,B=1000))
tw3.pa.beta.out<-data.frame(tw3.pa.beta.out)
tw3.pa.beta.out$Beta_Part<-c("JTU","JNE","JAC")
tw3.pa.beta.out$TimeWindow<-tw3


# Time window 4

tw4.abund.beta<-beta.sample.abund(rass4, index="ruzicka",sites=nrow(rass4)*0.50, samples=1000)
tw4.abund.beta.out<-rbind(smean.cl.boot(tw4.abund.beta$sampled.values$beta.RUZ.BAL,B=1000),smean.cl.boot(tw4.abund.beta$sampled.values$beta.RUZ.GRA,B=1000),
                          smean.cl.boot(tw4.abund.beta$sampled.values$beta.RUZ,B=1000))
tw4.abund.beta.out<-data.frame(tw4.abund.beta.out)
tw4.abund.beta.out$Beta_Part<-c("BAL","GRA","RUZ")
tw4.abund.beta.out$TimeWindow<-tw4

tw4.pa.beta<-beta.sample(rass4.pa, index="jaccard",sites=nrow(rass4.pa)*0.50,samples=1000)
tw4.pa.beta.out<-rbind(smean.cl.boot(tw4.pa.beta$sampled.values$beta.JTU,B=1000),smean.cl.boot(tw4.pa.beta$sampled.values$beta.JNE,B=1000),
                       smean.cl.boot(tw4.pa.beta$sampled.values$beta.JAC,B=1000))
tw4.pa.beta.out<-data.frame(tw4.pa.beta.out)
tw4.pa.beta.out$Beta_Part<-c("JTU","JNE","JAC")
tw4.pa.beta.out$TimeWindow<-tw4

## combmine results and write file

betadiv_all<-rbind.data.frame(tw1.abund.beta.out,tw1.pa.beta.out,tw2.abund.beta.out,tw2.pa.beta.out,tw3.abund.beta.out,tw3.pa.beta.out,tw4.abund.beta.out,tw4.pa.beta.out)
write.table(betadiv_all,"Cleaned_Data/Betadiv_RAstopic.csv",sep=",",row.names=F)
