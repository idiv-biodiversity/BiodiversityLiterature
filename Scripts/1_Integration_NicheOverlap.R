######################################################
# Estimate niche overlap using bipartite networks   #
# where concepts (results of LDA topic model) and   #
# subdisciplines are treated as belonging to        #
# different trophic levels                          #
# abundance = number of published articles          #
#####################################################

require(dplyr)
require(reshape2)
require(bipartite)

#########
# Data  #
#########

ra_tmm<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T,stringsAsFactors = FALSE)
ra_tmm$P_A<-1

########################
# create networks     ##
########################

rass<-summarize(group_by(ra_tmm, TimeWindow, ResArea_Orig,Most_Probable_Topic), Abundance=sum(P_A))
rass$ResArea_Orig<-as.factor(rass$ResArea_Orig)
rass<-select(rass,higher=ResArea_Orig,lower=Most_Probable_Topic,webID=TimeWindow,freq=Abundance)

#Time Window 1

m_b1<-filter(rass,webID=="1990_1995")
m_b1<-dcast(m_b1,lower~higher,value.var="freq",sum)
rownames(m_b1)<-m_b1$lower
m_b1$lower<-NULL
m_b1<-as.matrix(m_b1)

#Time Window 2
m_b2<-filter(rass,webID=="1996_2000")
m_b2<-dcast(m_b2,lower~higher,value.var="freq",sum)
rownames(m_b2)<-m_b2$lower
m_b2$lower<-NULL
m_b2<-as.matrix(m_b2)

#Time Window 3
m_b3<-filter(rass,webID=="2001_2005")
m_b3<-dcast(m_b3,lower~higher,value.var="freq",sum)
rownames(m_b3)<-m_b3$lower
m_b3$lower<-NULL
m_b3<-as.matrix(m_b3)

#Time Window 4
m_b4<-filter(rass,webID=="2006_2012")
m_b4<-dcast(m_b4,lower~higher,value.var="freq",sum)
rownames(m_b4)<-m_b4$lower
m_b4$lower<-NULL
m_b4<-as.matrix(m_b4)

#######################################################
# Bi-partite network indices (for each time window)   #
#######################################################

net_indd_null<-null.t.test(m_b1,index=c("connectance", "Shannon diversity","Fisher alpha", "H2", "niche overlap", "weighted nestedness","weighted NODF"),
                           weighted=TRUE,N=10)
net_indd_null<-data.frame(net_indd_null)
net_indd_null$Index<-rownames(net_indd_null)
net_indd_null<-select(net_indd_null, obs,Index)%>%
               filter(., Index=="niche.overlap.HL"|Index=="niche.overlap.LL")
net_indd_null$TimeWindow<-"1990_1995"


net_indd_null_2<-null.t.test(m_b2,index=c("connectance", "Shannon diversity","Fisher alpha", "H2", "niche overlap", "weighted nestedness","weighted NODF"),
                           weighted=TRUE,N=10)
net_indd_null_2<-data.frame(net_indd_null_2)
net_indd_null_2$Index<-rownames(net_indd_null_2)
net_indd_null_2<-select(net_indd_null_2, obs,Index)%>%
  filter(., Index=="niche.overlap.HL"|Index=="niche.overlap.LL")
net_indd_null_2$TimeWindow<-"1996_2000"

net_indd_null_3<-null.t.test(m_b3,index=c("connectance", "Shannon diversity","Fisher alpha", "H2", "niche overlap", "weighted nestedness","weighted NODF"),
                             weighted=TRUE,N=10)
net_indd_null_3<-data.frame(net_indd_null_3)
net_indd_null_3$Index<-rownames(net_indd_null_3)
net_indd_null_3<-select(net_indd_null_3, obs,Index)%>%
  filter(., Index=="niche.overlap.HL"|Index=="niche.overlap.LL")
net_indd_null_3$TimeWindow<-"2001_2005"

net_indd_null_4<-null.t.test(m_b4,index=c("connectance", "Shannon diversity","Fisher alpha", "H2", "niche overlap", "weighted nestedness","weighted NODF"),
                             weighted=TRUE,N=10)
net_indd_null_4<-data.frame(net_indd_null_4)
net_indd_null_4$Index<-rownames(net_indd_null_4)
net_indd_null_4<-select(net_indd_null_4, obs,Index)%>%
  filter(., Index=="niche.overlap.HL"|Index=="niche.overlap.LL")
net_indd_null_4$TimeWindow<-"2006_2012"

net_bipart<-rbind.data.frame(net_indd_null,net_indd_null_2,net_indd_null_3,net_indd_null_4)

write.csv(net_bipart,"Cleaned_Data/RAtopic_bipartite_indices.csv",row.names=F)
