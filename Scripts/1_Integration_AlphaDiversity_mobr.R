#####################################################
#  Integration:                                     #
# alpha        diversity                            #
# across & within biodiversity sub-disciplines      # 
# species = concepts (results of LDA topic model)   #
# communities = biodiversity sub-disciplines        #
# abundance = number of published articles          #
#####################################################

require(dplyr)
require(tidyr)
require(reshape2)
require(mobr)

################
# DATA         #
# and clean-up #
################

ras<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T,stringsAsFactors = FALSE)
ras$P_A<-1 # for abundance
rass<-summarize(group_by(ras, TimeWindow, ResArea_Orig,Most_Probable_Topic), Abundance=sum(P_A))
rass$ResArea_Orig<-as.factor(rass$ResArea_Orig)
rass<-unite(rass, "time_ResAreas", c("TimeWindow","ResArea_Orig"),sep = ":",remove=FALSE)

# create site x spp matrices

h_comm<-dcast(rass, time_ResAreas~Most_Probable_Topic, value.var="Abundance",sum)
rownames(h_comm)<-h_comm$time_ResAreas
h_comm<-select(h_comm,-time_ResAreas)

# create attribute objects

groupss<-as.character(unique(rass$time_ResAreas))

# make mob structure

h_stats<-calc_biodiv(h_comm, groups=groupss,effort=25,extrapolate=TRUE,index=c("N","S","S_n","S_asymp","S_PIE"),return_NA=FALSE)
h_stats$effort<-NULL
h_statss<-spread(h_stats, index, value, fill = NA, convert = FALSE)
h_statss$S_PIE<-ifelse(is.na(h_statss$S_PIE)==TRUE,h_statss$S,h_statss$S_PIE)
h_statss<-h_statss%>%
          separate(group, into=c("TimeWindow","ResArea_Orig"), sep=":",remove=FALSE, convert=FALSE)

h_statss2<-h_statss%>%
           gather(group, Value, N:S_n)

#####################
# write out results #
#####################

write.csv(h_statss2,"Cleaned_Data/integration_AlphaDiv_orders_mobr.csv",row.names=F)