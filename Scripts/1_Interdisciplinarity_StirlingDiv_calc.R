##################################
# Interdisciplinarity            #
# diversity of subdisciplines in #
# concepts                       #
# Step 2: Calculate Stirling     #
# diversity using multiply       #
# disparity matrices             #
##################################

require(tidyr)
require(dplyr)
require(diverse)

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

##############
# data       #
##############

# load terms from LDA model output
terms<-read.delim('Data/vis_50_1.0_50.csv',sep=";",header=T)
terms<-select(terms,Most_Probable_Topic=num_topics,timepoint,term,probability)
terms$term<-droplevels(terms$term)
terms$term<-as.character(terms$term)

topz<-distinct(select(terms, TimeWindow=timepoint, Most_Probable_Topic))

##############
# Abundance ##
# per Topic ##
##############

# load research areas & articles
ra_tmm<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T)
ra_tmm$P_A<-1

ra_tmm<-arrange(ra_tmm, UID_Original)
ra_tmm2<-select(ra_tmm,TimeWindow,Most_Probable_Topic,ResArea_Orig,UID_Original,P_A)

ra_tmm22<-summarize(group_by(ra_tmm2,TimeWindow,Most_Probable_Topic,ResArea_Orig), Abundance=sum(P_A))

ra_tmm22$TimeWindow<-as.character(ra_tmm22$TimeWindow)
ra_tmm22$TimeWindow<-ifelse(ra_tmm22$TimeWindow=="1990_1995","0",ra_tmm22$TimeWindow)
ra_tmm22$TimeWindow<-ifelse(ra_tmm22$TimeWindow=="1996_2000","1",ra_tmm22$TimeWindow)
ra_tmm22$TimeWindow<-ifelse(ra_tmm22$TimeWindow=="2001_2005","2",ra_tmm22$TimeWindow)
ra_tmm22$TimeWindow<-ifelse(ra_tmm22$TimeWindow=="2006_2012","3",ra_tmm22$TimeWindow)

ra_tmm22$TimeWindow<-as.integer(ra_tmm22$TimeWindow)
ra_tmm22$Most_Probable_Topic<-as.integer(ra_tmm22$Most_Probable_Topic)

########################
# Disparity Matrices #
########################

load(file="Cleaned_Data/RA_dissim_matrices.RData")

#####################################
# Calculate Stirling diversity      #
# using different disparity matrices#
#####################################


#####################################
# with jaccard dissimilarity        #
#####################################

ra_tmm24<-left_join(topz, ra_tmm22, by=c("TimeWindow","Most_Probable_Topic"))%>%
  filter(., is.na(ResArea_Orig)==FALSE)

# Time Window 1

ra_tm_tw1<-filter(ra_tmm24, TimeWindow==0)
ra_tm_tw1<-select(ra_tm_tw1, entity=Most_Probable_Topic, category=ResArea_Orig,value=Abundance)

ra_tm_tw1$entity<-as.character(ra_tm_tw1$entity)
ra_tm_tw1$category<-droplevels(ra_tm_tw1$category)
ra_tm_tw1$category<-as.character(ra_tm_tw1$category)

tw1990_dist_m <- as.matrix(tw1990_dist)

sd<-diversity(ra_tm_tw1, type="rs", dis=tw1990_dist_m,alpha=1,beta=1)
sd$Most_Probable_Topic<-rownames(sd)
sd$TimeWindow<-"1990_1995"
sd<-select(sd, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 2

ra_tm_tw2<-filter(ra_tmm24, TimeWindow==1)
ra_tm_tw2<-select(ra_tm_tw2, entity=Most_Probable_Topic, category=ResArea_Orig,value=Abundance)

ra_tm_tw2$entity<-as.character(ra_tm_tw2$entity)
ra_tm_tw2$category<-droplevels(ra_tm_tw2$category)
ra_tm_tw2$category<-as.character(ra_tm_tw2$category)

tw1995_dist_m <- as.matrix(tw1995_dist)

sd2<-diversity(ra_tm_tw2, type="rs", dis=tw1995_dist_m,alpha=1,beta=1)
sd2$Most_Probable_Topic<-rownames(sd2)
sd2$TimeWindow<-"1996_2000"
sd2<-select(sd2, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 3

ra_tm_tw3<-filter(ra_tmm24, TimeWindow==2)
ra_tm_tw3<-select(ra_tm_tw3, entity=Most_Probable_Topic, category=ResArea_Orig,value=Abundance)

ra_tm_tw3$entity<-as.character(ra_tm_tw3$entity)
ra_tm_tw3$category<-droplevels(ra_tm_tw3$category)
ra_tm_tw3$category<-as.character(ra_tm_tw3$category)

tw2000_dist_m <- as.matrix(tw2000_dist)

sd3<-diversity(ra_tm_tw3, type="rs", dis=tw2000_dist_m,alpha=1,beta=1)
sd3$Most_Probable_Topic<-rownames(sd3)
sd3$TimeWindow<-"2001_2005"
sd3<-select(sd3, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 4

ra_tm_tw4<-filter(ra_tmm24, TimeWindow==3)
ra_tm_tw4<-select(ra_tm_tw4, entity=Most_Probable_Topic, category=ResArea_Orig,value=Abundance)

ra_tm_tw4$entity<-as.character(ra_tm_tw4$entity)
ra_tm_tw4$category<-droplevels(ra_tm_tw4$category)
ra_tm_tw4$category<-as.character(ra_tm_tw4$category)

tw2006_dist_m <- as.matrix(tw2006_dist)

sd4<-diversity(ra_tm_tw4, type="rs", dis=tw2006_dist_m,alpha=1,beta=1)
sd4$Most_Probable_Topic<-rownames(sd4)
sd4$TimeWindow<-"2006_2012"
sd4<-select(sd4, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# combine output

sd_jaccard<-rbind.data.frame(sd, sd2, sd3, sd4)

###############################################
# with bray dissimilarity #####################
###############################################

# Time Window 1

tw1990_dist_mm <- as.matrix(tw1990_dist_bin)

sdd<-diversity(ra_tm_tw1, type="rs", dis=tw1990_dist_mm,alpha=1,beta=1)
sdd$Most_Probable_Topic<-rownames(sdd)
sdd$TimeWindow<-"1990_1995"
sdd<-select(sdd, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 2

tw1995_dist_mm <- as.matrix(tw1995_dist_bin)

sdd2<-diversity(ra_tm_tw2, type="rs", dis=tw1995_dist_mm,alpha=1,beta=1)
sdd2$Most_Probable_Topic<-rownames(sdd2)
sdd2$TimeWindow<-"1996_2000"
sdd2<-select(sdd2, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 3

tw2000_dist_mm <- as.matrix(tw2000_dist_bin)

sdd3<-diversity(ra_tm_tw3, type="rs", dis=tw2000_dist_mm,alpha=1,beta=1)
sdd3$Most_Probable_Topic<-rownames(sdd3)
sdd3$TimeWindow<-"2001_2005"
sdd3<-select(sdd3, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 4

tw2006_dist_mm <- as.matrix(tw2006_dist_bin)

sdd4<-diversity(ra_tm_tw4, type="rs", dis=tw2006_dist_mm,alpha=1,beta=1)
sdd4$Most_Probable_Topic<-rownames(sdd4)
sdd4$TimeWindow<-"2006_2012"
sdd4<-select(sdd4, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# combine output

sd_bray<-rbind.data.frame(sdd, sdd2, sdd3, sdd4)

################################################
# with cosine dissimilarity ####################
################################################

# Time Window 1

tw1990_dist_mm <- as.matrix(tw1990_dist_cos)

sdd11<-diversity(ra_tm_tw1, type="rs", dis=tw1990_dist_mm,alpha=1,beta=1)
sdd11$Most_Probable_Topic<-rownames(sdd11)
sdd11$TimeWindow<-"1990_1995"
sdd11<-select(sdd11, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 2

tw1995_dist_mm <- as.matrix(tw1995_dist_cos)

sdd22<-diversity(ra_tm_tw2, type="rs", dis=tw1995_dist_mm,alpha=1,beta=1)
sdd22$Most_Probable_Topic<-rownames(sdd22)
sdd22$TimeWindow<-"1996_2000"
sdd22<-select(sdd22, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 3

tw2000_dist_mm <- as.matrix(tw2000_dist_cos)

sdd33<-diversity(ra_tm_tw3, type="rs", dis=tw2000_dist_mm,alpha=1,beta=1)
sdd33$Most_Probable_Topic<-rownames(sdd33)
sdd33$TimeWindow<-"2001_2005"
sdd33<-select(sdd33, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# Time Window 4

tw2006_dist_mm <- as.matrix(tw2006_dist_cos)

sdd44<-diversity(ra_tm_tw4, type="rs", dis=tw2006_dist_mm,alpha=1,beta=1)
sdd44$Most_Probable_Topic<-rownames(sdd44)
sdd44$TimeWindow<-"2006_2012"
sdd44<-select(sdd44, TimeWindow, Most_Probable_Topic, RaoStirling=rao.stirling)

# combine output

sd_cosine<-rbind.data.frame(sdd11, sdd22, sdd33, sdd44)

################################################

save(sd_jaccard,sd_bray, sd_cosine,file="Cleaned_Data/Interdisciplinarity_StirlingDiversity.RData")

