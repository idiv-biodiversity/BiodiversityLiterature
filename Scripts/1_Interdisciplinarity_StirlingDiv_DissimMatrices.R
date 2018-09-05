##################################
# Interdisciplinarity            #
# diversity of subdisciplines in #
# concepts                       #
# Step 1: Calculate disparity    #
# matrix based on citation flow  #
# across WoS categories          #
##################################
# NOTE: files too big for GitHub #
##################################

require(dplyr)
require(stringr)
require(tidyr)
require(reshape2)
require(vegan)
require(proxy)

# Data

ra_tmm<-read.delim("Data/RAs_Topics_analyze.csv",sep=",",header=T) # article classification from LDA model
ra_tmm$UID_Orig<-str_trim(ra_tmm$UID_Orig, side = "both")
uids<-unique(as.character(ra_tmm$UID_Original)) # get UIDs used in final analysis

##################
# time window 1  #
##################

w1<-read.csv("Data/19901995_fixed.csv",header=T,stringsAsFactors = FALSE) # original data from WoS
w11<-select(w1, UID_Orig, ResArea_Orig, UID_Cited, ResArea_Cited)

ress<-unique(w11$ResArea_Orig)
w12<- filter(w11,ResArea_Cited %in% ress)%>%
  filter(., UID_Orig %in% uids)

ress1<-unique(w12$ResArea_Orig)
w12<- filter(w12, ResArea_Cited %in%ress1)
w12$PA<-1

w122<-dcast(w12, ResArea_Orig~  ResArea_Cited,value.var="PA",sum)
rownames(w122)<-w122$ResArea_Orig
w122<-select(w122,-ResArea_Orig)

w122m<-as.matrix(w122) # for jaccard

w122mm<-w122  # make binary for bray
w122mm[w122mm>=1] <- 1
w122mm<-as.matrix(w122mm)

# calculate disparity matrices

tw1990_dist<-vegdist(w122m, method="jaccard") 
tw1990_dist_bin<-vegdist(w122mm, method="bray") 
tw1990_dist_cos<-dist(w122m, method="cosine") 

##################
# time window 2  #
##################

w2<-read.csv("Data/19962000_fixed.csv",header=T,stringsAsFactors = FALSE) # original data from WoS
w22<-select(w2, UID_Orig, ResArea_Orig, UID_Cited, ResArea_Cited)

ress2<-unique(w22$ResArea_Orig)
w222<- filter(w22,ResArea_Cited %in% ress2)%>%
  filter(., UID_Orig %in% uids)

ress22<-unique(w222$ResArea_Orig)
w222<- filter(w222, ResArea_Cited %in%ress22)
w222$PA<-1

w223<-dcast(w222, ResArea_Orig~  ResArea_Cited,value.var="PA",sum)
rownames(w223)<-w223$ResArea_Orig
w223<-select(w223,-ResArea_Orig)
w223m<-as.matrix(w223)

w223mm<-w223  # make binary for bray
w223mm[w223mm>=1] <- 1
w223mm<-as.matrix(w223mm)

# calculate disparity matrices

tw1995_dist<-vegdist(w223m, method="jaccard") 
tw1995_dist_bin<-vegdist(w223mm, method="bray") 
tw1995_dist_cos<-dist(w223m, method="cosine")

##################
# time window 3  #
##################

w3<-read.csv("Data/20012005_fixed.csv",header=T,stringsAsFactors = FALSE) # original data from WoS
w33<-select(w3, UID_Orig, ResArea_Orig, UID_Cited, ResArea_Cited)

ress3<-unique(w33$ResArea_Orig)
w333<- filter(w33,ResArea_Cited %in% ress3)%>%
  filter(., UID_Orig %in% uids)

ress33<-unique(w333$ResArea_Orig)
w333<- filter(w333, ResArea_Cited %in%ress33)
w333$PA<-1

w334<-dcast(w333, ResArea_Orig~  ResArea_Cited,value.var="PA",sum)
rownames(w334)<-w334$ResArea_Orig
w334<-select(w334,-ResArea_Orig)
w334m<-as.matrix(w334)

w334mm<-w334  # make binary for bray
w334mm[w334mm>=1] <- 1
w334mm<-as.matrix(w334mm)

tw2000_dist<-vegdist(w334, method="jaccard") 
tw2000_dist_bin<-vegdist(w334mm, method="bray") 
tw2000_dist_cos<-dist(w334m, method="cosine")

#################
# time window 4 #
#################

w4<-read.csv("Data/20062012fixed.csv",header=T,stringsAsFactors = FALSE) # original data from WoS
w44<-select(w4, UID_Orig, ResArea_Orig, UID_Cited, ResArea_Cited)

ress4<-unique(w44$ResArea_Orig)
w444<- filter(w44,ResArea_Cited %in% ress4)%>%
  filter(., UID_Orig %in% uids)

ress44<-unique(w444$ResArea_Orig)
w444<- filter(w444, ResArea_Cited %in% ress44)
w444$PA<-1

w445<-dcast(w444, ResArea_Orig~  ResArea_Cited,value.var="PA",sum)
rownames(w445)<-w445$ResArea_Orig
w445<-select(w445,-ResArea_Orig)
w445m<-as.matrix(w445)

w445mm<-w445  # make binary for bray
w445mm[w445mm>=1] <- 1
w445mm<-as.matrix(w445mm)

tw2006_dist<-vegdist(w445m, method="jaccard") 
tw2006_dist_bin<-vegdist(w445mm, method="bray") 
tw2006_dist_cos<-dist(w445m, method="cosine")

#################
# Quality Check #
#################

# total articles in topic model

length(unique(ra_tmm$UID_Original))
#97945  

#total articles in across time windows (for disparity matrix)

length(unique(w12$UID_Orig))+length(unique(w222$UID_Orig))+length(unique(w333$UID_Orig))+length(unique(w444$UID_Orig))
# 97945

# total number of research areas across all time windows

res_areas1<-unique(select(w12,ResArea_Orig))
res_areas2<-unique(select(w222,ResArea_Orig))
res_areas3<-unique(select(w333,ResArea_Orig))
res_areas4<-unique(select(w444,ResArea_Orig))

res_areas<-rbind.data.frame(res_areas1, res_areas2, res_areas3, res_areas4)
res_areas<-distinct(res_areas)
length(unique(res_areas$ResArea_Orig))
#163

length(unique(ra_tmm$ResArea_Orig))
#163

#######

save(tw1990_dist,tw1995_dist,tw2000_dist,tw2006_dist,
     tw1990_dist_bin,tw1995_dist_bin,tw2000_dist_bin,tw2006_dist_bin,
     tw1990_dist_cos,tw1995_dist_cos,tw2000_dist_cos,tw2006_dist_cos,
     file="Cleaned_Data/RA_dissim_matrices.RData")
