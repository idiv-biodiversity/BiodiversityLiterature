####################################
# prep data for interactive figure #
####################################
# a couple of notes:
# 7 concepts couldn't be classified as increasing/decreasing b/c
# ENS_PIE values are N/A (infinite prob. of finding another species)
#

require(dplyr)
require(tidyr)

########
# data #
########

alpha_div<-read.csv("Cleaned_Data/interdisciplinarity_AlphaDiv_orders_mobr.csv",stringsAsFactors = FALSE)

load(file="Cleaned_Data/Concepts_Quantile_S_ENSPIE.RData")

# 
alpha_div_Spie<-filter(alpha_div, group=="S_PIE") # for ENSpie only
alpha_div_SSpie<- alpha_div_Spie%>%
  spread(TimeWindow, Value,fill=NA,convert=FALSE)

alpha_div_SSpie[is.na(alpha_div_SSpie)] <- 0 # this is only to give it a value so it appears in interactive figure

#
inter_SPIE_quants<-select(inter_SPIE_quants,Most_Probable_Topic, Quantile)
inter_SPIE_quants$Most_Probable_Topic<-as.integer(inter_SPIE_quants$Most_Probable_Topic)

# merge

inter_disp<-left_join(alpha_div_SSpie,inter_SPIE_quants, by.y="Most_Probable_Topic")

write.csv(inter_disp, "Cleaned_Data/InteractiveFigure_Data.csv",row.names=FALSE)
