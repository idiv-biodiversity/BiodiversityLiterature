###############################################
# Merge alpha diversity (rarefied:            #
# Concept diversity & Interdisciplinarity     #
###############################################

require(cowplot)

load(file="Cleaned_Data/Interdisc_Rare.RData")
load(file="Cleaned_Data/Integr_Rare.RData")

# merge figures

int_tog<-plot_grid(DivParts_integr + theme(legend.position="none", 
                                    panel.background = element_blank(),
                                    panel.grid.major = element_blank()),
                   DivParts_n +theme(legend.position="none", 
                                         panel.background = element_blank(),
                                         panel.grid.major = element_blank()),
                   labels=c("a)","b)"),label_size=6,align="vh",ncol=2)

ggsave(filename = file.path("Figures", "Concept_Interdiscp_alpha_Srare.png"), 
       width    = 6.5, 
       height   = 4, 
       units    = "in")

int_tog

dev.off()