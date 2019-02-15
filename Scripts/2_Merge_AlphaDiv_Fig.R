###############################################
# Merge alpha diversity:                      #
# Concept diversity & Interdisciplinarity     #
###############################################

require(cowplot)

load(file="Cleaned_Data/Integration_Div.RData")
load(file="Cleaned_Data/Interdisc_Div.RData")

# merge figures

int_tog<-plot_grid(DivParts + theme(legend.position="none", 
                                    panel.background = element_blank(),
                                    panel.grid.major = element_blank()),
                   Inter_DivParts +theme(legend.position="none", 
                                         panel.background = element_blank(),
                                         panel.grid.major = element_blank()),
                   labels=c("a)","b)"),label_size=6,align="vh",ncol=2)

legend <- get_legend(DivParts + theme(legend.position="top",legend.direction="horizontal", legend.text=element_text(size=6)))

int_togg <- plot_grid(legend,int_tog,rel_heights = c(0.27,5),ncol=1)

ggsave(filename = file.path("Figures", "Concept_Interdiscp_alpha.png"), 
       width    = 6.5, 
       height   = 4, 
       units    = "in")

int_togg

dev.off()