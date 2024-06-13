library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)


tax_genus=read.table("./40v40 elisa2.txt", header=T, row.names=1, sep="\t", comment.char="")


tax_genus$Group<-factor(tax_genus$Group, level=c("Control","OA"))


######FGF19############
p<-ggplot(data = tax_genus,
          aes(x=Group, y=fgf,fill=Group)) +  #,color=Group
  geom_jitter(aes(fill = Group,color=Group),
              width = 0.15,  size=1.5, stroke=0.5, shape=21) +
  geom_boxplot(aes(fill = Group),
               alpha=0.6, linewidth=0.4, width=0.5, outliers = FALSE, staplewidth = 0.4) +
  geom_signif(comparisons = list(c("Control", "OA")),
              map_signif_level = T,
              test = t.test, 
              step_increase = 0.1,
              textsize = 5) +
  scale_fill_manual(values=c("OA"="#eebf6d","Control"="#4eab90")) +
  scale_color_manual(values=c("OA"="#eebf6d","Control"="#4eab90"))+
  labs(title="FGF19",x='',y="Concentration (pg/mL)") +
  theme(axis.text.x=element_blank(),    #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_text(colour = "black",size = 12),
        axis.title.y=element_text(colour = "black",size = 12),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.4),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title=element_text(colour = "black",size = 12,hjust=0.5),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth =0.4),
        panel.grid.minor=element_blank(),
        legend.position = "top",legend.text = element_text(colour = "black",size = 12),
        legend.title = element_text(colour = "black",size = 14))

p

width=70
height=80

ggsave(paste0("./boxplot-FGF19",".pdf"), p, width =width, height = height, dpi=600, units = "mm")



######GLP-1############

p<-ggplot(data = tax_genus,
          aes(x=Group, y=glp1,fill=Group)) +  #,color=Group
  geom_jitter(aes(fill = Group,color=Group),
              width = 0.15,  size=1.5, stroke=0.5, shape=21) +
  geom_boxplot(aes(fill = Group),
               alpha=0.6, linewidth=0.4, width=0.5, outliers = FALSE, staplewidth = 0.5) +
  geom_signif(comparisons = list(c("Control", "OA")),
              map_signif_level = T,
              test = t.test, 
              step_increase = 0.1,
              textsize = 5) +
  scale_fill_manual(values=c("OA"="#eebf6d","Control"="#4eab90")) +
  scale_color_manual(values=c("OA"="#eebf6d","Control"="#4eab90"))+
  labs(title="GLP-1",x='',y="Concentration (pg/mL)") +
  theme(axis.text.x=element_blank(),    #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_text(colour = "black",size = 12),
        axis.title.y=element_text(colour = "black",size = 12),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.4),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title=element_text(colour = "black",size = 12,hjust=0.5),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth =0.4),
        panel.grid.minor=element_blank(),
        legend.position = "top",legend.text = element_text(colour = "black",size = 12),
        legend.title = element_text(colour = "black",size = 14))


p

width=70
height=80

ggsave(paste0("./boxplot-GLP1",".pdf"), p, width =width, height = height, dpi=600, units = "mm")



