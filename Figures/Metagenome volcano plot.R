library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(RUnit)
library(ggforce)
library(tidyverse)
library(ggpubr)
library(ggprism)
library(paletteer)



rm(list = ls())

data <- read.table("./XO metagenome.txt", header=T,sep="\t")


#volcano plot

table(data$Phylum)

data$Phylum <- factor(data$Phylum,levels = c("NS","Firmicutes_A","Bacteroidota","Proteobacteria"))

data$abscoef=abs(data$coef)

p<-ggplot(data, aes(x = coef, y = -log10(qval),  fill = Phylum,colour = Phylum,stroke=abscoef,size=abscoef))+
  geom_point(shape = 20, stroke = 0.5)+
  scale_size(limits = c(0.001,1.11))+
  scale_fill_manual(values=c("NS"="grey","Firmicutes_A"="#EA8379","Bacteroidota"="#7DAEE0","Proteobacteria"="#B395BD"))+
  scale_color_manual(values=c("NS"="grey","Firmicutes_A"="#EA8379","Bacteroidota"="#7DAEE0","Proteobacteria"="#B395BD"))+
  # ylab('-log10 (Q value)')+
  #  xlab('beta coefficient')+
  labs(x = 'beta coefficient',
       y = '-log10 (Q value)')+
  theme(axis.text.x=element_text(colour = "black",size = 14),    #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_text(colour = "black",size = 14),
        axis.title.y=element_text(colour = "black",size = 14),
        # axis.line.y.right=element_blank(),
        # axis.line.y.left=element_line(linetype=1,linewidth=0.5),
        # axis.line.x.top=element_blank(),
        # axis.line.x.bottom = element_line(linetype=1,linewidth=0.5),
        panel.background = element_rect(fill="white",color="black",linetype=1,linewidth=1),
        panel.grid.minor=element_blank(),
        legend.position ="right",
        legend.key = element_blank(),
        #   legend.key.size = unit(1,"cm"),
        legend.text = element_text(colour = "black",size = 14),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 14)) + 
  geom_vline(xintercept = 0,lty = 2, col = "black", lwd = 0.5)+
  geom_hline(yintercept = -log10(0.2), lty = 2, col = "black", lwd = 0.5)+
  geom_text_repel(
    data = data[data$qval < 0.2 ,],
    aes(label = feature,color = Phylum),
    position = "identity",
    size = 5,
    segment.color = "black", show.legend = FALSE)

p

width=160
height=120

ggsave(paste0("./MGS volcano",".pdf"), p, width =width, height = height, dpi=600, units = "mm")


