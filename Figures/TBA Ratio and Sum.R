library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)



setwd(".\\Figures")
getwd()


data <- read.table("sum and ratio OR.txt", header=T,sep="\t")


data$tbaname <- factor(data$tbaname,levels = rev(c("Total BAs", "UDCA species", "CDCA species", 
                                                   "CA species", "DCA species",  "LCA species", "HCA species", "Others",
                                                   "UDCAs/Total BAs", "UDCAs/CDCAs", "PBAs/SBAs"
)))



#sum
p1<- ggplot(subset(data,cat=="Sums"), aes(x=tbaname, y=OR)) + 
  geom_bar(stat="identity", width=0.8,alpha=0.9,linewidth=0.6,fill="#A8D07B") +
  geom_errorbar(aes(ymin=LCI, ymax=UCI),  width=.3,linewidth=0.3,color="#666666")+
  labs(x="",y="",title = "Sums") +
  scale_y_continuous(breaks  = c(0.0,0.5,1.0))+
  theme(axis.line = element_line(colour = "black", linetype = 1, linewidth  = 0.3),
        # axis.ticks.x = element_blank(),
        axis.title.x = element_text(colour = "black",size = 24),
        axis.title.y = element_text(colour = "black",size = 24),
        axis.text.x = element_text(colour = "black",size = 24),
        axis.text.y=element_text(colour = "black",size = 24),
        panel.background = element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        strip.background = element_blank(), panel.spacing.x = unit(0, 'in'), 
        strip.text.x = element_text(colour = 'black', size = 24), 
        strip.placement = 'outer',
        #    plot.title = element_text(colour = "black",size = 24,hjust=0.5),
        legend.position = "none",
        legend.box.background = element_blank(), 
        legend.key = element_blank(),
        legend.text=element_text(colour = 'black', size = 24), 
        legend.title = element_text(colour = 'black', size = 24),
        plot.margin = unit(c(1, 1, 1, 2.2), "lines"))+   #
  geom_hline(yintercept = 1, linetype="dashed",lwd=0.4,color="black")+
  coord_flip()


p1



p2<- ggplot(subset(data,cat=="Ratios"), aes(x=tbaname, y=OR)) + 
  geom_bar(stat="identity", width=0.8,alpha=0.9,linewidth=0.6,fill="#A8D07B") +
  geom_errorbar(aes(ymin=LCI, ymax=UCI),  width=.3,linewidth=0.3,color="#666666")+
  labs(x="",y="",title = "Ratios") +
  theme(axis.line = element_line(colour = "black", linetype = 1, linewidth  = 0.3),
        # axis.ticks.x = element_blank(),
        axis.title.x = element_text(colour = "black",size = 24),
        axis.title.y = element_text(colour = "black",size = 24),
        axis.text.x = element_text(colour = "black",size = 24),
        axis.text.y=element_text(colour = "black",size = 24),
        panel.background = element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        strip.background = element_blank(), panel.spacing.x = unit(0, 'in'), 
        strip.text.x = element_text(colour = 'black', size = 24), 
        strip.placement = 'outer',
        #    plot.title = element_text(colour = "black",size = 24,hjust=0.5),
        legend.position = "none",
        legend.box.background = element_blank(), 
        legend.key = element_blank(),
        legend.text=element_text(colour = 'black', size = 24), 
        legend.title = element_text(colour = 'black', size = 24),
        plot.margin = unit(c(1, 1, 1, 0), "cm"))+   
  geom_hline(yintercept = 1, linetype="dashed",lwd=0.4,color="black")+
  coord_flip()

p2



library(cowplot)

combined_plot <- plot_grid(p1, p2, ncol = 1,rel_heights = c(1, 0.6))
combined_plot

width=200
height=240


ggsave(paste0("barplot-TBA sum and ratio",".pdf"), combined_plot, width =width, height = height, dpi=600, units = "mm")





