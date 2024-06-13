library(forestplot)
library(grid)
library(openxlsx)
library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)


#boxplot
data <- read.table("./boxplot-all TBA.txt", header=T,sep="\t")
forestdata<-read.table("./XO single TBA.txt", header=T,sep="\t" )

#set order
data$tbaname <- factor(data$tbaname,levels = rev(forestdata$tbaname))
table(data$tbaname)

data$Group <- factor(data$Group,levels = c("Control","OA"))
table(data$Group)


p=ggplot(data, aes(x=tbaname, y=log10(tba),fill=Group,color=Group)) + scale_y_continuous()+
  labs(x='',y="log10 (Concentration (nmol/L))") +
  theme(axis.title.x = element_text(size = 14),axis.title.y = element_text(size = 14))+
  geom_boxplot(width = 0.5,outlier.size=0.1,alpha=0.7,linewidth=0.3,fatten=1)+ 
  theme(axis.text.x=element_text(colour = "black"), 
        axis.text.y=element_blank(),  #element_text(colour = "black",size = 9),
        axis.ticks.y = element_blank(), 
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.5),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.5),
        #   plot.margin = margin( 0,  0,  1, 0, unit = "pt"),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=1),
        panel.grid.minor=element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("OA"="#f7df87","Control"="#99b9e9"))+  #"OA"="#ecc97f"
  scale_color_manual(values=c("OA"="#f7df87","Control"="#99b9e9"))+
  coord_flip()  #+facet_grid(enrich ~ ., scales = "free", space = "free")

p

width=3
height=5  #4.5

ggsave(paste0("./XO all tba boxplot",".pdf"), p, width =width, height = height, dpi=600, units = "in")



#forest
forestdata$tbaname

order = c( "GUDCA",   "TUDCA" , "GHDCA" ,"HCA" ,"7-KLCA" ,"UDCA" , "LCA", "TDCA" ,"12-DHCA",
           "GHCA", "GCDCA", "GCA", "HDCA", "GLCA", "7-KDCA", "TCA","23-NDCA", "DCA" ,   
           "UCA" , "12-KLCA", "CDCA","GDCA", "CA" , "TCDCA", "ILCA" ,"ACA"  )

forestdata2 = forestdata[order(forestdata$OR),]
rownames(forestdata2) = forestdata2$tbaname
forestdata2 = forestdata2[order,]

pdf(paste0("./XO_tbaforest",  ".pdf"),
    width = 3, height = 5) 

#height = 15, width = 7)
#height = 25, width = 15)
#height = 4, width = 3)

p2<-forestplot(mean = forestdata2[,"OR"],
               lower = forestdata2[,"LCI"],
               upper = forestdata2[,"UCI"],
               labeltext = forestdata2[,"tbaname"],
               #new_page = FALSE,
               zero = 1,
               #is.summary=c(TRUE,rep(FALSE,n.cells),TRUE,rep(FALSE,n.cells),TRUE,FALSE),
               #  clip=c(0.001,55),
               #  xlog=F,
               #xlim = c(0, 4),
               xlab = "OR (95% CI)",
               boxsize = .3,
               lwd.ci=1.5,
               lwd.zero=1.5,
               ci.vertices=T,
               ci.vertices.height = 0.15, 
               #vertices = FALSE,
               col=fpColors(box="#96b6d8",line="#96b6d8",zero="#999a9e"),
               txt_gp = fpTxtGp(label = gpar(fontsize = 14), xlab = gpar(fontsize = 16),
                                ticks = gpar(fontsize = 16))
)

p2

dev.off()


# Barplot 
forestdata2$minus_log10_pval = -log10(forestdata2$raw_p)
forestdata2$tbaname = factor(forestdata2$tbaname, levels = rev(order))
forestdata2$tbaname 

p3<- ggplot(forestdata2, aes(x = tbaname, y = minus_log10_pval),color="#f7df87") +
  geom_bar(stat = "identity", position=position_dodge(),fill="#f7df87") +
  # scale_fill_manual(values = c("#FED8B1"))  +
  coord_flip() + 
  #  scale_y_reverse() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 14),
        axis.text.y = element_text(colour = "black", size = 14),
        legend.position = "none") +
  xlab("") +
  ylab("-log10 (P value)") +
  scale_x_discrete(position = "top") +
  geom_hline(yintercept = -log10(0.05), linetype="dashed",lwd=0.5)


p3

pdf(paste0("./XO_tbabarplot",  ".pdf"), 
    height = 5, width = 3)
plot(p3)
dev.off()


