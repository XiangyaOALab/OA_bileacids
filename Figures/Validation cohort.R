library(ggplot2)
library(reshape2)


#####################stack plot#########################

metadata=read.table("./SS metadata.txt", header=T, row.names=1, sep="\t", comment.char="")
tax=read.table("./ss individual tba.txt", header=T, row.names=1, check.names=F,sep="\t", comment.char="")
metadata$Group<-factor(metadata$Group, level=c("OA","Control"))
table(metadata$Group)


# filter
idx = rownames(metadata) %in% colnames(tax)
metadata = metadata[idx,]
tax = tax[, rownames(metadata)]

# sample group info
sampFile = as.data.frame(metadata[,"Group"],row.names = row.names(metadata))
colnames(sampFile)[1] = "Group"


#sort by concentration
mean_sort = as.data.frame(tax[(order(-rowSums(tax,na.rm = T))), ])

merge_tax=mean_sort


#standardized as percentage
data_norm=merge_tax
for(j in 1:154){
  sample_sum=apply(data_norm, 2, sum,na.rm=T)  
  for(i in 1:31){
    data_norm[i,j]=(data_norm[i,j]/sample_sum[j])*100
  }
}



#sort by concentration
data_norm$Taxonomy = rownames(data_norm)
data_all = as.data.frame(melt(data_norm, id.vars=c("Taxonomy")))


data_all$Taxonomy<-factor(data_all$Taxonomy, levels=c( 
  "IALCA","DHLCA" ,"TLCA" ,"23-NDCA","3-DHCA", "THDCA", "ACA","12-DHCA","7-KDCA", 
  "ILCA","UCA", "LCA","12-KLCA","GLCA","GHCA", "HCA","7-KLCA", "HDCA",   
  "TDCA", "GHDCA" , "TCA", "TCDCA","CA" ,"GDCA" ,    "DCA","GCA" ,    
  "CDCA", "GCDCA","UDCA" ,"TUDCA" ,"GUDCA"))
table( data_all$Taxonomy)


#sort sample by GUDCA level
data_norm2<-as.data.frame(t(data_norm[,-155]))
mean_sort2 = as.data.frame(data_norm2[(order(data_norm2$GUDCA)),])  
mean_sort2$id<-row.names(mean_sort2)

data_all$variable  = factor(data_all$variable, levels=mean_sort2$id)


# set group facet
data_all = merge(data_all, sampFile, by.x="variable", by.y = "row.names")


# set color
color_otu <- c("IALCA"="#4D4D4D","DHLCA"="#e9b6be" ,"TLCA"="#caadd8" ,"23-NDCA"="#CD96CD","3-DHCA"="#CD8C95", 
               "THDCA"="#D1EEEE", "ACA"="#d3e2b7","12-DHCA"="#EEE8AA","7-KDCA"="#FAFAD2", 
               "ILCA"="#E0EEE0","UCA"="#FFE1FF", "LCA"="#EED5D2","12-KLCA"="#EEE0E5","GLCA"="#FFC0CB",
               "GHCA"="#F0F8FF", "HCA"='#CAE1FF',"7-KLCA"="#B9D3EE", "HDCA"="#8DB6CD",   
               "TDCA"="#F0FFF0", "GHDCA"="#FAEBD7", "TCA"="#FFF0F5", "TCDCA"="#FFE4E1","CA"="#EED2EE",
               "GDCA"="#999999", "DCA"="#B8B8B8","GCA"="#E8E8E8", "CDCA"="#EEE9E9", "GCDCA"="#F4F4F4",
               "UDCA"='#FFD700',"TUDCA"="#99b9e9","GUDCA"="#e3716e")


data_all2<-subset(data_all,data_all$value != "NA")

p = ggplot(data_all2, aes(x=variable, y = value, fill = Taxonomy )) +
  geom_bar(stat = "identity",position="fill", width=0.8)+
  scale_y_continuous(labels = scales::percent) +
  facet_grid( ~ Group, scales = "free_x", space="free_x", switch = "x") +
  theme(strip.background = element_rect(fill = "#F4F4F4"),
        plot.background= element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "black",size = 22),
        axis.text.y = element_text(colour = "black",size = 22),
        strip.text.x =element_text(colour = "black",size = 22),
        legend.position ="bottom",
        legend.text = element_text(colour = "black",size = 22),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 22))+
  xlab("")+ylab("Percentage")+
  guides(fill = guide_legend(title = "Bile acids"))+
  scale_fill_manual(values=color_otu,breaks=rev(c("IALCA","DHLCA" ,"TLCA" ,"23-NDCA","3-DHCA", "THDCA", "ACA","12-DHCA","7-KDCA", 
                                                  "ILCA","UCA", "LCA","12-KLCA","GLCA","GHCA", "HCA","7-KLCA", "HDCA",   
                                                  "TDCA", "GHDCA" , "TCA", "TCDCA","CA" ,"GDCA" ,    "DCA","GCA" ,    
                                                  "CDCA", "GCDCA","UDCA" ,"TUDCA" ,"GUDCA")))



p


width=400
height=200
ggsave(paste0("./SS TBA stackplot.pdf"), p, width = width, height = height, dpi=600, units = "mm")



#####box_forest_bar############

library(forestplot)
library(grid)
library(openxlsx)
library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)


#boxplot

rm(list = ls()) 
data <- read.table("./SS-boxplot-all TBA.txt", header=T,sep="\t")
forestdata<-read.table("./SS-single TBA.txt", header=T,sep="\t" )


data$tbaname <- factor(data$tbaname,levels = rev(forestdata$tbaname))
table(data$tbaname)
data$Group <- factor(data$Group,levels = c("Control","OA"))
table(data$Group)


p=ggplot(data, aes(x=tbaname, y=log10(tba),fill=Group,color=Group)) + scale_y_continuous()+
  labs(x='',y="log10 (Concentration (nmol/L))") +
  theme(axis.title.x = element_text(size = 12),axis.title.y = element_text(size = 12))+
  geom_boxplot(width = 0.5,outlier.size=0.1,alpha=0.7,linewidth=0.4,fatten=1)+ 
  theme(axis.text.x=element_text(colour = "black"), 
        axis.text.y=element_blank(),   #element_text(colour = "black",size = 9),
        axis.ticks.y = element_blank(), 
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.4),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        #   plot.margin = margin( 0,  0,  1, 0, unit = "pt"),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=1),
        panel.grid.minor=element_blank(),
        legend.position = "none") +
  scale_fill_manual(values=c("OA"="#90cd74","Control"="#9e89cd"))+
  scale_color_manual(values=c("OA"="#90cd74","Control"="#9e89cd"))+
  coord_flip()  #+facet_grid(enrich ~ ., scales = "free", space = "free")

p

width=3.5
height=4.5

ggsave(paste0("./SS-all tba boxplot",".pdf"), p, width =width, height = height, dpi=600, units = "in")



#forest
forestdata$tbaname

order = c( "GUDCA" , "GCDCA","LCA", "GCA" , "TCDCA" , "GDCA" ,"DCA" , "CDCA","CA",     
           "TCA" , "GLCA","ILCA" , "HCA", "TUDCA" ,"ACA" , "TDCA" ,"UDCA","HDCA",   
           "12-DHCA", "7-KDCA","12-KLCA" ,"GHCA","7-KLCA","GHDCA",   "UCA" )


forestdata2 = forestdata[order(forestdata$OR),]
rownames(forestdata2) = forestdata2$tbaname
forestdata2 = forestdata2[order,]

pdf(paste0("./SS_tbaforest",  ".pdf"),
    width = 3.5, height = 4.5) 

#height = 15, width = 7)
#height = 25, width = 15)
#height = 4, width = 3)

forestplot(mean = forestdata2[,"OR"],
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
           #   vertices = FALSE,
           col=fpColors(box="#9e89cd",line="#9e89cd",zero="#999a9e"),
           txt_gp = fpTxtGp(label = gpar(fontsize = 12), xlab = gpar(fontsize = 12),
                            ticks = gpar(fontsize = 12))
)
dev.off()



# Barplot 
forestdata2$minus_log10_pval = -log10(forestdata2$raw_p)
forestdata2$tbaname = factor(forestdata2$tbaname, levels = rev(order))
forestdata2$tbaname 

plot = ggplot(forestdata2, aes(x = tbaname, y = minus_log10_pval),color="#90cd74") +
  geom_bar(stat = "identity", position=position_dodge(),fill="#90cd74") +
  # scale_fill_manual(values = c("#FED8B1"))  +
  coord_flip() + 
  #  scale_y_reverse() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", size = 10),
        axis.text.y = element_text(colour = "black", size = 10),
        legend.position = "none") +
  xlab("") +
  ylab("-log10 (P value)") +
  scale_x_discrete(position = "top") +
  geom_hline(yintercept = -log10(0.05), linetype="dashed",lwd=0.4)

plot

pdf(paste0("./SS_tbabarplot",  ".pdf"), 
    height = 4.5, width = 3.5)
plot(plot)
dev.off()



#########GUDCA and OA severity half violin plot##############
library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)
library(gghalves)
library(reshape2)
library(cowplot)

data <- read.table("./SS-boxplot-GUDCA.txt", header=T,sep="\t")


#Laterality
data$unibi <- factor(data$unibi,levels = c("Control","Unilateral","Bilateral"))


p=ggplot(data, aes(x=unibi, y=log10(tba),fill=unibi)) +scale_y_continuous()+
  labs(title="Laterality",x='',y="log10 (Concentration (nmol/L))") +
  geom_half_violin(side="r",scale="width", aes(colour=unibi), nudge=-0.5,
                   width=1.8,linewidth=0.4,
                   #draw_quantiles=c(0.5),
                   trim=F,alpha=0.4)+
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.0001, aes(colour=unibi), 
                    linewidth=0.4,nudge=-0.5,alpha=0.7,outlier.shape = NA) +
  scale_color_manual(values=c("Control" = "#A3D2E2", "Unilateral" = "#4B8DBC", 
                              "Bilateral" = "#2D66A5"))+ 
  theme(axis.text.x=element_blank(),  #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_text(colour = "black",size = 12),
        axis.title.y=element_text(colour = "black",size = 12),
        axis.title.x=element_text(colour = "black",size = 12),
        axis.ticks.x=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.4),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title = element_text(colour = "black",size = 12,hjust=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=0.4),
        panel.grid.minor=element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 10),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 10)) + 
  scale_fill_manual(values=c("Control" = "#9fc5e8", "Unilateral" = "#4B8DBC", 
                             "Bilateral" = "#2D66A5"))

p



#KL grade
data$klcat <- factor(data$klcat,levels = c("Control","KL=2","KL>2"))
table(data$klcat)


p2=ggplot(data, aes(x=klcat, y=log10(tba),fill=klcat)) +scale_y_continuous()+
  labs(title ="KL grade",x='',y="") +
  geom_half_violin(side="r",scale="width", aes(color=klcat), nudge=-0.5,
                   width=1.8,linewidth=0.4,
                   #draw_quantiles=c(0.5),
                   trim=F,alpha=0.4)+
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.0001, aes(color=klcat),
                    linewidth=0.4,nudge=-0.5,alpha=0.7,outlier.shape = NA) +
  scale_color_manual(values=c("Control" = "#f4cccc", "KL=2" = "#d8988c", "KL>2" = "#a32a31"))+ 
  theme(axis.text.x=element_blank(), #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_text(colour = "black",size = 12),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_blank(),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title = element_text(colour = "black",size = 12,hjust=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=0.4),
        panel.grid.minor=element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 10),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 10)) + 
  scale_fill_manual(values=c("Control" = "#f4cccc", "KL=2" = "#d8988c", "KL>2" = "#a32a31"))


p2



#Pain severity

data2<-subset(data,vas_cat != "")

data2$vas_cat <- factor(data2$vas_cat,levels = c("No pain", "Mild pain","Moderate to severe pain"))
table(data$vas_cat)


p3=ggplot(data2, aes(x=vas_cat, y=log10(tba),fill=vas_cat)) +scale_y_continuous()+
  labs(title="Pain",x='',y="") +
  geom_half_violin(side="r",scale="width", aes(color=vas_cat), nudge=-0.5,
                   width=1.8,linewidth=0.4,
                   #draw_quantiles=c(0.5),
                   trim=F,alpha=0.4)+
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.0001, aes(color=vas_cat),
                    linewidth=0.4,nudge=-0.5,alpha=0.7,outlier.shape = NA) +
  scale_color_manual(values=c("No pain" = "#f6dc91", "Mild pain" = "#fac628", 
                              "Moderate to severe pain" = "#f8b044"))+ 
  theme(axis.text.x=element_blank(),  #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_text(colour = "black",size = 12),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_blank(),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title = element_text(colour = "black",size = 12,hjust=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=0.4),
        panel.grid.minor=element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 10),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 10)) + 
  scale_fill_manual(values=c("No pain" = "#f6dc91", "Mild pain" = "#fac628", 
                             "Moderate to severe pain" = "#f8b044"))


p3



#OST

data$OST <- factor(data$OST,levels = c("0", "1","2","3"))
table(data$OST)


p4=ggplot(data, aes(x=OST, y=log10(tba),fill=OST)) +scale_y_continuous()+
  labs(title="OST",x='',y="") +
  geom_half_violin(side="r",scale="width", aes(color=OST), nudge=-0.5,
                   width=1.8,linewidth=0.4,
                   #draw_quantiles=c(0.5),
                   trim=F,alpha=0.4)+
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.0001, aes(color=OST),
                    linewidth=0.4,nudge=-0.5,alpha=0.7,outlier.shape = NA) +
  scale_color_manual(values=c("0" = "#D9F0A3", "1" = "#90cd74", "2"="#37b44a",
                              "3" = "#32820e"))+ 
  theme(axis.text.x=element_blank(),  #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_text(colour = "black",size = 12),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_blank(),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title = element_text(colour = "black",size = 12,hjust=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=0.4),
        panel.grid.minor=element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 10),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 10)) + 
  scale_fill_manual(values=c("0" = "#D9F0A3", "1" = "#90cd74", "2"="#37b44a",
                             "3" = "#32820e"))


p4



#JSN

data$JSN <- factor(data$JSN,levels = c("0", "1","2","3"))
table(data$JSN)


p5=ggplot(data, aes(x=JSN, y=log10(tba),fill=JSN)) +scale_y_continuous()+
  labs(title="JSN",x='',y="") +
  geom_half_violin(side="r",scale="width", aes(color=JSN), nudge=-0.5,
                   width=1.8,linewidth=0.4,
                   #draw_quantiles=c(0.5),
                   trim=F,alpha=0.4)+
  geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.000000000001, aes(color=JSN),
                    linewidth=0.4,nudge=-0.5,alpha=0.8,outlier.shape = NA) +
  scale_color_manual(values=c("0" = "#d5cce9", "1" = "#b7acd1", "2"="#9e89cd",
                              "3" = "#6449a9"))+ 
  theme(axis.text.x=element_blank(), #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_text(colour = "black",size = 12),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_blank(),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        plot.title = element_text(colour = "black",size = 12,hjust=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=0.4),
        panel.grid.minor=element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 10),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 10)) + 
  scale_fill_manual(values=c("0" = "#d5cce9", "1" = "#b7acd1", "2"="#9e89cd",
                             "3" = "#6449a9"))


p5




plot<-plot_grid(p, p2,p3,p4,p5, align = "h", nrow=1,rel_widths = c(1,1,1,1,1))
plot



width=450
height=120

ggsave(paste0("./SS dose",".pdf"), plot, width =width, height = height, dpi=600, units = "mm")




