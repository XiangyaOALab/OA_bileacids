library(dplyr)
library(psych)
library(cowplot)
library(ggplot2)


metadata_MGS=read.table("./metadata-incRA-MGS.txt", header=T, row.names=1, sep="\t", comment.char="")
metadata=read.table("./gudca class.txt", header=T, row.names=1, sep="\t", comment.char="")



#CB species
tax_genus=read.table("./species profile.txt", header=T, row.names=1, sep="\t", comment.char="")

tax_genus2<-as.data.frame(t(tax_genus))

cb<-subset(tax_genus2,select=c("Clostridium M bolteae"))

df<-merge(metadata_MGS,cb,by="row.names")

df$gudcagroup <- ifelse(df$GUDCAcat1=="cat1","Low","High")
table(df$gudcagroup)

df$gudcagroup <- factor(df$gudcagroup,levels = c("Low","High"))

p=ggplot(df, aes(x=gudcagroup, y=log10(df[,60]),fill=gudcagroup)) +scale_y_continuous()+
  labs(title="Clostridium bolteae",x='GUDCA category',y="log10 (Relative abundance)") +
  #  geom_violin(scale="width", trim=F,aes(colour=gudcagroup),alpha=0.6)+  #draw_quantiles = c(0.25,0.5,0.75),
  geom_boxplot(width = 0.3,color="black",lwd=0.4,outlier.size=0.4,outlier.shape = NA,staplewidth = 0.5,notch = FALSE,aes(fill=gudcagroup),alpha=0.7)+ 
  geom_jitter(width=0.1, shape=20, size=0.6,aes(colour=gudcagroup),alpha=0.6)+   #aes(colour = factor(unibi))
  scale_color_manual(values=c("High"="#e3716e","Low"="#99b9e9"))+ 
  theme(axis.text.x=element_blank(),   #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_text(colour = "black",size = 14),
        axis.title.y=element_text(colour = "black",size = 14),
        axis.title.x=element_text(colour = "black",size = 14),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.4),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=0.4),
        panel.grid.minor=element_blank(),
        plot.title = element_text(colour = "black",size = 14,hjust=0.5),
        #    legend.key = element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 14),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 14)) + 
  #  guides(fill=guide_legend(title="Group"))+
  #  facet_wrap(~tbaname,scales = "free",nrow = 1)+
  scale_fill_manual(values=c("High"="#e3716e","Low"="#99b9e9"))

#stat_compare_means(comparisons = my_comparisons)# Add pairwise comparisons p-value
p

#p1<-p+  geom_smooth(data=data,aes(group=1),method='lm',se=F,colour="#9C9C9C")

#p1

width=140
height=150

ggsave(paste0("./CB-GUDCA CAT",".pdf"), p, width =width, height = height, dpi=600, units = "mm")

