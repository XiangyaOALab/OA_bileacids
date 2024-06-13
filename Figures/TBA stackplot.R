library(ggplot2)
library(reshape2)


metadata=read.table("./metadata-SKOA.txt", header=T, row.names=1, sep="\t", comment.char="")
tax=read.table("./tbadata-1714.txt", header=T, row.names=1, sep="\t", comment.char="")
metadata$Group<-factor(metadata$Group, level=c("OA","Control"))


# filter
idx = rownames(metadata) %in% colnames(tax)
metadata = metadata[idx,]
tax = tax[, rownames(metadata)]


# sample group info
sampFile = as.data.frame(metadata[,"Group"],row.names = row.names(metadata))
colnames(sampFile)[1] = "Group"


# sort by concentration
mean_sort = as.data.frame(tax[(order(-rowSums(tax,na.rm = T))), ])

merge_tax=mean_sort


#standardized as percentage
data_norm=merge_tax
for(j in 1:1714){
  sample_sum=apply(data_norm, 2, sum,na.rm=T) 
  for(i in 1:32){
    data_norm[i,j]=(data_norm[i,j]/sample_sum[j])*100
  }
}



data_norm$Taxonomy = rownames(data_norm)
data_all = as.data.frame(melt(data_norm, id.vars=c("Taxonomy")))

data_all$Taxonomy<-factor(data_all$Taxonomy, levels=c( 
  "DHLCA","IDCA" ,"TLCA" ,"THDCA","IALCA", "3-DHCA", "23-NDCA","12-DHCA","UCA","7-KDCA", 
  "ACA","12-KLCA", "LCA","GHDCA","ILCA","GLCA", "7-KLCA","HDCA", "TDCA",   
  "GHCA", "HCA" , "TCA", "TCDCA","GDCA" ,"DCA" ,    "GCA","CA" ,    
  "CDCA", "GCDCA","UDCA" ,"TUDCA" ,"GUDCA"))


#sort sample by GUDCA level
data_norm2<-as.data.frame(t(data_norm[,-1715]))
mean_sort2 = as.data.frame(data_norm2[(order(data_norm2$GUDCA)),])  
mean_sort2$id<-row.names(mean_sort2)

data_all$variable  = factor(data_all$variable, levels=mean_sort2$id)


# set group facet
data_all = merge(data_all, sampFile, by.x="variable", by.y = "row.names")

#set color
color_otu <- c("DHLCA"="#4D4D4D","IDCA"="#e9b6be" ,"TLCA"="#caadd8" ,"THDCA"="#CD96CD","IALCA"="#CD8C95", 
               "3-DHCA"="#DDBEA9", "23-NDCA"="#F5DEB3","12-DHCA"="#EEE8AA","UCA"="#FAFAD2","7-KDCA"="#d3e2b7", 
               "ACA"="#E0EEE0","12-KLCA"="#FFE1FF", "LCA"="#f6ecf7","GHDCA"="#EEE0E5","ILCA"="#D1EEEE",
               "GLCA"="#F0F8FF", "7-KLCA"='#d0e4ef',"HDCA"="#CAE1FF", "TDCA"="#B9D3EE",   
               "GHCA"="#F0FFF0", "HCA"="#FAEBD7", "TCA"="#FFF0F5", "TCDCA"="#FFE4E1","GDCA"="#EED2EE",
               "DCA"="#949494", "GCA"="#B8B8B8","CA"="#E8E8E8", "CDCA"="#EEE9E9", "GCDCA"="#F4F4F4",
               "UDCA"='#FFD700',"GUDCA"="#e3716e","TUDCA"="#99b9e9")


data_all2<-subset(data_all,data_all$value != "NA")

p = ggplot(data_all2, aes(x=variable, y = value, fill = Taxonomy )) +
  geom_bar(stat = "identity",position="fill", width=0.8)+
  scale_y_continuous(labels = scales::percent) +
  facet_grid( ~ Group, scales = "free_x", space="free_x", switch = "x") +
  theme(strip.background = element_rect(fill = "#F4F4F4"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(colour = "black",size = 32),
        axis.text.y = element_text(colour = "black",size = 32),
        strip.text.x =element_text(colour = "black",size = 32),
        legend.position ="bottom",
        legend.text = element_text(colour = "black",size = 32),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 32))+
  xlab("")+ylab("Percentage")+
  guides(fill = guide_legend(title = "Bile acids"))+
  scale_fill_manual(values=color_otu,breaks=rev(c("DHLCA","IDCA" ,"TLCA" ,"THDCA","IALCA", "3-DHCA", "23-NDCA","12-DHCA","UCA","7-KDCA", 
                                                  "ACA","12-KLCA", "LCA","GHDCA","ILCA","GLCA", "7-KLCA","HDCA", "TDCA",   
                                                  "GHCA", "HCA" , "TCA", "TCDCA","GDCA" ,"DCA" ,    "GCA","CA" ,    
                                                  "CDCA", "GCDCA","UDCA" ,"TUDCA" ,"GUDCA")))



p


width=400
height=350
ggsave(paste0("./TBA stackplot.pdf"), p, width = width, height = height, dpi=600, units = "mm")
