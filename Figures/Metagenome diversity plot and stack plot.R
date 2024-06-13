library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)



metadata=read.table("./metadata-incRA.txt", header=T, row.names=1, sep="\t", comment.char="")

metadata$Sample_name=row.names(metadata)


#################Alpha diversity#####################
#species-shannon
data <- read.table("./Species-shannon.txt", header=T,sep="\t")

data2=merge(data,metadata,by="Sample_name")
head(data2,n=3)

data2$Group<-factor(data2$Group, level=c("Control","OA"))
table(data2$Group)


p<-ggplot(data2, aes(x=Group, y=Shannon_index,fill=Group,color=Group)) + 
  #  stat_boxplot(geom="errorbar",width=0.2,position=position_dodge(0.6))+    
  #stat_boxplot(geom = 'errorbar',width = 0.2)+
  labs(x='',y="Shannon index") +scale_y_continuous()+  #limits = c(1000,4200)+
  geom_violin(scale="width", trim=F,aes(colour=Group),alpha=0.5)+  #draw_quantiles = c(0.25,0.5,0.75),
  geom_boxplot(width = 0.3,outlier.size=0.3,outlier.shape=NA,notch=FALSE,alpha=0.34,linewidth=0.6)+ 
  #alpha=0.7,outlier.shape=16
  theme(axis.text.x=element_text(colour = "black",size = 14), 
        axis.text.y=element_text(colour = "black",size = 14),
        axis.title.y=element_text(colour = "black",size = 14),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.5),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.5),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth =1),
        panel.grid.minor=element_blank(),legend.position = "none") + 
  scale_fill_manual(values=c("OA"="#D46669","Control"="#6D8EB2"))+
  scale_color_manual(values=c("OA"="#D46669","Control"="#6D8EB2"))


p


width=70
height=100

ggsave(paste0("./Species-shannon",".pdf"), p, width =width, height = height, dpi=600, units = "mm")




#gene

#gene-shannon
data <- read.table("./Gene-shannon.txt", header=T,sep="\t")
head(data)

data2=merge(data,metadata,by="Sample_name")

data2$Group<-factor(data2$Group, level=c("Control","OA"))
table(data2$Group)



p<-ggplot(data2, aes(x=Group, y=Shannon_index,fill=Group,color=Group)) + 
  labs(x='',y="Shannon index") +scale_y_continuous()+  #limits = c(1000,4200)+
  geom_violin(scale="width", trim=F,aes(colour=Group),alpha=0.5)+  #draw_quantiles = c(0.25,0.5,0.75),
  geom_boxplot(width = 0.3,outlier.size=0.3,outlier.shape=NA,notch=FALSE,alpha=0.34,linewidth=0.6)+ 
  #alpha=0.7,outlier.shape=16
  theme(axis.text.x=element_text(colour = "black",size = 14), 
        axis.text.y=element_text(colour = "black",size = 14),
        axis.title.y=element_text(colour = "black",size = 14),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.5),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.5),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth =1),
        panel.grid.minor=element_blank(),legend.position = "none") + 
  scale_fill_manual(values=c("OA"="#D46669","Control"="#6D8EB2"))+
  scale_color_manual(values=c("OA"="#D46669","Control"="#6D8EB2"))


p


width=70
height=100

ggsave(paste0("./Gene-shannon",".pdf"), p, width =width, height = height, dpi=600, units = "mm")


########Beta diversity###############

suppressWarnings(suppressMessages(library(amplicon)))

group = "Group"
metadata[[group]] = factor(metadata[[group]], levels = c("Control","OA"))

#species-bray curtis
distance_type = "Species-bray"

distance_mat = read.table("./Species-bray.txt", header=T, row.names=1, sep="\t", comment.char="")
distance_mat[1:3, 1:3]


# Plotting Constrained PCoA based on distance matrix
p = beta_pcoa(distance_mat, metadata, groupID = group)+
  scale_color_manual(values=c("OA"="#D46669","Control"="#6D8EB2"))+geom_point(alpha=0.3)

p

width = 150
height = 100

ggsave(paste0("./pcoa_",distance_type,".pdf"), p, width = width, height = height,dpi=600, units = "mm")



#gene-bray curtis
distance_type = "Gene-bray"

distance_mat = read.table("./Gene-bray.txt", header=T, row.names=1, sep="\t", comment.char="")
distance_mat[1:3, 1:3]


# Plotting Constrained PCoA based on distance matrix
p = beta_pcoa(distance_mat, metadata, groupID = group)+
  scale_color_manual(values=c("OA"="#D46669","Control"="#6D8EB2"))+geom_point(alpha=0.3)

p

width = 150
height = 100

ggsave(paste0("./pcoa_",distance_type,".pdf"), p, width = width, height = height,dpi=600, units = "mm")





##########Stack plot###############
library(ggplot2)
library(reshape2)


metadata=read.table("./metadata-incRA.txt", header=T, row.names=1, sep="\t", comment.char="")
tax=read.table("./species profile.txt", header=T, row.names=1, sep="\t", comment.char="")
metadata$Group<-factor(metadata$Group, level=c("OA","Control"))



# filter
idx = rownames(metadata) %in% colnames(tax)
metadata = metadata[idx,]
tax = tax[, rownames(metadata)]

# sample group info
sampFile = as.data.frame(metadata[,"Group"],row.names = row.names(metadata))
colnames(sampFile)[1] = "Group"


# sort by abundance
mean_sort = as.data.frame(tax[(order(-rowSums(tax,na.rm = T))), ])


# filter top N

topN=15

other = colSums(mean_sort[topN:dim(mean_sort)[1], ])

mean_sort = mean_sort[1:(topN - 1), ]

mean_sort = rbind(mean_sort,other)

rownames(mean_sort)[topN] = c("Other")


merge_tax=mean_sort


# calculate mean by group

mat_t = t(merge_tax)
mat_t2 = merge(sampFile, mat_t, by="row.names")
mat_t2 = mat_t2[,c(-1)]

mat_mean = aggregate(mat_t2[,-1], by=mat_t2[1], FUN=mean) # mean

mat_mean_final = do.call(rbind, mat_mean)[-1,]

geno = mat_mean$Group

colnames(mat_mean_final) = geno
mean_sort=as.data.frame(mat_mean_final)

mean_sort$Taxonomy = rownames(mean_sort)
data_all = as.data.frame(melt(mean_sort, id.vars=c("Taxonomy")))
data_all$Taxonomy  = factor(data_all$Taxonomy, levels=rownames(mean_sort))


colorset<-c("#D1EEEE",'#B9D3EE','#92c2dd',"#4995c6",'#1663a9','#fabb6e','#fc8002','#add888','#369f2d','#fac7b3','#ee4431','#b9181a',
            
            '#b4b4d5','#8484b1','#614099')  #,"#ffff99"

p = ggplot(data_all, aes(x=variable, y = value, fill = Taxonomy )) +
  geom_bar(stat = "identity",position="fill", width=0.3)+
  scale_y_continuous(labels = scales::percent) +
  ylab("Percentage (%)")+ theme_classic()+
  scale_fill_manual(values=rev(colorset)) + 
  theme(axis.line= element_blank(),axis.ticks=element_blank(),
        axis.text.y=element_blank(),axis.text.x=element_text(size=10),
        axis.title.y=element_text(size=10),axis.title.x=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=10),
        legend.position = "bottom",legend.box = "horizontal")+
  guides(fill = guide_legend(nrow = 5))

p


width=150
height=80
ggsave(paste0("./species-top15.pdf"), p, width = width, height = height, dpi=600, units = "mm")
