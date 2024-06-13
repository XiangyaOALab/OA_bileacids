library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)


#load ggplot2 version 3.4.3
packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.4.3.tar.gz"
install.packages(packageurl, repos=NULL, type="source")

rm(list=ls())




############Ratio############

data <- read.table("./boxplot-TBA ratio.txt", header=T,sep="\t")

# set order
data$tbaname <- factor(data$tbaname,levels = c("UDCA/CDCA","PBA/SBA","UnconBA/ConBA","12-OH/non12-OH BA","GUDCA/CDCA","GUDCA/UDCA","TUDCA/CDCA",
                                               "TUDCA/UDCA"))
table(data$tbaname)

data$Group <- factor(data$Group,levels = c("Control","OA"))
table(data$Group)


#barplot

library(plyr)

#calculate mean and SD
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, !!varname := mean)
  return(data_sum)
}



df<-data

df2 <- data_summary(df, varname="tba", 
                    groupnames=c("tbaname", "Group"))



df2$tbaname <- factor(df2$tbaname,levels = c("UDCA/CDCA","PBA/SBA","UnconBA/ConBA","12-OH/non12-OH BA","GUDCA/CDCA","GUDCA/UDCA","TUDCA/CDCA",
                                             "TUDCA/UDCA"))

my_comparisons <- list( c("OA","Control"))


p<- ggplot(df2, aes(x=Group, y=tba, fill=Group,color=Group)) + 
  geom_bar(stat="identity", width=0.8,alpha=0.5,linewidth=0.6,
           position=position_dodge()) +
  geom_errorbar(aes(ymin=tba, ymax=tba+sd,colour=Group),  width=.3,
                position=position_dodge(.9))+
  labs(x='Ratios',y="") +
  theme(axis.line = element_line(colour = "black", linetype = 1, linewidth  = 0.4),
        # axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 26),
        axis.text.x = element_blank(),
        axis.text.y=element_text(colour = "black",size = 26),
        panel.background = element_rect(fill="white"),
        panel.grid.minor=element_blank(),
        strip.background = element_blank(), panel.spacing.x = unit(0, 'in'), 
        strip.text.x = element_text(colour = 'black', size = 26), 
        strip.placement = 'outer',
        legend.position = "top",
        legend.box.background = element_blank(), 
        legend.key = element_blank(),
        legend.text=element_text(colour = 'black', size = 26), 
        legend.title = element_text(colour = 'black', size = 26)) +  #
  #  scale_x_discrete(expand = expansion(add = 2)) +
  #  facet_wrap(~tbaname,scales = "free",nrow=1)+
  scale_fill_manual(values=c("OA"="#f7df87","Control"="#99b9e9"))+
  scale_color_manual(values=c("OA"="#f7df87","Control"="#99b9e9"))+
  facet_grid(~tbaname, switch = 'x') +
  stat_compare_means(aes(group = Group), comparisons = my_comparisons, tip.length = 0.01) 


p

library(ggbreak) 

p1 <- p + scale_y_break(c(0.6,1), scales=1) 

#,expand = expansion(add=c(0,0))


p1


width=400
height=120


ggsave(paste0(".\\barplot-TBA ratio",".pdf"), p1, width =width, height = height, dpi=600, units = "mm")



###########Sum####################

data <- read.table("./boxplot-TBA sum.txt", header=T,sep="\t")


#set order
data$tbaname <- factor(data$tbaname,levels = c("Total UDCA","CBA","ABA","GlycineBA","TaurineBA"))
table(data$tbaname)

data$Group <- factor(data$Group,levels = c("Control","OA"))
table(data$Group)


#violin plot
p<-ggplot(data, aes(x=Group, y=log10(tba),fill=Group,color=Group)) + 
  labs(x='',y="log10 (Concentration (nmol/L))") +
  geom_boxplot(width = 0.4,outlier.size=0.3,outlier.shape=NA,notch=F,alpha=0.5,linewidth=0.4)+
  geom_violin(scale="width", trim=F,aes(colour=Group),alpha=0.35)+  #draw_quantiles = c(0.25,0.5,0.75),#alpha=0.7,outlier.shape=16
  theme(axis.text.x=element_blank(),    #axis.text.x=element_text(colour = "black",size = 14)
        axis.text.y=element_text(colour = "black",size = 26),
        axis.title.y=element_text(colour = "black",size = 20),
        axis.line.y.right=element_blank(),
        axis.line.y.left=element_line(linetype=1,linewidth=0.4),
        axis.line.x.top=element_blank(),
        axis.line.x.bottom = element_line(linetype=1,linewidth=0.4),
        panel.background = element_rect(fill="white",color="white",linetype=1,linewidth=1),
        panel.grid.minor=element_blank(),
        strip.text= element_text(colour = "black",size = 26),
        strip.background=element_blank(),
        legend.position ="top",legend.text = element_text(colour = "black",size = 26),  #legend.position = c(.9, .25)
        legend.title = element_text(colour = "black",size = 26)) + 
  facet_wrap(~tbaname,scales = "free",nrow=1)+
  scale_fill_manual(values=c("OA"="#f7df87","Control"="#96b6d8"))+
  scale_color_manual(values=c("OA"="#f7df87","Control"="#96b6d8"))

p




width=400
height=110

ggsave(paste0("./violin-TBA sum",".pdf"), p, width =width, height = height, dpi=600, units = "mm")





