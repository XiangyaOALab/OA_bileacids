library(circlize) 
library(reshape2)
library(ComplexHeatmap) 
library(grid) 
library(dplyr)


taxonomy <- read.delim('./sig corr-species and tba.txt', sep = '\t', stringsAsFactors = FALSE)
node <- read.delim('./node.txt', sep = '\t', stringsAsFactors = FALSE)

tax_class <- unique(node$Class)


all_ID <- node$node1
all_otu <- node$node1[node$Class=="Gut microbiome"]
all_tba<-node$node1[node$Class=="Plasma bile acids"]

taxonomy$OTU_ID <- factor(taxonomy$node1, levels = all_otu)


count1<-taxonomy %>% 
  count(node1)      

count1$node1 <- factor(count1$node1, levels = all_otu)

count2<-taxonomy %>% 
  count(node2)    

count2<-count2 %>% rename(node1=node2)  

count<-rbind(count1,count2)


all_ID2<-as.data.frame(all_ID) 
all_ID2<-all_ID2 %>% rename(node1=all_ID)   


countdata<-merge(all_ID2,count,all.x=T,by="node1")  
row.names(countdata)<-countdata$node1  



num<-ifelse(is.na(countdata$n) , 1, countdata$n) 
names(num) <-row.names(countdata)
num

countdata <- cbind(countdata,num)
countdata2<-countdata[3]

all_ID_xlim <- cbind(rep(0, length(all_ID)),countdata2)



color_otu2 <- c('#fc8002','#FDB462', '#FCCDE5','#cedfef','#fac7b3','#ee4431','#FFFFB3','#92c2dd',
                
                "#313695","#6181BD",'#1663a9','#b4b4d5','#8484b1','#614099')



circos.clear()

pdf('./circlize_plot2.pdf', width = 20, height = 15)

circle_size = unit(1, 'snpc')


gap_size <- c(rep(1, length(all_otu) - 1), 4, rep(1, length(all_tba) - 1), 4)
circos.par(cell.padding = c(0, 0, 0, 0), start.degree = 0, gap.degree = gap_size,circle.margin=c(0.25,0.25,0.4,0.65))  
circos.initialize(factors = factor(all_ID, levels = all_ID), xlim = all_ID_xlim)

set.seed(123)


#the first track

circos.trackPlotRegion(
  ylim = c(0, 1), track.height = 0.1,  bg.border = NA, bg.col = color_otu2,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data('xlim')
    sector.name = get.cell.meta.data('sector.index')
    #  circos.axis(h = 'top', labels.cex = 0.4, major.tick.length  = 0.4, labels.niceFacing = FALSE)
    circos.text(mean(xlim), 1, sector.name, track.index = get.current.track.index(),
                facing="downward",niceFacing = T, adj = c(-0.1, 0),cex=2,col = 'black')
  } )




#link

corr <- taxonomy

color_corr<-colorRamp2(c(-0.3,0, 0.3), c("#619db8","white", "#c85d4d"),transparency = 0.3)

for(i in seq_len(nrow(corr))) {
  ran<-runif(1, min = 1, max = num[corr[i,2]])    
  ran2<-runif(1, min = 1, max = num[corr[i,1]])
  circos.link(
    corr[i,2],  c(ran-1, ran),
    corr[i,1],c(ran2-1, ran2),
    col = color_corr(corr$corr[i]),
    lwd=0.1
  )
}
circos.clear()


#add legend

library(ComplexHeatmap)

lgd2 <- Legend(title = "Correlation", col_fun = color_corr,title_position="topcenter",title_gp = gpar(fontsize=14),
               grid_width = unit(0.8, "cm"),legend_height = unit(4, "cm"),labels_gp = gpar(col="black",fontsize=14),
               at=c(-0.3,0,0.3))
draw(lgd2,x=unit(0.1,"npc"),y=unit(0.9,"npc"))


circos.clear()
dev.off()


