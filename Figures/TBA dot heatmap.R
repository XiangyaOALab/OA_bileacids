
library(ggplot2)
library(plyr)
library(ggpubr)
library(rstatix)
library(dplyr)
library(reshape2)
library(ComplexHeatmap)
library(circlize)


data <- read.table("./boxplot-dose.txt", header=T,sep="\t")

data$logtba<-log10(data$tba)

data2<-dcast(data, id+unibi+klcat+vas_cat+OST+JSN ~ tbaname, value.var = "logtba")

row.names(data2)<-data2[,1]


# z-score 
data3<-scale(data2[,7:12])

data4<-cbind(data2[,1:6],data3)


#write.table(data4,file="data4.txt",sep="\t",row.names = F)


data5<-data4


#Laterality

data6<-as.data.frame(aggregate(data5[,7:12],by = list(data5$unibi),mean,na.rm=T))
row.names(data6)<-data6[,1]
data6<-data6[,-1]

data6<-data6[, c("GUDCA", "Total UDCA" , "UDCA/CDCA", "PBA/SBA" , 
                 "TUDCA","GHDCA")]


data6_map<-as.data.frame(t(data6))

data6_map2<-data6_map[, c("Control", "Unilateral","Bilateral")]


#KL grade
data7<-as.data.frame(aggregate(data5[,7:12],by = list(data5$klcat),mean,na.rm=T))
row.names(data7)<-data7[,1]
data7<-data7[,-1]

data7<-data7[, c("GUDCA", "Total UDCA" , "UDCA/CDCA", "PBA/SBA" , 
                 "TUDCA","GHDCA")]


data7_map<-as.data.frame(t(data7))

data7_map2<-data7_map[, c("Control", "KL=2","KL>2")]



#Pain severity
data5_2<-subset(data5,vas_cat != "")

data8<-as.data.frame(aggregate(data5_2[,7:12],by = list(data5_2$vas_cat),mean,na.rm=T))
row.names(data8)<-data8[,1]
data8<-data8[,-1]

data8<-data8[, c("GUDCA", "Total UDCA" , "UDCA/CDCA", "PBA/SBA" , 
                 "TUDCA","GHDCA")]


data8_map<-as.data.frame(t(data8))

data8_map2<-data8_map[, c("No pain", "Mild pain","Moderate to severe pain")]



#OST
data9<-as.data.frame(aggregate(data5[,7:12],by = list(data5$OST),mean,na.rm=T))
row.names(data9)<-data9[,1]
data9<-data9[,-1]

data9<-data9[, c("GUDCA", "Total UDCA" , "UDCA/CDCA", "PBA/SBA" , 
                 "TUDCA","GHDCA")]


data9_map<-as.data.frame(t(data9))

data9_map2<-data9_map[, c("0", "1","2","3")]


#JSN
data10<-as.data.frame(aggregate(data5[,7:12],by = list(data5$JSN),mean,na.rm=T))
row.names(data10)<-data10[,1]
data10<-data10[,-1]

data10<-data10[, c("GUDCA", "Total UDCA" , "UDCA/CDCA", "PBA/SBA" , 
                   "TUDCA","GHDCA")]


data10_map<-as.data.frame(t(data10))

data10_map2<-data10_map[, c("0", "1","2","3")]


#combine

total<-cbind(data6_map2,data7_map2,data8_map2,data9_map2,data10_map2)  


total<-total[ c("GUDCA", "Total UDCA" , "UDCA/CDCA", "PBA/SBA" , 
                "TUDCA","GHDCA"),]


category=data.frame(cat=c("Laterality","Laterality","Laterality","KL grade","KL grade","KL grade",
                          "Pain","Pain","Pain","OST","OST","OST","OST","JSN","JSN","JSN","JSN"))


category$cat <- factor(category$cat,levels = c("Laterality","KL grade","Pain","OST","JSN"))

ha = HeatmapAnnotation(cat=category$cat,show_legend = F,
                       col = list(`cat` = c("Laterality" = "#A3D2E2", "KL grade" = "#dfb6bc",
                                            "Pain"="#FFEC8B","OST"="#ADDD8E","JSN"="#BCBDDC")))


split = factor(c("Laterality","Laterality","Laterality","KL grade","KL grade","KL grade",
                 "Pain","Pain","Pain","OST","OST","OST","OST","JSN","JSN","JSN","JSN"),
               levels=c("Laterality","KL grade","Pain","OST","JSN"))

min(total)
max(total)

col_fun = colorRamp2(c(-0.41,0, 0.41), c("#313695","white", "#A50026"))

total_map2<-as.matrix(total)


pdf(paste0("./XO dose_heatmap",  ".pdf"),
    width = 7, height = 5) 

Heatmap(total_map2, cluster_rows = F, cluster_columns = FALSE,col=col_fun,
        #       column_split = split,    
        show_column_names = T, column_names_gp = gpar(fontsize = 14), 
        row_names_gp = gpar(fontsize = 14),
        show_heatmap_legend = F,
        heatmap_legend_param=list((title="z score")), top_annotation = ha,  
        rect_gp = gpar(col = "white"),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.rect(x = x, y = y, width = width, height = height, gp = gpar(col = "white", fill = "#EDEDED"))
          grid.circle(x = x, y = y, r = 0.04,gp = gpar(fill = col_fun(total_map2[i, j]), col = NA))
        }
)


lgd2 <- Legend(title = "z score", direction = "vertical",col_fun = col_fun,title_position="topcenter",title_gp = gpar(fontsize=16),
               grid_width = unit(0.7, "cm"),legend_height = unit(3.5, "cm"),labels_gp = gpar(col="black",fontsize=16)) # at=c(-0.2,0,0.10,0.20,0.3)

draw(lgd2,x=unit(0.85,"npc"),y=unit(0.4,"npc"))

dev.off()
