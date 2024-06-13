#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(tidyverse)
library(gapminder)
library(ggplot2)



species<-readxl::read_xlsx("./cyto network.xlsx",sheet="species")
tba<-readxl::read_xlsx("./cyto network.xlsx",sheet="tba")

total<-rbind(species,tba)


species$direction<-ifelse(species$fdrp>0.2,"nonsig",ifelse(species$corr<0,"neg","pos"))
tba$direction<-ifelse(tba$fdrp>0.2,"nonsig",ifelse(tba$corr<0,"neg","pos"))
total$direction<-ifelse(total$fdrp>0.2,"nonsig",ifelse(total$corr<0,"neg","pos"))



species2 <- species %>%
  make_long(node1, node2)


colnames(tba)[1]<-"node3"

tba2 <- tba %>%
  make_long(node3, node2)

test<-rbind(species2,tba2)

table(test$node)

test$node<-factor(test$node,
                  levels=c(rev(c("Clostridium bolteae","Clostridium sp001517625","Sutterella wadsworthensis A",
                                 "Blautia A obeum B","Lachnospira rogosae","Agathobaculum butyriciproducens",
                                 "Gemmiger formicilis","CAG-279 sp000437795","Secondary bile acid biosynthesis",
                                 "GUDCA","Total UDCA","PBA/SBA","UDCA/CDCA",
                                 "TUDCA","GHDCA"))))


colors <- c('#FFED6F','#fabb6e','#fc8002','#b4b4d5','#8484b1','#614099','#fde5f0','#b9181a','#e2d7fa','#f0eebb','#1663a9','#cedfef',
            
            '#92c2dd',"#4995c6",'#ee4431')

dir_color<-c("grey","#c85d4d", "#313695")

test2<-merge(test,total,by.x=c("node","next_node"),by.y=c("node1","node2"),all.x=T)
test2$corr2<-ifelse(test2$node=="Secondary bile acid biosynthesis",0.25,test2$corr2)


p <- ggplot(test2, aes(x = x, next_x = next_x, node = node, next_node = next_node,
                       label = node,value=corr2)) +
  geom_sankey(flow.alpha = 0.5, 
              smooth = 7, 
              width = 0.5,
              node.color = colors,node.fill = colors,flow.fill="#c85d4d") + 
  geom_sankey_text(size = 3.5, color = 'black') +
  scale_fill_manual(values=colors)+
  theme_void() +
  theme(legend.position = 'none') 

p


width=160
height=100
ggsave(paste0("./sankey.pdf"), p, width = width, height = height, dpi=600, units = "mm")


