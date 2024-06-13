library(vegan)
library(stats)
library(FSA)
library(pairwiseAdonis)


rm(list = ls())

#import metadata

metadata=read.table("./metagenome/metadata-include RA-combine TBA.txt", header=T, row.names=1, sep="\t", comment.char="")
metadata$Sample_name=row.names(metadata)



#alpha diversity
#species level

shannon <- read.table("./Total/taxanalysis/diversity/alpha/Species-shannon.txt", header=T,sep="\t")

shannon_2=merge(shannon,metadata,by="Sample_name")

kruskal.test(Shannon_index~factor(Group),data = shannon_2)


#gene level
shannon <- read.table("./Total/taxanalysis/diversity/alpha/Gene-shannon.txt", header=T,sep="\t")

shannon_2=merge(shannon,metadata,by="Sample_name")

kruskal.test(Shannon_index~factor(Group),data = shannon_2)


#beta diversity

#PERMANOVA test--species level

unifrac=read.table("./metagenome/div/Species-bray2.txt", header=T, row.names=1, sep="\t", comment.char="")
head (metadata)

adonis_result_dis1<-adonis2(unifrac~Group, metadata, permutations = 999)
adonis_result_dis1

#PERMANOVA test--gene level
unifrac=read.table("./metagenome/div/Gene-bray2.txt", header=T, row.names=1, sep="\t", comment.char="")
head (metadata)

adonis_result_dis1<-adonis2(unifrac~Group, metadata, permutations = 999)
adonis_result_dis1




