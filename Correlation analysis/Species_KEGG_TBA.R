library(ppcor)
library(openxlsx)
library(reshape2)
library(ggm)
library(psych)



metadata=read.table(".\\combine metagenome\\metadata-incRA.txt", header=T, sep="\t", comment.char="")
head(metadata)
table(metadata$Group)

#Species and Secondary bile acids biosynthesis pathway
tax=read.table(".\\metagenome\\metagenome\\sig species.txt", header=T,  sep="\t", comment.char="",check.names=FALSE)

tba=read.table(".\\metagenome\\metagenome\\bile acid pathway-rela.txt", 
               row.names=1,header=T, sep="\t", comment.char="",check.names=FALSE)

tba<-as.data.frame(t(tba))



total<-merge(metadata,tax,by.x="sample_id",by.y="tax")

total2<-merge(total,tba,by.x="sample_id",by.y="row.names")



L_tax<-colnames(tax)[-1]

L_metab<-colnames(tba)



corrList <- c()
for (i in 1:length(L_tax)) {
  for (j in 1:length(L_metab)) {
    parcor<-partial.r(total2,c(which(colnames(total2)==L_tax[i]),which(colnames(total2)==L_metab[j])),c(3,4,5,10,11,12,19),method="spearman",use="complete")
    ncom<-nrow(na.omit(total2[c(3,4,5,10,11,12,19,which(colnames(total2)==L_tax[i]),which(colnames(total2)==L_metab[j]))]))
    pval<-corr.p(parcor,n=ncom-7,adjust="none")   #n is the number of observations minus the number of variables partialed out
    element <- c(L_tax[i],L_metab[j],as.numeric(parcor[2]),as.numeric(pval$p[2]))
    corrList <- rbind(corrList,element)
  }
}



net <- data.frame(corrList)
colnames(net) <- c("node1","node2","corr","corr.p")  
pval3<-as.vector(net[,4])

fdrp<-as.matrix(p.adjust(pval3,"BH"))

res<-as.data.frame(cbind(net,fdrp))


write.table(res, file = ".\\metagenome\\metagenome\\corr-species and bile acid pathway.txt", sep = "\t",
            row.names = FALSE,
            col.names = TRUE)





#TBA and Secondary bile acids biosynthesis pathway
tax=read.table(".\\metagenome\\metagenome\\sig tba.txt", header=T,  sep="\t", comment.char="",check.names=FALSE)

tba=read.table(".\\metagenome\\metagenome\\bile acid pathway-rela.txt", 
               row.names=1,header=T, sep="\t", comment.char="",check.names=FALSE)

tba<-as.data.frame(t(tba))



total<-merge(metadata,tax,by.x="sample_id",by.y="tax")

total2<-merge(total,tba,by.x="sample_id",by.y="row.names")



L_tax<-colnames(tax)[-1]

L_metab<-colnames(tba)



corrList <- c()
for (i in 1:length(L_tax)) {
  for (j in 1:length(L_metab)) {
    parcor<-partial.r(total2,c(which(colnames(total2)==L_tax[i]),which(colnames(total2)==L_metab[j])),c(3,4,5,10,11,12,19),method="spearman",use="complete")
    ncom<-nrow(na.omit(total2[c(3,4,5,10,11,12,19,which(colnames(total2)==L_tax[i]),which(colnames(total2)==L_metab[j]))]))
    pval<-corr.p(parcor,n=ncom-7,adjust="none")   #n is the number of observations minus the number of variables partialed out
    element <- c(L_tax[i],L_metab[j],as.numeric(parcor[2]),as.numeric(pval$p[2]))
    corrList <- rbind(corrList,element)
  }
}



net <- data.frame(corrList)
colnames(net) <- c("node1","node2","corr","corr.p")  
pval3<-as.vector(net[,4])

fdrp<-as.matrix(p.adjust(pval3,"BH"))

res<-as.data.frame(cbind(net,fdrp))


write.table(res, file = ".\\metagenome\\metagenome\\corr-tba and bile acid pathway.txt", sep = "\t",
            row.names = FALSE,
            col.names = TRUE)













