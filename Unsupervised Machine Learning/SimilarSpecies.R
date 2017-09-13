#Clearing the Environment Memory
remove(list =ls())
#Importing the data Set
SD=read.csv("C:/Users/Jaivignesh/Downloads/Species_data.csv")
#EDA
summary(SD)
str(SD)
dim(SD)
#missing Value
sum(is.na(SD))
#Removing the Duplicates
Rmdentrez= SD[!duplicated(SD$entrez),]
Rmdsymbol= Rmdentrez[!duplicated(Rmdentrez$symbol),]

#Removing the entrez and symbols since they are unique ids
SD_Cont_data=subset(Rmdsymbol, select = -c(entrez,symbol))
set.seed(111)
#normalizing the data using mean and SD
m=apply(SD_Cont_data,2,mean)
s=apply(SD_Cont_data,2,sd)
NormalizeSD=scale(SD_Cont_data,m,s)

#Transpose is required for clustering the data
TransposeSD=t(NormalizeSD)

#cacluating Euclidean distance
distance=dist(TransposeSD)

#Cluster Dendrogram with complete Linkage
hc.c=hclust(distance)
plot(hc.c,hang = -1)


#Cluster Dendrogram with Average Linkage
hc.a=hclust(distance,method = "average")
plot(hc.a,hang = -1,cex=0.9)

#cluster members
member.c = cutree(hc.c,6)
member.a =cutree(hc.a,6)
table(member.a,member.c)

#silhouette Plot
library(cluster)
plot(silhouette(cutree(hc.c,6),distance))

#apply kmeans
wtss = c()
btss = c()
for(i in 2:10)
{
  kmeanscluster = kmeans(TransposeSD,i)
  wtss = c(wtss,kmeanscluster$tot.withinss)
  btss = c(btss,kmeanscluster$betweenss)
}
plot(wtss)
plot(btss)

#kmeans
kc=kmeans(TransposeSD,6)
kc
kc$cluster
kc$size
D=daisy(TransposeSD)
plot(silhouette(kc$cluster, D))



