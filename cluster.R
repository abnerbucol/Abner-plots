getwd()
setwd("C:/Users/admin/Documents/biostat/cluster")
df <- read.csv("fishes.csv")
str(df)
head(df)
#normalize
z <- df[,-c(1,1)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)
#compute distance
distance = dist(nor)
#heirarchical
df.hclust = hclust(distance)
plot(df.hclust)
plot(df.hclust,labels=df$site,main='Default from hclust')
plot(df.hclust,hang=-1, labels=df$site,main='Default from hclust')

#Hierarchical agglomerative clustering using "average" linkage
df.hclust<-hclust(distance,method="average") 
plot(df.hclust,hang=-1) 


