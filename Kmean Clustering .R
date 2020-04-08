wine=read.csv("wine.csv")
str(wine)
library(tidyverse)
library(cluster)
library(factoextra)

wine$class <- NULL
str(wine)
wine$Magnesium <- as.numeric(wine$Magnesium)
wine$proline <- as.numeric(wine$proline)
str(wine)
wine<-scale(wine)
head(wine)

distance<-get_dist(wine)
fviz_dist(distance, gradient = list(low="#AB82FF", mid="white", high="#D01679"))
set.seed(1234)
mycluster<-kmeans(wine, centers=2, nstart=25)
str(mycluster)
fviz_cluster(mycluster, data=wine)
#optimal number of clusters
set.seed(1234)
fviz_nbclust(wine,kmeans, method="wss")
fviz_nbclust(wine,kmeans, method = "silhouette")
gap_stat<-clusGap(wine, FUN=kmeans, nstart=25,K.max=10, B=50)
fviz_gap_stat(gap_stat)

set.seed(1234)
mycluster1<-kmeans(wine, centers=3, nstart=25)
str(mycluster1)
print(mycluster1)

fviz_cluster(mycluster1, data=wine)


wine %>%
  mutate(Cluster=mycluster1$cluster) %>%  
  group_by(Cluster) %>%
  summarise_all("mean")  



original=read.csv("wine.csv")
str(original)

table(mycluster1$cluster,original$class ) 
table(original$class)
table(mycluster1$cluster)


