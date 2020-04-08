wine=read.csv("wine.csv")
str(wine)
wine$Magnesium <- as.numeric(wine$Magnesium)
wine$proline <- as.numeric(wine$proline)
wine$class <- NULL
str(wine)


distance=dist(wine,method = "euclidean")
clusterWine=hclust(distance, method="ward.D2")
plot(clusterWine)
set.seed(1234)
clusterGroups=cutree(clusterWine, k=3)
tapply(wine$Magnesium, clusterGroups, mean)





wineMatrix=as.matrix(wine)
str(wineMatrix)
wineVector=as.vector(wineMatrix)
str(wineVector)
distance=dist(wineVector, method = "euclidean")
clusterIntensity=hclust(distance, method = "ward.D2")
plot(clusterIntensity)

rect.hclust(clusterIntensity,k=3, border="red")
set.seed(1234)
wineClusters=cutree(clusterIntensity,k=3)
wineClusters


tapply(wineVector, wineClusters, mean)

