#włączenie bibliotek
library(proxy)
library(dendextend)
library(corrplot)
library(flexclust)

#zmiana katalogu roboczego
workDir <- "F:\\KW\\TextMining12S"
setwd(workDir)

#lokalizacja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir,
  "\\",
  "lsa.R",
  sep = ""
)
source(sourceFile)

#analiza skupień
##metoda hierarchiczna
#parametry metody:
# 1. macierz częstości:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. miara odległości (euclidean, jaccard, cosine)
# 3. sposób wyznaczania odległości pomiedzy skupieniami (single, complete, ward.D2)

par(mai = c(1,2,1,1))
nDocuments = 19
#eksperyment 1
distMatrix1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(distMatrix1, method = "ward.D2")
plot(hclust1)
barplot(hclust1$height, names.arg = 18:1)
nClusters1 = 5
clusters1 <- cutree(hclust1, k=nClusters1)
clustersMatrix1 <- matrix(0,nDocuments,nClusters1)
rownames(clustersMatrix1) <- names(clusters1)
for (i in 1:nDocuments) {
  clustersMatrix1[i, clusters1[i]] <- 1
}
corrplot(clustersMatrix1)
dendrogram1 <- as.dendrogram(hclust1)
coloredDendrogram1 <- color_branches(dendrogram1, h = 100)
plot(coloredDendrogram1)
#alternatywne wersje dendrogramów
par(mar=c(16,5,1,1), mgp=c(4,2.5,0))
coloredDendrogram1%>%set("labels_cex", 0.8)%>%plot()
par(mar=c(5,1,1,16), mgp=c(4,2.5,0))
coloredDendrogram1%>%set("labels_cex", 0.9)%>%plot(horiz = T)
par(mai = c(1,2,1,1))

#eksperyment 2
distMatrix2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(distMatrix2, method = "ward.D2")
plot(hclust2)
barplot(hclust2$height, names.arg = 18:1)
nClusters2 = 3
clusters2 <- cutree(hclust2, k=nClusters2)
clustersMatrix2 <- matrix(0,nDocuments,nClusters2)
rownames(clustersMatrix2) <- names(clusters2)
for (i in 1:nDocuments) {
  clustersMatrix2[i, clusters2[i]] <- 1
}
corrplot(clustersMatrix2)
dendrogram2 <- as.dendrogram(hclust2)
coloredDendrogram2 <- color_branches(dendrogram2, h = 1.2)
plot(coloredDendrogram2)

#porównanie wyników eksperymentów
Bk_plot(
  dendrogram1,
  dendrogram2,
  add_E = F,
  rejection_line_asymptotic = F,
  main = "Index Fawlkes'a Mallows'a",
  ylab = "Index Fawlkes'a Mallows'a"
)

##metoda niehierarchiczna
#parametry metody:
# 1. macierz częstości:
# a. waga (weighting)
# b. zakres zmiennych (bounds)
# 2. zakładana liczba skupień

#eksperyment 3
nClusters3 <- 3
kmeans3 <- kmeans(dtmTfidfBounds, centers = nClusters3)
clustersMatrix3 <- matrix(0,nDocuments,nClusters3)
rownames(clustersMatrix3) <- names(kmeans3$cluster)
for (i in 1:nDocuments) {
  clustersMatrix3[i, kmeans3$cluster[i]] <- 1
}
corrplot(clustersMatrix3)

#współczynnik zbieżności podziałów przy zadanej liczbie skupień
##dla 3 skupień
randEx2Ex3 <- randIndex(clusters2, kmeans3$cluster, F)
randEx2Ex3
randEx2Pattern <- randIndex(clusters2, pattern, F)
randEx2Pattern
randEx3Pattern <- randIndex(kmeans3$cluster, pattern, F)
randEx3Pattern


