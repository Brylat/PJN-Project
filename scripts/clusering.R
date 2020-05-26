#załadowanie bibliotek
library(proxy)
library(corrplot)
library(dendextend)
library(flexclust)
library(fossil)

#zmiana katalogu roboczego
#workDir <- "F:/KW/TextMining11S"
workDir <- "C:\\KW\\TextMining11S"
setwd(workDir)

#definicja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir, 
  "lsa.R",
  sep = "\\"
)
source(sourceFile)

#analiza skupień
#hierarchiczna
#parametry metody:
#1. macierz częstości
#a. waga (weighting)
#b. zakres amiennych (bounds)
#2. miara odległości (euclidean, jaccard, cosine)
#3. odległość pomiędzy skupieniami (single, complete, ward.D2)

#eksperyment 1
dist1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(dist1, method = "ward.D2")
plot(hclust1)
barplot(hclust1$height, names.arg = 18:1)

#eksperyment 2
dist2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(dist2, method = "ward.D2")
plot(hclust2)
barplot(hclust2$height, names.arg = 18:1)

#eksperyment 3
dist3 <- dist(coorDocs, method = "cosine")
hclust3 <- hclust(dist3, method = "ward.D2")
plot(hclust3)
barplot(hclust3$height, names.arg = 18:1)

#podział obiektów na skupienia przy zadanej liczbie klas
#eksperyment 1
clusters1 <- cutree(hclust1, k = 4)
clustersMatrix1 <- matrix(0, 19, 4)
rownames(clustersMatrix1) <- names(clusters1)
for (i in 1:19){
  clustersMatrix1[i,clusters1[i]] <- 1
}
corrplot(clustersMatrix1)

#eksperyment 2
clusters2 <- cutree(hclust2, k = 3)
clustersMatrix2 <- matrix(0, 19, 3)
rownames(clustersMatrix2) <- names(clusters2)
for (i in 1:19){
  clustersMatrix2[i,clusters2[i]] <- 1
}
corrplot(clustersMatrix2)

#eksperyment 3
clusters3 <- cutree(hclust3, k = 3)
clustersMatrix3 <- matrix(0, 19, 3)
rownames(clustersMatrix3) <- names(clusters3)
for (i in 1:19){
  clustersMatrix3[i,clusters3[i]] <- 1
}
corrplot(clustersMatrix3)

#porównanie wyników eksperymentów
dendrogram1 <- as.dendrogram(hclust1)
dendrogram2 <- as.dendrogram(hclust2)
dendrogram3 <- as.dendrogram(hclust3)

Bk_plot(
  dendrogram1, 
  dendrogram2, 
  add_E = FALSE,
  rejection_line_asymptotic = FALSE,
  main = "Indeks Fawlks'a - Mallows'a",
  ylab = "Indeks Fawlks'a - Mallows'a"
)

Bk_plot(
  dendrogram1, 
  dendrogram3, 
  add_E = FALSE,
  rejection_line_asymptotic = FALSE,
  main = "Indeks Fawlks'a - Mallows'a",
  ylab = "Indeks Fawlks'a - Mallows'a"
)

Bk_plot(
  dendrogram2, 
  dendrogram3, 
  add_E = FALSE,
  rejection_line_asymptotic = FALSE,
  main = "Indeks Fawlks'a - Mallows'a",
  ylab = "Indeks Fawlks'a - Mallows'a"
)

#niehierarchiczna (k-średnich)
#parametry metody:
#1. macierz częstości
#a. waga (weighting)
#b. zakres amiennych (bounds)
#2. zakładana liczba skupień

#eksperyment 4
kmeans1 <- kmeans(dtmTfidfBounds, centers = 3)
clustersMatrix4 <- matrix(0, 19, 3)
rownames(clustersMatrix4) <- names(kmeans1$cluster)
for (i in 1:19){
  clustersMatrix4[i,kmeans1$cluster[i]] <- 1
}
corrplot(clustersMatrix4)

#współczynnik zbieżności klasyfikacji przy zadanej liczbie klas
randEx1Ex4 <- rand.index(clusters1, kmeans1$cluster)
randEx2Ex4 <- rand.index(clusters2, kmeans1$cluster)
randEx3Ex4 <- rand.index(clusters3, kmeans1$cluster)
randEx2Ex3 <- rand.index(clusters2, clusters3)

