#załadowanie bibliotek
library(tm)
library(hunspell)
library(stringr)
library(lsa)


#zmiana katalogu roboczego
workDir <- "C:\\TextMining"
setwd(workDir)

#definicja katalogów funkcjonalnych
inputDir <- ".\\data"
outputDir <- ".\\results"
scriptsDir <- ".\\scripts"
workspacesDir <- ".\\workspaces"
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspacesDir, showWarnings = FALSE)

#utworzenie korpusu dokmentów
corpusDir <- paste(
  inputDir, 
  "Teksty - orginał",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#usunięcie z tekstów podziału na akapity
pasteParagraphs <- content_transformer(function(x,char) paste(x, collapse = char))
corpus <- tm_map(corpus, pasteParagraphs, " ")

#wstępne przetwarzanie
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
stoplistFile <- paste(
  inputDir, 
  "stopwords_pl.txt",
  sep = "\\"
)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
corpus <- tm_map(corpus, removeWords, stoplist)
corpus <- tm_map(corpus, stripWhitespace)

#usunięcie em dash i 3/4
removeChar <- content_transformer(function(x,pattern) gsub(pattern, "", x))
corpus <- tm_map(corpus, removeChar, intToUtf8(8722))
corpus <- tm_map(corpus, removeChar, intToUtf8(190))

#lematyzacja
polish <- dictionary(lang="pl_PL")
lemmatize <- function(text) {
  simpleText <- str_trim(as.character(text))
  parsedText <- strsplit(simpleText, split = " ")
  newTextVec <- hunspell_stem(parsedText[[1]], dict = polish)
  for (i in 1:length(newTextVec)) {
    if (length(newTextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]
    if (length(newTextVec[[i]]) > 1) newTextVec[i] <- newTextVec[[i]][1]
  }
  newText <- paste(newTextVec, collapse = " ")
  return(newText)
}
corpus <- tm_map(corpus, content_transformer(lemmatize))

#usunięcie rozszerzeń z nazw plików
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)

#eksport zawartości korpusu do plików tekstowych
preprocessedDir <- paste(
  outputDir, 
  "Teksty - przetworzone",
  sep = "\\"
)
dir.create(preprocessedDir)
writeCorpus(corpus, path = preprocessedDir)


#utworzenie korpusu dokmentów
corpusDir <- paste(
  inputDir, 
  "Teksty - przetworzone",
  sep = "\\"
)
corpus <- VCorpus(
  DirSource(
    corpusDir,
    pattern = "*.txt",
    encoding = "UTF-8"
  ),
  readerControl = list(
    language = "pl_PL"
  )
)

#usunięcie rozszerzeń z nazw plików w korpusie
cutExtensions <- function(document){
  meta(document, "id") <- gsub(pattern = "\\.txt$", replacement = "", meta(document, "id"))
  return(document)
}
corpus <- tm_map(corpus, cutExtensions)


#analiza głównych składowych
amcPlotFunction <- function(matrix) {
  pca <- prcomp(matrix)
  legend <- paste(paste("d", 1:20, sep = ""), rownames(matrix), sep = "<-")
  options(scipen = 5)
  x <- pca$x[,1]
  y <- pca$x[,2]
  plot(
    x, 
    y,
    pch = 1, 
    col = "orange"
  )
  text(
    x, 
    y, 
    paste("d", 1:20, sep = ""), 
    col = "orange",
    pos = 4
  )
  legend("topright", legend, text.font = 3, cex = 0.48, text.col = "orange")
}

#Podejście 1
#max i min liczba dokumentów
minMaxDoc <- c(2,16)

dtmTfAll <- DocumentTermMatrix(corpus)

dtmTfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = minMaxDoc
    )
  )
)

dtmSmartBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightSMART,
    bounds = list(
      global = minMaxDoc
    )
  )
)

dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = minMaxDoc
    )
  )
)


amcPlotFunction(dtmTfAll)
amcPlotFunction(dtmTfBounds)
amcPlotFunction(dtmSmartBounds)
amcPlotFunction(dtmTfidfBounds)

#Podejście 2
#max i min liczba dokumentów
minMaxDoc <- c(4,14)

dtmTfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = minMaxDoc
    )
  )
)

dtmSmartBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightSMART,
    bounds = list(
      global = minMaxDoc
    )
  )
)

dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = minMaxDoc
    )
  )
)


amcPlotFunction(dtmTfBounds)
amcPlotFunction(dtmSmartBounds)
amcPlotFunction(dtmTfidfBounds)

#najlepszy rezultat
dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,12)
    )
  )
)
amcPlotFunction(dtmTfidfBounds)

#przybliżenie dla największego skupiska
pca <- prcomp(dtmTfidfBounds)
legend <- paste(paste("d", 1:20, sep = ""), rownames(dtmTfidfBounds), sep = "<-")
options(scipen = 5)
x <- pca$x[,1]
y <- pca$x[,2]
plot(
  x, 
  y,
  ylim=c(-0.1,0.01),
  xlim=c(-0.1,0.4),
  pch = 1, 
  col = "orange"
)
text(
  x, 
  y, 
  paste("d", 1:20, sep = ""), 
  col = "orange",
  pos = 4
)
legend("bottomright", legend, text.font = 3, cex = 0.5, text.col = "orange")


#max i min liczba dokumentów
minMaxDoc <- c(2,16)
#utworzenie macierzy częstosci
tdmTfAll <- TermDocumentMatrix(corpus)
tdmBinAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightBin
  )
)
tdmTfidfAll <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf
  )
)
tdmTfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = minMaxDoc
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = minMaxDoc
    )
  )
)

tdmTfAllMatrix <- as.matrix(tdmTfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)



#analiza ukrytych wymiarów semantycznych (dekompozycja wg. wartości osobliwych)
lsaPlotFunction <- function(matrix, legendPosition) {
  lsa <- lsa(matrix)
  lsa$tk #odpowiednik macierzy U, współrzędne wyrazów
  lsa$dk #odpowiednik macierzy V, współrzędne dokumentów
  lsa$sk #odpowiednik macierzy D, znaczenie składowych
  
  #przygotowanie danych do wykresu
  coordTerms <- lsa$tk%*%diag(lsa$sk)
  coorDocs <- lsa$dk%*%diag(lsa$sk)
  termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
  importantTerms <- names(tail(sort(termsImportance),25))
  coordTerms <- coordTerms[importantTerms,]
  legend <- paste(paste("d", 1:20, sep = ""), rownames(coorDocs), sep = "<-")
  x1 <- coorDocs[,1]
  y1 <- coorDocs[,2]
  x2 <- coordTerms[,1]
  y2 <- coordTerms[,2]
  
  #wykres dokumentów i wybranych słów w przestrzeni dwuwymiatowej
  options(scipen = 5)
  plot(
    x1, 
    y1, 
    #xlim = c(-0.2,0.05),
    #ylim = c(,),
    pch = 1, 
    col = "orange"
  )
  points(
    x2, 
    y2, 
    pch = 2, 
    col = "brown"
  )
  text(
    x1, 
    y1, 
    paste("d", 1:20, sep = ""), 
    col = "orange",
    pos = 4
  )
  text(
    x2, 
    y2, 
    rownames(coordTerms), 
    col = "brown",
    pos = 4
  )
  legend(legendPosition, legend, cex = 0.5, text.col = "orange")
}

lsaPlotFunction(tdmTfAllMatrix, "topleft")
lsaPlotFunction(tdmBinAllMatrix, "left")
lsaPlotFunction(tdmTfidfAllMatrix, "bottomleft")
lsaPlotFunction(tdmTfBoundsMatrix, "topleft")
lsaPlotFunction(tdmTfidfBoundsMatrix, "topleft")


#max i min liczba dokumentów
minMaxDoc <- c(2,12)
#utworzenie macierzy częstosci

tdmTfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = minMaxDoc
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = minMaxDoc
    )
  )
)

tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)

lsaPlotFunction(tdmTfBoundsMatrix, "topleft")
lsaPlotFunction(tdmTfidfBoundsMatrix, "topleft")


########################################## TODO TO DOCUMENT ##############
library(proxy)
library(corrplot)
library(dendextend)
library(flexclust)
library(fossil)

dtmTfAllMatrix <- as.matrix(dtmTfAll)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)
dtmTfBoundsMAtrix <- as.matrix(dtmTfBounds)

#eksperyment 1
dist1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(dist1, method = "ward.D2")
plot(hclust1)
barplot(hclust1$height, names.arg = 20:1)

#eksperyment 2
dist2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(dist2, method = "ward.D2")
plot(hclust2)
barplot(hclust2$height, names.arg = 20:1)

#eksperyment 3
lsa <- lsa(tdmTfidfBoundsMatrix)
coorDocs <- lsa$dk%*%diag(lsa$sk)
dist3 <- dist(coorDocs, method = "cosine")
hclust3 <- hclust(dist3, method = "ward.D2")
plot(hclust3)
barplot(hclust3$height, names.arg = 20:1)


#eksperyment 4
dist1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(dist1, method = "complete")
plot(hclust1)
barplot(hclust1$height, names.arg = 20:1)

#eksperyment 5
dist2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(dist2, method = "complete")
plot(hclust2)
barplot(hclust2$height, names.arg = 20:1)

#eksperyment 6
lsa <- lsa(tdmTfidfBoundsMatrix)
coorDocs <- lsa$dk%*%diag(lsa$sk)
dist3 <- dist(coorDocs, method = "cosine")
hclust3 <- hclust(dist3, method = "complete")
plot(hclust3)
barplot(hclust3$height, names.arg = 20:1)

#eksperyment 7
dist1 <- dist(dtmTfAllMatrix, method = "euclidean")
hclust1 <- hclust(dist1, method = "single")
plot(hclust1)
barplot(hclust1$height, names.arg = 20:1)

#eksperyment 8
dist2 <- dist(dtmTfidfBoundsMatrix, method = "cosine")
hclust2 <- hclust(dist2, method = "single")
plot(hclust2)
barplot(hclust2$height, names.arg = 20:1)

#eksperyment 9
lsa <- lsa(tdmTfidfBoundsMatrix)
coorDocs <- lsa$dk%*%diag(lsa$sk)
dist3 <- dist(coorDocs, method = "cosine")
hclust3 <- hclust(dist3, method = "single")
plot(hclust3)
barplot(hclust3$height, names.arg = 20:1)

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


#podział obiektów na skupienia przy zadanej liczbie klas
hclustOwn <- hclust(dist2, method = "ward.D2")
#eksperyment 1
clusters1 <- cutree(hclustOwn, k = 5)
clustersMatrix1 <- matrix(0, 20, 5)
rownames(clustersMatrix1) <- names(clusters1)
for (i in 1:20){
  clustersMatrix1[i,clusters1[i]] <- 1
}
corrplot(clustersMatrix1)

#eksperyment 2
clusters2 <- cutree(hclust2, k = 4)
clustersMatrix2 <- matrix(0, 20, 4)
rownames(clustersMatrix2) <- names(clusters2)
for (i in 1:20){
  clustersMatrix2[i,clusters2[i]] <- 1
}
corrplot(clustersMatrix2)

#eksperyment 3
clusters3 <- cutree(hclustOwn, k = 6)
clustersMatrix3 <- matrix(0, 20, 6)
rownames(clustersMatrix3) <- names(clusters3)
for (i in 1:20){
  clustersMatrix3[i,clusters3[i]] <- 1
}
corrplot(clustersMatrix3)



#niehierarchiczna (k-średnich)
#parametry metody:
#1. macierz częstości
#a. waga (weighting)
#b. zakres amiennych (bounds)
#2. zakładana liczba skupień

#eksperyment 4
kmeans1 <- kmeans(dtmTfidfBounds, centers = 5)
clustersMatrix4 <- matrix(0, 20, 5)
rownames(clustersMatrix4) <- names(kmeans1$cluster)
for (i in 1:20){
  clustersMatrix4[i,kmeans1$cluster[i]] <- 1
}
corrplot(clustersMatrix4)

#współczynnik zbieżności klasyfikacji przy zadanej liczbie klas
randEx1Ex4 <- rand.index(clusters1, kmeans1$cluster)
randEx2Ex4 <- rand.index(clusters2, kmeans1$cluster)
randEx3Ex4 <- rand.index(clusters3, kmeans1$cluster)
randEx2Ex3 <- rand.index(clusters2, clusters3)


library(topicmodels)
#analiza ukrytej alokacji Dirichlet'a
nWords <- ncol(dtmTfAll)
nTopics <- 5
lda <- LDA(
  dtmTfAll, 
  k = nTopics, 
  method = "Gibbs", 
  control = list(
    burnin = 2000, 
    thin = 100, 
    iter = 3000
  )
)
perplaxity <- perplexity(lda, dtmTfAll)
results <- posterior(lda)

#prezentacja tematów
par(mai = c(1, 2, 1, 1))
topic1 <- head(sort(results$terms[1,], decreasing = TRUE), 20)
barplot(
  rev(topic1),
  horiz = TRUE,
  las = 1, 
  main = "Temat 1",
  xlab = "Prawdopodobieństwo",
  col = 'orange'
)
topic2 <- head(sort(results$terms[2,], decreasing = TRUE), 20)
barplot(
  rev(topic2),
  horiz = TRUE,
  las = 1, 
  main = "Temat 2",
  xlab = "Prawdopodobieństwo",
  col = 'turquoise'
)
topic3 <- head(sort(results$terms[3,], decreasing = TRUE), 20)
barplot(
  rev(topic3),
  horiz = TRUE,
  las = 1, 
  main = "Temat 3",
  xlab = "Prawdopodobieństwo",
  col = 'violet'
)
topic4 <- head(sort(results$terms[4,], decreasing = TRUE), 20)
barplot(
  rev(topic4),
  horiz = TRUE,
  las = 1, 
  main = "Temat 4",
  xlab = "Prawdopodobieństwo",
  col = 'violet'
)
topic5 <- head(sort(results$terms[5,], decreasing = TRUE), 20)
barplot(
  rev(topic5),
  horiz = TRUE,
  las = 1, 
  main = "Temat 5",
  xlab = "Prawdopodobieństwo",
  col = 'violet'
)
#prezentacja dokumentów
documentBarPlot <- function(n) {
  document <- results$topics[n,]
  barplot(
    rev(document),
    horiz = TRUE,
    las = 1, 
    main = rownames(results$topics)[n],
    xlab = "Prawdopodobieństwo",
    col = 'orange'
  )
}

documentBarPlot(1)
documentBarPlot(2)
documentBarPlot(3)
documentBarPlot(4)
documentBarPlot(5)
documentBarPlot(6)
documentBarPlot(7)
documentBarPlot(8)
documentBarPlot(9)
documentBarPlot(10)
documentBarPlot(11)
documentBarPlot(12)
documentBarPlot(13)
documentBarPlot(14)
documentBarPlot(15)
documentBarPlot(16)
documentBarPlot(17)
documentBarPlot(18)
documentBarPlot(19)
documentBarPlot(20)

#udział tematów w słowach
words1 <- c("civic", "goku", "ludzi", "ciri", "król")
round(results$terms[,words1],2)

words2 <- c("straż", "wirus", "silnik", "kula", "wiedźmin")
round(results$terms[,words2],2)


#słowa / frazy kluczowe
library(wordcloud)
library(slowraker)

keyWordsMethods <- function(n) {
  #czyszczenie konsoli
  cat("\014")
  ##wagi tf jako miara ważności słów
  keywordsTf1 <- head(sort(dtmTfAllMatrix[n,], decreasing = TRUE))
  print("wagi tf jako miara ważności słów")
  print(keywordsTf1)
  ##wagi tfidf jako miara ważności słów
  keywordsTfidf1 <- head(sort(dtmTfidfBoundsMatrix[n,], decreasing = TRUE))
  print("wagi tfidf jako miara ważności słów")
  print(keywordsTfidf1)
  ##lda jako miara ważności słów
  importance1 <- c(results$topics[n,]%*%results$terms)
  names(importance1) <- colnames(results$terms)
  keywordsLda1 <- head(sort(importance1, decreasing = TRUE))
  print("lda jako miara ważności słów")
  print(keywordsLda1)
  ##chmura tagów
  par(mai = c(0,0,0,0))
  wordcloud(corpus[n], max.words = 200,colors=brewer.pal(8, "PuOr"))
  ##algorytm RAKE
  text <- as.character(corpus[n])
  rake <- slowrake(txt = text, stem = FALSE, stop_pos = NULL)
  print(rake[[1]])
}

keyWordsMethods(1)
keyWordsMethods(2)
keyWordsMethods(3)
keyWordsMethods(4)
keyWordsMethods(5)
keyWordsMethods(6)
keyWordsMethods(7)
keyWordsMethods(8)
keyWordsMethods(9)
keyWordsMethods(10)
keyWordsMethods(11)
keyWordsMethods(12)
keyWordsMethods(13)
keyWordsMethods(14)
keyWordsMethods(15)
keyWordsMethods(16)
keyWordsMethods(17)
keyWordsMethods(18)
keyWordsMethods(19)
keyWordsMethods(20)
