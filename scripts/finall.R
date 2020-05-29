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

