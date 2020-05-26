#załadowanie bibliotek
library(wordcloud)
library(slowraker)

#zmiana katalogu roboczego
#workDir <- "F:/KW/TextMining11S"
workDir <- "F:\\KW\\TextMining11S"
setwd(workDir)

#definicja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir, 
  "lda.R",
  sep = "\\"
)
source(sourceFile)

#dla pierwszego dokumentu
##wagi tf jako miara ważności słów
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = TRUE))
keywordsTf1
##wagi tfidf jako miara ważności słów
keywordsTfidf1 <- head(sort(dtmTfidfBoundsMatrix[1,], decreasing = TRUE))
keywordsTfidf1
##lda jako miara ważności słów
importance1 <- c(results$topics[1,]%*%results$terms)
names(importance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(importance1, decreasing = TRUE))
keywordsLda1
##chmura tagów
par(mai = c(0,0,0,0))
wordcloud(corpus[1], max.words = 200,colors=brewer.pal(8, "PuOr"))
##algorytm RAKE
text1 <- as.character(corpus[1])
rake1 <- slowrake(txt = text1, stem = FALSE, stop_pos = NULL)
print(rake1[[1]])
