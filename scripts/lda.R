#załadowanie bibliotek
library(topicmodels)

#zmiana katalogu roboczego
#workDir <- "F:/KW/TextMining11S"
workDir <- "C:\\TextMining"
setwd(workDir)

#definicja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir, 
  "frequency_matrix.R",
  sep = "\\"
)
source(sourceFile)

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

#prezentacja dokumentów
document1 <- results$topics[1,]
barplot(
  rev(document1),
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[1],
  xlab = "Prawdopodobieństwo",
  col = 'orange'
)
document9 <- results$topics[9,]
barplot(
  rev(document9),
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[9],
  xlab = "Prawdopodobieństwo",
  col = 'turquoise'
)
document16 <- results$topics[16,]
barplot(
  rev(document16),
  horiz = TRUE,
  las = 1, 
  main = rownames(results$topics)[16],
  xlab = "Prawdopodobieństwo",
  col = 'violet'
)

#udział tematów w słowach
words1 <- c("czarodziej", "czarownica", "wampir")
round(results$terms[,words1],2)

words2 <- c("harry", "łucja", "bell")
round(results$terms[,words2],2)

#podział dokumentów na skupienia na podstawie dominujących tematyk


