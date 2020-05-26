#załadowanie bibliotek
library(lsa)

#zmiana katalogu roboczego
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

#analiza ukrytych wymiarów semantycznych (dekompozycja wg. wartości osobliwych)
lsa <- lsa(tdmTfidfBoundsMatrix)
lsa$tk #odpowiednik macierzy U, współrzędne wyrazów
lsa$dk #odpowiednik macierzy V, współrzędne dokumentów
lsa$sk #odpowiednik macierzy D, znaczenie składowych

#przygotowanie danych do wykresu
coordTerms <- lsa$tk%*%diag(lsa$sk)
coorDocs <- lsa$dk%*%diag(lsa$sk)
terms <- c("wiedźmin", "civica", "who", "zachorowanie")
termsImportance <- diag(lsa$tk%*%diag(lsa$sk)%*%t(diag(lsa$sk))%*%t(lsa$tk))
importantTerms <- names(tail(sort(termsImportance),25))
coordTerms <- coordTerms[terms,]
coordTerms <- coordTerms[importantTerms,]
legend <- paste(paste("d", 1:19, sep = ""), rownames(coorDocs), sep = "<-")
x1 <- coorDocs[,1]
y1 <- coorDocs[,2]
x2 <- coordTerms[,1]
y2 <- coordTerms[,2]

#wykres dokumentów i wybranych słów w przestrzeni dwuwymiatowej
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
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
  paste("d", 1:19, sep = ""), 
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
legend("topleft", legend, cex = 0.7, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir, 
  "lsa.png",
  sep = "\\"
)
png(file = plotFile)
options(scipen = 5)
plot(
  x1, 
  y1, 
  xlim = c(-0.2,0.05),
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
  paste("d", 1:19, sep = ""), 
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
legend("bottomleft", legend, cex = 0.5, text.col = "orange")
dev.off()
