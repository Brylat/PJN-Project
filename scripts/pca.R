#załadowanie bibliotek

#zmiana katalogu roboczego
#workDir <- "F:/KW/TextMining11S"
workDir <- "F:\\KW\\TextMining11S"
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

#analiza głównych składowych
pca <- prcomp(dtmTfidfBounds)

#wykres dokumentów w przestrzeni dwuwymiatowej
legend <- paste(paste("d", 1:19, sep = ""), rownames(dtmTfidfBounds), sep = "<-")
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
  paste("d", 1:19, sep = ""), 
  col = "orange",
  pos = 4
)
legend(0.01, 0.05, legend, text.font = 3, cex = 0.5, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir, 
  "pca.png",
  sep = "\\"
)
png(file = plotFile)
options(scipen = 5)
plot(
  x, 
  y, 
  #xlim = c(,),
  #ylim = c(,),
  pch = 1, 
  col = "orange"
)
text(
  x, 
  y, 
  paste("d", 1:19, sep = ""), 
  col = "orange",
  pos = 3
)
legend("bottomright", legend, text.font = 3, cex = 0.5, text.col = "orange")
dev.off()
