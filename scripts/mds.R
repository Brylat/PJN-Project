#załadowanie bibliotek
library(proxy)

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

#skalowanie wielowymiarowe (MDS)
distCos <- dist(dtmTfidfBoundsMatrix, method = "cosine")
distCosMatrix <- as.matrix(distCos)
mds <- cmdscale(distCos, eig = TRUE, k=2)

#rysowanie wykresu w oknie aplikacji
legend <- paste(paste("d", 1:19, sep = ""), rownames(distCosMatrix), sep = "<-")
x <- mds$points[,1]
y <- mds$points[,2]
plot(
  x,
  y,
  col = "orange"
  xlab = "Synthetic variable 1", 
  ylab = "Syntehtic variable 2",
  main = "Multidimensional Scalling"
)
text(
  x,
  y,
  labels = paste("d", 1:19, sep = ""),
  col = "orange"
  pos = 4
)
legend("bottom", legend, cex = 0.6, text.col = "orange")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir, 
  "mds.png",
  sep = "\\"
)
png(file = plotFile)
plot(
  x,
  y,
  xlab = "Synthetic variable 1", 
  ylab = "Syntehtic variable 2",
  main = "Multidimensional Scalling",
  col = "orange",
  xlim = c(-0.5,0.5)
)
text(
  x,
  y,
  labels = paste("d", 1:19, sep = ""),
  col = "orange"
)
legend("bottom", legend, cex = 0.6, text.col = "orange")
dev.off()
