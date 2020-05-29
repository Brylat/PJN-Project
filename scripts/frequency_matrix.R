#załadowanie bibliotek
library(tm)
library(stringr)

#definicja katalogów funkcjonalnych
inputDir <- ".\\data"
outputDir <- ".\\results"
workspacesDir <- ".\\workspaces"
dir.create(outputDir, showWarnings = FALSE)
dir.create(workspacesDir, showWarnings = FALSE)

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

#utworzenie macierzy częstosci
tdmTfAll <- TermDocumentMatrix(corpus)
dtmTfAll <- DocumentTermMatrix(corpus)
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
      global = c(2,16)
    )
  )
)
tdmTfidfBounds <- TermDocumentMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfidfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    weighting = weightTfIdf,
    bounds = list(
      global = c(2,16)
    )
  )
)
dtmTfBounds <- DocumentTermMatrix(
  corpus, 
  control = list(
    bounds = list(
      global = c(2,16)
    )
  )
)

#konwersja macirzy rzadkich do macierzy klasycznch
tdmTfAllMatrix <- as.matrix(tdmTfAll)
dtmTfAllMatrix <- as.matrix(dtmTfAll)
tdmBinAllMatrix <- as.matrix(tdmBinAll)
tdmTfidfAllMatrix <- as.matrix(tdmTfidfAll)
tdmTfBoundsMatrix <- as.matrix(tdmTfBounds)
tdmTfidfBoundsMatrix <- as.matrix(tdmTfidfBounds)
dtmTfidfBoundsMatrix <- as.matrix(dtmTfidfBounds)
dtmTfBoundsMAtrix <- as.matrix(dtmTfBounds)

#eksport macirzy częstości do pliku .csv
matrixFile <- paste(
  outputDir, 
  "tdmTfidfBounds.csv",
  sep = "\\"
)
write.table(
  tdmTfidfBoundsMatrix,
  file = matrixFile,
  sep = ";",
  dec = ",",
  col.names = NA
)



