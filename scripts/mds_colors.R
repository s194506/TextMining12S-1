#włączenie bibliotek

#zmiana katalogu roboczego
workDir <- "F:\\KW\\TextMining12S"
setwd(workDir)

#lokalizacja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptów
sourceFile <- paste(
  scriptsDir,
  "\\",
  "lda.R",
  sep = ""
)
eval(parse(sourceFile, encoding="UTF-8"))
sourceFile <- paste(
  scriptsDir,
  "\\",
  "clustering.R",
  sep = ""
)
eval(parse(sourceFile, encoding="UTF-8"))

#wzorce kolorystyczne
##pattern
colors=c()
for (i in 1:length(corpus)) {
  colors[i] <- switch(pattern[i],"turquoise","orange","violet","lightskyblue","darkseagreen")
}
##LDA
topics <- strtoi(colnames(results$topics)[apply(results$topics,1,which.max)])
colors=c()
for (i in 1:length(corpus)) {
  colors[i] <- switch(topics[i],"turquoise","orange","violet","lightskyblue","darkseagreen")
}
##analiza skupień
colors=c()
for (i in 1:length(corpus)) {
  colors[i] <- switch(clusters1[i],"turquoise","orange","violet","lightskyblue","darkseagreen")
}

#skalowanie wielowymiarowe
distCos <- dist(dtmTfidfBoundsMatrix, method = "cosine")
mds <- cmdscale(distCos,eig=TRUE, k=2)
x <- mds$points[,1]
y <- mds$points[,2]

#przygotowanie legendy
legend <- paste(
  paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""),
  rownames(dtmTfidfBoundsMatrix),
  sep = "<-"
)

#wykres dokumentów w przestrzeni dwuwymiarowej
plot(
  x,
  y,
  #xlim = c(-0.5,-0.2),
  #ylim = c(-0.2,0.1),
  xlab="Współrzędna syntetyczna 1", 
  ylab="Współrzędna syntetyczna 2",
  main="Skalowanie wielowymiarowe", 
  col = colors
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = colors
)
legend("bottom", legend, cex=.5, text.col = "black")

#eksport wykresu do pliku .png
plotFile <- paste(
  outputDir,
  "\\",
  "mds.png",
  sep = ""
)
png(file = plotFile)
plot(
  x,
  y,
  xlab="Współrzędna syntetyczna 1", 
  ylab="Współrzędna syntetyczna 2",
  main="Skalowanie wielowymiarowe", 
  col = colors
)
text(
  x, 
  y, 
  labels = paste("d", 1:length(rownames(dtmTfidfBoundsMatrix)),sep = ""), 
  pos = 3,
  col = colors
)
legend("bottom", legend, cex=.65, text.col = "black")
dev.off()
