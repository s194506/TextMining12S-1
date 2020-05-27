#włączenie bibliotek
library(wordcloud)

#zmiana katalogu roboczego
workDir <- "F:\\KW\\TextMining12S"
setwd(workDir)

#lokalizacja katalogu ze skryptami
scriptsDir <- ".\\scripts"

#załadowanie skryptu
sourceFile <- paste(
  scriptsDir,
  "\\",
  "lda.R",
  sep = ""
)
source(sourceFile)

#dla pierwszego dokumentu
##waga tf jako miara ważności słów
keywordsTf1 <- head(sort(dtmTfAllMatrix[1,], decreasing = T))
keywordsTf1

##waga tfidf jako miara ważności słów
keywordsTfidf1 <- head(sort(dtmTfidfBoundsMatrix[1,], decreasing = T))
keywordsTfidf1

##prawdopodobieństwo w LDA jako miara ważności słów
termsImportance1 <- c(results$topics[1,]%*%results$terms)
names(termsImportance1) <- colnames(results$terms)
keywordsLda1 <- head(sort(termsImportance1, decreasing = T))
keywordsLda1

##chmury tagów
par(mai = c(0,0,0,0))
wordcloud(corpus[1], max.words = 200, colors = brewer.pal(8,"PuOr"))
