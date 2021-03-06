library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library("RSQLite")
db<-dbConnect(SQLite(), dbname="dreamsdetails.db")
dreams <- data.frame(dbGetQuery(db, 'select dream from dreamdetails'))

dreamsCorpus <- VCorpus(VectorSource(dreams))
dreamsCorpus <- tm_map(dreamsCorpus, content_transformer(tolower))
dreamsCorpus <- tm_map(dreamsCorpus, removeNumbers)
dreamsCorpus <- tm_map(dreamsCorpus, removePunctuation)
dreamsCorpus <- tm_map(dreamsCorpus, removeWords, stopwords())
dreamsCorpus <- tm_map(dreamsCorpus, stripWhitespace)
dreamsCorpus <- tm_map(dreamsCorpus, PlainTextDocument)

dreamsDTM <- TermDocumentMatrix(dreamsCorpus)
dreamsDTM <- removeSparseTerms(dreamsDTM, 0.999)

dreamsDF <- as.data.frame(as.matrix(dreamsDTM))
dreamsDF$Liked = dreams$Liked
#write.csv(dreamsDF, file="~/sqlite/data/dreamsDF.csv")

dreamsFreqTerms <- findFreqTerms(dreamsDTM, lowfreq = 2)

dreamsFrequency <- rowSums(as.matrix(dreamsDTM))
dreamsFrequency <- subset(dreamsFrequency, dreamsFrequency>=20)
barplot(dreamsFrequency, las=2, col=rainbow(20))

dreamsFrequency2 <- rowSums(as.matrix(dreamsDTM))
dreamsFrequency2 <- subset(dreamsFrequency2, dreamsFrequency2>=500)
barplot(dreamsFrequency2, las=2, col=rainbow(20))

dreamsMatrix <- as.matrix(dreamsDTM)
wordFreq <- sort(rowSums(dreamsMatrix), decreasing = TRUE)
grayLevels <- gray((wordFreq+10)/(max(wordFreq)+10))
wordcloud(words=names(wordFreq),freq=wordFreq, min.freq=200, random.order = F, colors=grayLevels)
wordcloud(words=names(wordFreq),freq=wordFreq, min.freq=200, random.order = F, colors=rainbow(20))




