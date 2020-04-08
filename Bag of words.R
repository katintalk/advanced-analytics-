library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(quanteda)
tw<-read.csv("ID-textDreams.csv", header = T)

tw$text <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", tw$text)
tw$text <- sapply(tw$text,function(row) iconv(row, "latin1", "ASCII", sub=""))


twCorpus <- VCorpus(VectorSource(tw$text))
subSpace <- content_transformer(function(text, pattern) gsub(pattern, 
                                                             " ", tw$text))
twitterHandleRemover <- function(text) gsub("@\\S+","", tw$text)
shortWordRemover <- function(text) gsub('\\b\\w{1,5}\\b','',tw$text)
urlRemover <- function(text) gsub("http:[[:alnum:]]*","", tw$text)
hashtagRemover <- function(text) gsub("#\\S+","", tw$text)



twCorpus <- tm_map(twCorpus, content_transformer(tolower))
twCorpus <- tm_map(twCorpus, removeNumbers)

twCorpus <- tm_map(twCorpus, removePunctuation)
twCorpus <- tm_map(twCorpus, removeWords, stopwords())
twCorpus <- tm_map(twCorpus, stripWhitespace)
twCorpus <- tm_map(twCorpus, PlainTextDocument)


twDTM <- TermDocumentMatrix(twCorpus)

twDTM <- removeSparseTerms(twDTM, 0.999)

twDF <- as.data.frame(as.matrix(twDTM))
#topfeatures(twDF,n=30)

twDF$Liked = tw$Liked
#write.csv(twfDF,"~/OSU/advance analitics", row.names = FALSE)



twFreq <- findFreqTerms(twDTM, lowfreq = 2)
findFreqTerms(twDTM, lowfreq = 30)
#topfeatures(twDTM,n=30)


twFrequency <- rowSums(as.matrix(twDTM))
twFrequency <- subset(twFrequency, twFrequency>=20)
barplot(twFrequency, las=2, col=rainbow(20))

twFrequency2 <- rowSums(as.matrix(twDTM))
twFrequency2 <- subset(twFrequency2, twFrequency2>=500)
barplot(twFrequency2, las=2, col=rainbow(20))

twMatrix <- as.matrix(twDTM)
wordFreq <- sort(rowSums(twMatrix), decreasing = TRUE)
grayLevels <- gray((wordFreq+10)/(max(wordFreq)+10))
wordcloud(words=names(wordFreq),freq=wordFreq, min.freq=100, random.order = F, colors=grayLevels)
wordcloud(words=names(wordFreq),freq=wordFreq, min.freq=100, random.order = F, colors=rainbow(20))







