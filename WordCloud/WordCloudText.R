words1 = Corpus(DirSource("/Users/janardhanbonu/OneDrive/MUM/datamining/twitter/modi/"), readerControl=list(language="eng"))
words1 <- tm_map(words1, stripWhitespace)
words1 <- tm_map(words1, tolower)
words1 <- tm_map(words1, removeWords, stopwords("english"))
words1 <- tm_map(words1, PlainTextDocument)
words1 <- tm_map(words1, stemDocument)
words1 <- Corpus(VectorSource(words1))
tdm <- TermDocumentMatrix(words1$text)
m1 <- as.matrix(tdm)
v1<- sort(rowSums(m1), decreasing=TRUE)
d1 <- data.frame(word=names(v1), freq=v1)
wordcloud(d1$word,d1$freq,col=brewer.pal(8,"Set2"), min.freq="1")

# Install the packages TM, wordcloud, RColorBrewer
install.packages(c("tm", "wordcloud", "RColorBrewer"))

# Import the libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

# Read text as a vector
articleCorpus <- readLines("/Users/janardhanbonu/git/BusinessIntelligenceDataMining/WordCloud/NYTMArticle.txt")

# Create corpus
mycorpus = Corpus(VectorSource(articleCorpus))

# Transformation
mycorpus1 = tm_map(mycorpus, removePunctuation)
mycorpus2 = tm_map(mycorpus1, removeWords, stopwords("english"))
mycorpus3 = tm_map(mycorpus2, tolower)
mycorpus4 = tm_map(mycorpus3, stripWhitespace)
mycorpus5 = tm_map(mycorpus4, PlainTextDocument)


# Create Term document matrix. A document-term matrix or 
# term-document matrix is a mathematical matrix that describes 
# the frequency of terms that occur in a collection of documents.
tdm <- TermDocumentMatrix(mycorpus5)

m1 <- as.matrix(tdm)

# Sort data by freqency
v1<- sort(rowSums(m1), decreasing=TRUE)

# create dataframe with data and count/frequency
d1 <- data.frame(word=names(v1), freq=v1)

# Create word cloud
wordcloud(d1$word,d1$freq,col=brewer.pal(8,"Set2"), min.freq="5")
