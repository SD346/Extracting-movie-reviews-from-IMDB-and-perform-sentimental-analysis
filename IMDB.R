library(rvest)
library(XML)
library(magrittr)

# IMDB Reviews #############################
aurl <- "https://www.imdb.com/title/tt8130968/reviews?ref_=undefined&paginationKey=[.show-more__control]/u/ErixErns"
IMDB_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
  i <- i+1
  p=length(rev)
}

length(IMDB_reviews) # Reviews Till load more screen

setwd("C:\\Users\\SD\\Desktop")
write.table(IMDB_reviews,"IMDB.txt",row.names = F)

IMDB <- read.delim(choose.files())
str(IMDB)

#Build Corpus
library(tm)
corpus <- iconv(IMDB$x, to = 'utf-8')
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean text
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeWords,stopwords('english'))
inspect(corpus[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
corpus <- tm_map(corpus,content_transformer(removeURL))
inspect(corpus[1:5])

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

corpus <- tm_map(corpus,gsub,pattern = 'ratings', replacement='rating') #Replacing the word ratings to rating
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeWords,c('rating')) #Removing the common word rating
inspect(corpus[1:5])

corpus <- tm_map(corpus,removeWords,c('movie','badla')) #Removing the common word movie and badla
inspect(corpus[1:5])

#Term document matrix
tdm <- TermDocumentMatrix(corpus)
tdm

tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

#Bar plot
w <- rowSums(tdm)
w <- subset(w,w>=50)
barplot(w,las=2,col=rainbow(50))

#Wordcloud
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)

#Building the wordcloud
wordcloud(words = names(w),freq = w)
#Designing the wordcloud
wordcloud(words = names(w),freq = w,max.words = 100,random.order = F,
          min.freq =10,colors = brewer.pal(8,'Dark2'),
          scale = c(2.5,0.5),rot.per = 0.5)

#Sentiment analysis
install.packages("syuzhet")
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)

#Obtain sentiment scores
corpus1 <- iconv(IMDB$x, to ='utf-8')
s <- get_nrc_sentiment(corpus1)
head(s)

#Barplot
barplot(colSums(s),las=2,col=rainbow(10),
        ylab='Count',main='Sentiment Scores for IMDB Badla reviews')

#Most of the revies of Badla movie are Positive
