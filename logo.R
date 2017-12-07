library(XML)
library(xml2)
library(rvest)
library(dplyr) 
library(tm)

web_address <- 'https://www.forbes.com/sites/lisaarthur/2013/08/15/what-is-big-data/#4a4e83355c85'
web_address <- 'http://searchbusinessanalytics.techtarget.com/definition/big-data-analytics'
web_address <- 'https://en.wikipedia.org/wiki/Data_analysis'

webpage_code<-read_html(web_address)
main <- webpage_code %>% html_nodes('p') %>% html_text()

main2 <- webpage_code %>% html_nodes('p') %>% html_text()

main <- append(main2,main)

myCorpus = Corpus(VectorSource(main))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
myCorpus = tm_map(myCorpus, removeWords, 'can')

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

m = as.matrix(myDTM)
#m <- as.data.frame(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
#n <- rownames(m)
#s <- rowSums(m)
#w <- as.data.frame(n,s)
w<-names(v)
w<-as.data.frame(w)

v <- as.data.frame(v)

s <- cbind(w,v)


library(wordcloud)
set.seed(4363)
wordcloud(names(v), v, min.freq = 1,,colors=brewer.pal(8, "Dark2"))

library(wordcloud2)
wordcloud2(s, figPath = 'D&A3.png', color = "skyblue")
wordcloud2(s,color='skyblue', figPath = 'tw.jpg')

letterCloud( s, word = " & ", color="orange")

wordcloud(s$w, s$v, random.order=FALSE, colors=brewer.pal(8, "Dark2"),random.color= TRUE)

wordcloud(s, random.order=FALSE, colors=brewer.pal(8, "Dark2"),random.color= TRUE)

wordcloud(myCorpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  
          random.color= TRUE,random.order = FALSE,  max.words = 150)

