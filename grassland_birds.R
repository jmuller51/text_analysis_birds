#Trying out R for text analysis on survey data
#loading neccessary packages -- these should be installed before loading
library(tm)
library(ggplot2)
library(wordcloud)
library (plyr)
library(tidytext)
library(cluster)
library(dplyr)
library(tidyr)
library(topicmodels)

#Creating the corpus
abstracts <- read.csv("data.csv", header=TRUE)
corpus <- Corpus(VectorSource(abstracts$AB))
corpus

#Adding metadata (ids) to corpus -- need to come back to this. 
ids <- as.list(levels(survey$DB_ID))
meta(corpus[[1]])


#Cleaning up data
#Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
#making all lower case
corpus <- tm_map(corpus, content_transformer(tolower))
#Removing English stopwords. Might prefer to custom list for project, removes all version of not -- could be problematic
corpus_nostopwords <-corpus #saving a version of the corpus without stopwords for later use
myStopwords <- c(stopwords('english'), "college") #Add other words you want to exclude to this list
corpus <- tm_map(corpus, removeWords, myStopwords)
#striping extra whitespace
corpus <- tm_map(corpus, stripWhitespace)
#stemming words -- may not want to do - I left it out because it didn't seem helpful. Uncomment the next line to try it out.
#corpus <- tm_map(corpus, stemDocument)
#checking cleaned data
inspect(corpus_nostopwords[32])
inspect(corpus[32])


#Creating a Term Document Matrix
dtm <- DocumentTermMatrix(corpus)
inspect(dtm[])
dtm

#Finding frequent terms
findFreqTerms(dtm, lowfreq=150) #only returns words that appear 150 times or more

#finding Associations - words that often appear near other words
findAssocs(dtm, "grassland", 0.2)
findAssocs(dtm, "grazing", 0.2)
findAssocs(dtm, "wind", 0.5)
findAssocs(dtm, "mating", 0.2)


#Trying to plot
#putting data in a plottable format
freqr <- colSums(as.matrix(dtm))
freqr
class(freqr)

#Create bar chart of most frequent terms
wf=data.frame(term=names(freqr), occurrences=freqr)
p <- ggplot(subset(wf, freqr>200), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Creating a wordcloud
#limit words by specifying minimum frequency
wordcloud(names(freqr), freqr, min.freq = 200, colors=brewer.pal(6, "Dark2"))

#Trying out clustering
#Removing sparse terms
dtm_rm_sparse <- removeSparseTerms(dtm, 0.75) #removes terms that only in less than 75% of the documents -- have to do this or cluster will be unreadable
dtm_rm_sparse


#Hierarchal Clustering
dtm_rm_sparse
d <- dist(t(dtm_rm_sparse), method="euclidian")
fit <- hclust(d=d, method="complete")
fit

plot(fit, hang=-1)

#identifing clusters
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=10) #k is the number of cluster
rect.hclust(fit, k=10, border="red")


#Trying out Topic Models
#getting rid of empty entries in dtm
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

#set a seed so that the output of model is predictable
corpus_lda <-LDA(dtm.new, k=5, control = list(seed =150)) # Setting the seed makes this random process repeatable -- Probably want to do more reading on this.

corpus_topics <-tidy(corpus_lda, matrix="beta")
corpus_topics

#pulling out top 10 terms most common within each topic
corpus_top_terms <- corpus_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Plotting those terms
corpus_top_terms %>%
  mutate(term= reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Looking at terms with the greatest difference in beta between topics 1 and 2
beta_spread <- corpus_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic2))

beta_spread

#Examining per document per topic, called gamma
corpus_documents <- tidy(corpus_lda, matrix = "gamma")
corpus_documents

tidy(dtm.new) %>%
  filter(document == 5) %>%
  arrange(desc(count))

#Using some of the terms to pull assign cateogories to entries. Using regex patterns with keywords that might apply
wind = "wind|turbine"


#Writing a loop to crawl through the assigned terms
inspect(corpus_nostopwords)
corpus_length = length(corpus)

#Checking wind words
total_wind = 0
for (doc in 1:corpus_length){
  keywords = wind
  response <- corpus_nostopwords[[doc]]
  status <-grepl(keywords, response)
  if (status == TRUE){
    total_wind = total_wind + 1
    print(paste0(doc, ": ", corpus_nostopwords[[doc]]))
  }
}
print(total_wind)

