##Main bitgram Model
library(topicmodels)
library(stringi)
library(readr)
library (tm)
Survey <- read_csv("C:/Users/mauro/Dropbox/BitGram/Survey.csv")
docs<-Survey$`La ringraziamo per il tempo che ci ha dedicato! Vuole aggiungere qualche altre considerazioni sul nostro sito?`

stopwords_regex = paste(stopwords('it'), collapse = '\\b|\\b')
soptwords_regex = paste0('\\b', stopwords_regex, '\\b')
docs = stringr::str_replace_all(docs, stopwords_regex, '')
docs<-gsub('[[:punct:] ]+',' ',docs)
docs<-tolower(docs)
docs<-na.omit(docs)
#words<-unlist(unique(stri_extract_all_words(docs)))
source("C:\\Users\\mauro\\Dropbox\\BitGram\\commons.R")
source("C:\\Users\\mauro\\Dropbox\\BitGram\\model.R")

docs<-unique(stri_extract_all_words(docs))
fit<-list()
k<-1
for (d in docs)
{  
  if(length(d)>1)
  {
    bigram<-combn(d,2,simplify = F)
  if(length(bigram)>1)
  {
  fit[[k]]<-gibbs_sampler(bigram,50/3,0.01,3,length(unique(d)),500)
  k<-k+1
  }
  }
}
model_fit<-lapply(fit,as_model)
relevant_word<-lapply(model_fit, function(x,y) {extract_relevant_word_per_topic(x$topic_terms,x$topic_proportions)})
lapply(relevant_word,count_topic)

count_topic<-function(x)
{
  
}
