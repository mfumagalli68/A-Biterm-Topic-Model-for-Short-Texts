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
  fit[[k]]<-gibbs_sampler(bigram,50/3,0.01,5,length(unique(d)),1000)
  k<-k+1
  }
  }
}

#####unique documents
#docs<-unlist(unique(stri_extract_all_words(docs)))
#bigram<-combn(docs,2,simplify = F)
#fit<-gibbs_sampler(bigram,50/3,0.01,3,length(unique(unlist(docs))),500)
#model_fit<-as_model(fit)
#word_topic<-extract_relevant_word_per_topic(model_fit$topic_terms,model_fit$topic_proportions)

model_fit<-lapply(fit,as_model)
relevant_word<-lapply(model_fit, function(x,y) {extract_relevant_word_per_topic(x$topic_terms,x$topic_proportions)})
data_to_plot<-as.data.frame(do.call(rbind,relevant_word))
data_to_plot<-data_to_plot%>%dplyr::rename(terms=V1,prob=V2)%>%mutate(prob=as.numeric(prob))


library(ggplot2)
library(wordcloud2)
wordcloud(data_to_plot$terms,data_to_plot$prob,color=brewer.pal(8, "Dark2")[data_to_plot$topic],ordered.colors = T)
