##Main bitgram Model
library(topicmodels)
library(stringi)
data("AssociatedPress")
words<-dimnames(AssociatedPress)$Terms[1:100]
#docs<-"IBM software company founded devoted to consultancy processing first happen"
#words<-unlist(unique(stri_extract_all_words(docs)))
source("C:\\Users\\mauro\\Dropbox\\BitGram\\commons.R")
source("C:\\Users\\mauro\\Dropbox\\BitGram\\model.R")

bigram<-combn(words,2,simplify = FALSE)
fit<-gibbs_sampler(bigram,50/2,0.01,10,length(words),50)
model_fit<-as_model(fit)
extract_relevant_word_per_topic(model_fit$topic_terms)
