##Main bitgram Model

library(stringi)
docs<-"IBM software company founded devoted to consultancy processing first happen"
words<-unlist(unique(stri_extract_all_words(docs)))
source("C:\\Users\\mauro\\Dropbox\\BitGram\\commons.R")
source("C:\\Users\\mauro\\Dropbox\\BitGram\\model.R")

fit<-gibbs_sampler(combn(words,2,simplify = FALSE),50/2,0.01,2,length(words),4)
model_fit<-as_model(fit)
