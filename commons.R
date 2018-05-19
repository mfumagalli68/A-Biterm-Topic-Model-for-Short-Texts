multinomial<-function(p)
{
 # browser()
  r=runif(1,0,1)
  for( i in 1:length(p))
  {
    r=r-p[i]
    if (r<0)
      return (i)
  }
  
  return (len(p)-1)
}

doc_to_biterm<-function(docs)
{
  library(stringi)
  words<-unlist(stri_extract_all_words(docs))
  biterm<-combn(words,2,simplify = FALSE)
  return (biterm)
}

sample_biterm<-function()
{
  z=multinomial(topic_prop)
  w1=multinomial(topic_terms[z])
  w2=multinomial(topic_terms[z])
  
  return (w1,w2)
}

sample_n<-function(n)
{
  for ( i in 1:length(n))
  {
    return (sample_biterm())
  }
}

normalize<-function(v)
{
  p<-vector()
  j<-1
  norm=sum(v)
  for( i in v)
  {
    p[j]<-i/norm
    j<-j+1
  }
  return (p)
}

initialize<-function(biterms,num_topics,num_words)
{
  topic_sum<-rep(0,num_topics)
  topic_terms<-matrix(0,nrow=num_topics,ncol=num_words)
  colnames(topic_terms)<-unique(unlist(biterms))
  biterm_topics<-rep(0,length(biterms))
  
  for (i in 1:length(biterms))
  {
    w1=biterms[[i]][1]
    w2=biterms[[i]][2]
    z=sample(1:num_topics,1)
    biterm_topics[i]=z
    topic_terms[z,w1]=topic_terms[z,w1]+1
    topic_terms[z,w2]=topic_terms[z,w2]+1
    topic_sum[z]=topic_sum[z]+1
  }
  
  return (list(topic_terms=topic_terms,topic_sum=topic_sum,biterm_topics=biterm_topics))
}
