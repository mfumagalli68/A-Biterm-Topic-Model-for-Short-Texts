
gibbs_sampler<-function(biterms,alpha,beta,num_topics,num_words,n_iter)
{
  #browser()
  
  initialized<-initialize(biterms,num_topics,num_words)
  print("gibbs-sampling intialized...")
  topic_terms=initialized$topic_terms
  topic_sum=initialized$topic_sum
  biterm_topics=initialized$biterm_topics
  
  
  for( s in 1:n_iter)
  {
    begin<-Sys.time()
  for (i in 1:length(biterms))
  {
    list_w1_w2=biterms[[i]]
    
    old_topic<-biterm_topics[i]
    topic_terms[old_topic,list_w1_w2[1]]=topic_terms[old_topic,list_w1_w2[1]]-1
    topic_terms[old_topic,list_w1_w2[2]]=topic_terms[old_topic,list_w1_w2[2]]-1
    topic_sum[old_topic]=topic_sum[old_topic]-1
    m_beta=num_words*beta
    
    posterior<-vector()
    for (z in 1:num_topics)
    {
    posterior[z]=(alpha*topic_sum[z])*(topic_terms[z,list_w1_w2[1]]+beta)/(topic_sum[z]*2+m_beta)*
                           (topic_terms[z,list_w1_w2[2]]+beta)/(topic_sum[z]*2+m_beta)
    }
    posterior_norm<-normalize(posterior)
    
    new_topic=multinomial(posterior_norm)
    biterm_topics[i]<-new_topic
    topic_terms[new_topic,list_w1_w2[1]]=topic_terms[new_topic,list_w1_w2[1]]+1
    topic_terms[new_topic,list_w1_w2[2]]=topic_terms[new_topic,list_w1_w2[2]]+1
    topic_sum[new_topic]=topic_sum[new_topic]+1
    
  }
    end<-Sys.time()-begin
    if(s%%100==0)
      {print (paste("iter.",s,end))}
  }

  return (list(alpha=alpha,beta=beta,num_topics=num_topics,num_words=num_words,
               topic_sum=topic_sum,topic_terms=topic_terms,num_biterms=length(biterms)))
}


#gibbs_sampler(combn(words,2,simplify = FALSE),0.1,0.1,2,3)


as_model<-function(fit_model)
{
  #browser()
  alpha<-fit_model$alpha
  beta<-fit_model$beta
  num_topics<-fit_model$num_topics
  num_words<-fit_model$num_words
  topic_sum<-fit_model$topic_sum
  num_biterms<-fit_model$num_biterms
  topic_terms<-fit_model$topic_terms
  
  k_alpha=num_biterms+(num_topics*alpha)
  m_beta=num_words*beta
  n_topics<-sapply(topic_sum,function(x) x*2)
  
  interm<-vector()
  
  for (z in 1:num_topics)
  {
    for (w in 1:num_words)
    {
      
      interm[w]<-(topic_terms[z,w]+beta)/(n_topics[z]+m_beta)
    }
    topic_terms[z,]<-normalize(interm)
  }
  interm<-vector()
  for (z in 1:num_topics)
  
    {
    interm[z]<-(topic_sum[z] +alpha) / (num_biterms + k_alpha)
  
  }
  topic_proportions<-normalize(interm)
  
  return (list(num_topics=num_topics,num_words=num_words,
               topic_terms=topic_terms,topic_proportions=topic_proportions))
  
}

extract_relevant_word_per_topic<-function(topic_terms,topic_prop)
{
 # browser()
  v<-list()
  z<-which.max(topic_prop)
  top<-ifelse(z==1,"first_topic",ifelse(z==2,"second_topic",ifelse(z==3,"third_topic",
                                                                   ifelse(z==4,"fourth_topic","fifth_topic"))))
  term<-topic_terms[z,]
  term<-term[order(term)][1]
  data<-c(names(term),as.numeric(term),topic=top)
 
  
  return (data)
  
}
