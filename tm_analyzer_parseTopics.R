##writing some function to create the information necessary 
create_subs<- function(object){
  pis<- matrix(NA, nrow= nrow(object$thetas), ncol = ncol(object$thetas))
  for(z in 1:nrow(pis)){
    pis[z,]<- object$thetas[z,]/sum(object$thetas[z,])
  }
  return(pis) #804*60, each topic's prop in each book
}

create_sups<- function(object, pis){
  sup_pis<- matrix(NA, nrow = nrow(object$thetas), ncol=ncol(object$cs))
  class_tops<- apply(object$cs, 1, which.max) # each sub-topic mostly belongs to which topic
  for(z in 1:nrow(sup_pis)){
    for(g in 1:ncol(sup_pis)){
      sup_pis[z,g]<- sum(pis[z, which(class_tops==g)]) #get each book's topic prop (sum of subs)
    }
  }
  return(sup_pis) #804*4
}


##now let's put the topics together for each kind of topic
create_sub_topics<- function(object){
  tops<- matrix(NA, nrow=ncol(object$mus), ncol=10)
  for(z in 1:nrow(tops)){
    part1<- object$mus[,z] - apply(object$mus[,-z], 1, mean)
    tops[z,]<- rownames(object$gammas)[order(part1, decreasing=T)[1:10]]
  }
  return(tops)
}

create_super_topics<- function(object){
  tops<- matrix(NA, nrow=ncol(object$gammas), ncol=10)
  for(z in 1:nrow(tops)){
    part1<- object$gammas[,z] - apply(object$gammas[,-z], 1, mean)
    tops[z,]<- rownames(object$gammas)[order(part1, decreasing=T)[1:10]]
  }
  return(tops)
}	
