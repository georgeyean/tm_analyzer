
##Suppose that there are D total documents and w words.
##the program requires a D \times w matrix (term-document matrix), call this term.doc

##data should be organized so that documents are sorted by author.
##authors is an N x 2 matrix 
##where N is the number of authors.  The first column should identify the first row of the document 
##term matrix that corresponds to that author.  The second column should identify the last row of 
##the document term matrix that corresponds to that author

##n.cats_sup is the number of ``coarse" categories
##n.cats_sub is the number of ``granular" categories

##kappa is a concentration parameter that we are setting here
##
##thetas:authors*4
##gamma: term*4
##mus: term*60


exp.agenda.vonmon<- function(term.doc, authors, n.cats_sup, n.cats_sub, verbose=T, kappa=400){
  
  Log("exp.agenda.vonmon (%d, %d)",  n.cats_sup, n.cats_sub)
  len<- length
  
  ##the likelihood for the dirichlet component of the model
  dir.lik<- function(par, pis, prior){
    alphas = par[1:ncol(pis)]
    alphas<- exp(alphas) 
    ester<- log(ddirichlet(pis,alpha=alphas))
    ## ester[which(ester==-Inf)]<- log(1e-323)
    priors<- dgamma(alphas, rate=prior, shape=1, log=T)
    out<- sum(ester) +sum(priors)
    return(out)
  }
  
  
  n.cats<- n.cats_sub
  n.cats_sup<- n.cats_sup
  
  require(MCMCpack)
  N<- nrow(authors)
  
  ##component labels
  #doc*60
  taus<- matrix(NA, nrow=nrow(term.doc), ncol=n.cats)
  
  #term*60, gamma/normal
  mus<- matrix(NA, nrow=ncol(term.doc), ncol=n.cats)
  for(j in 1:ncol(mus)){
    mus[,j]<- rgamma(ncol(term.doc), shape=10)
  }
  for(j in 1:ncol(mus)){
    mus[,j]<- mus[,j]/sqrt(mus[,j]%*%mus[,j])
  }
  
  
  ##etas in the paper 
  #term*4, gamma/normal
  gammas<- matrix(NA, nrow=ncol(term.doc), ncol=n.cats_sup)
  for(j in 1:ncol(gammas)){
    gammas[,j]<- rgamma(ncol(term.doc), shape=10)
  }
  for(j in 1:ncol(gammas)){
    gammas[,j]<- gammas[,j]/sqrt(gammas[,j]%*%gammas[,j])
  }

  beta<- c(rdirichlet(1, alpha=rep(100, n.cats_sup)))*n.cats
  
  ##nesting of granular categories into coarse categories
  #60*4
  cs<- matrix(NA, nrow=n.cats, ncol=n.cats_sup)
  
  ##estimated prior on the attention to categories, 
  ##alpha in the paper
  #1*60
  thetas<-matrix(NA, nrow=1, ncol=n.cats)
  for(j in 1:nrow(thetas)){
    thetas[j,]<- 20 + rnorm(n.cats, 0,0.025)
  }
  
  
  ##authors' attention to granular topics
  #author*60
  pis<- matrix(NA, nrow=N, ncol=n.cats)
  for(j in 1:N){
    pis[j,]<- thetas + (authors[j,2] - authors[j, 1])/n.cats
  }
  
  ##normalizing the term document matrix
  #term.new<- term.doc
  #for(z in 1:nrow(term.new)){
  #	term.new[z,]<- term.new[z,]/sqrt(term.new[z,]%*%term.new[z,])
  #	}
  #term.doc<- term.new
  
  
  kappa<- kappa
  k<-0
  prior<- 1
  prior<- c(rep(prior, n.cats))
  prior<- prior*-1
  Bayes<- 1
  v<- 0
  z<- 0
  step = 0
  
  while(z==0){
    #browser()
    step = step+1
    thetas.old<- thetas
    pi.gam<- digamma(pis) - digamma(apply(pis, 1, sum))
    exp.pi<- exp(pi.gam)
    
    part1<- kappa*term.doc%*%mus
    
    for(j in 1:N){
      for(k in authors[j,1]:authors[j,2]){
        temp<- part1[k,] - max(part1[k,])
        num<- exp.pi[j,]*exp(temp)
        taus[k,]<- num/sum(num)
      }
    }
    
    ##that estimates taus
    ##now estimating the cs
    
    part2<- kappa*t(mus)%*%gammas
    
    exp.beta<- exp(digamma(beta) - digamma(sum(beta)))
    for(j in 1:n.cats){
      temp<- part2[j,] - max(part2[j,])
      num<- exp.beta*exp(temp)
      cs[j,]<- num/sum(num)
    }
    
    ##ok, now updating the gammas
    
    gammas<- matrix(1/sqrt(ncol(term.doc)), nrow=nrow(gammas), ncol=ncol(gammas))
    
    beta<-  1 + apply(cs, 2, sum)
    
    for(j in 1:n.cats){
      fills<- mus[,j]%o%cs[j,]
      gammas<- gammas + fills
    }
    
    for(j in 1:n.cats_sup){
      gammas[,j]<- gammas[,j]/sqrt(gammas[,j]%*%gammas[,j])
    }
    
    
    mus<- gammas%*%t(cs)
    
    for(j in 1:N){
      for(k in authors[j,1]:authors[j,2]){
        fills<- term.doc[k,]%o%taus[k,]
        mus<- mus + fills
      }
    }
    
    for(j in 1:ncol(mus)){
      mus[,j]<- mus[,j]/sqrt(mus[,j]%*%mus[,j])
    }
    
    
    
    ##this is the Newton-Raphson algorithm from 
    ##Blei, Ng, and Jordan (2003), derived from Minka (2000)
    alpha<- thetas[1,]
    N1<- N
    te<- log(exp.pi)
    suff.stats<- apply(te, 2, sum)/N1
    k<-0
    a<- 0

    while(k==0){
      sum.alpha<- digamma(sum(alpha))
      di.alph<- digamma(alpha)
      grads<- Bayes*prior + N1*(sum.alpha - di.alph + suff.stats)
      qs<- - N1*trigamma(alpha)
      c <- N1*trigamma(sum(alpha))
      b1<- sum(grads/qs) 
      b2<- 1/c + sum(1/qs)
      b<- b1/b2
      g<- (grads-b)/qs
      alpha2<- alpha - g
      ester<- which(alpha2<0)
      if(len(ester)>0){
        alpha2[ester]<- pi + rpois(len(ester), rep(3, len(ester)))
      }
      alpha<- alpha2
      #print(alpha)
      g<- max(g)
      if(abs(g)<1e-8){k<-1}
      a<- a + 1
      if(a>15000){
        Log('Switching to Optim')
        temp.pi<- exp.pi/apply(exp.pi,1, sum)
        temp<- optim(log(thetas[1,]), dir.lik, control=list(trace=100, fnscale=-1), method='BFGS',
                     pis=temp.pi, prior=abs(prior))
        alpha<- exp(temp$par) 
        k<- 1
      }
    }
    
    thetas[1,]<- alpha
    
    ##identifying the specific texts
    sen.list<- list()
    for(j in 1:N){
      ints<- authors[j,1]:authors[j,2]
      sen.list[[j]]<- taus[ints,,drop=F]
    }
    
    for(i in 1:N){
      pre.mle<- apply(sen.list[[i]], 2, sum)
      theta.ch<- thetas
      theta.ch<- theta.ch + pre.mle
      pis[i,]<- theta.ch
    }
    
    
    afr<- abs(thetas.old-thetas)
    if(max(afr)<6.5e-1){z<-1} #1e-5
    cat('\n')
    if(verbose==T){
      Log("(%d, %d) step %d: %.8f",  n.cats_sup,  n.cats, step, max(afr))
      Log('(%d, %d) next it',  n.cats_sup,  n.cats)
    }
  }
  
  out<- list(pis, mus, taus, thetas, cs, beta, gammas)
  names(out)<- c('thetas', 'mus', 'rs', 'alpha', 'cs', 'beta', 'gammas')
  return(out)
  
}