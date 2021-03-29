source("semanticCoherence.R")
source("exclusivity.R")
source("parseTopics.R")
if(!require(ggplot2))
  install.packages("ggplot2")
library(ggplot2)

#' Calculate semantic coherence of topics in the a model.
#' Refer to paper of Mimno et al 2011. 
#' 
#' @param twm a topic by top keywords matrix.
#' @param tdm a doc by keyword matrix.
#' 
semanticCoherence <- function(twm, tdm, M=10){
  
  #Get the Top N Words
  #top.words <- apply(beta, 1, order, decreasing=TRUE)[1:M,]
  top.words <- t(twm)
  
  wordlist <- unique(as.vector(top.words))
  
  #mat <- mat[,wordlist]
  #mat$v <- ifelse(mat$v>1, 1,mat$v) #binarize
  
  #do the cross product to get co-occurrences
  cross <- matrix(NA, length(wordlist), length(wordlist))
  rownames(cross) <- wordlist
  colnames(cross) <- wordlist
  #cross <- slam::tcrossprod_simple_triplet_matrix(t(mat))
  for(i in 1:nrow(cross)){
    for(j in 1:ncol(cross)){
      ij_tdm <- tdm[, which(colnames(tdm) %in% c(wordlist[i], wordlist[j]))]
      if(i==j){
        cross[i,j] <- length(ij_tdm[ij_tdm>0])
      }else{
        res <- ij_tdm[which(ij_tdm[,1]>0 & ij_tdm[,2]>0), ]
        cross[i,j] <- ifelse(is.null(res), 0, nrow(matrix(res, ncol=2)))
      }
    }
  }
  
  #create a list object with the renumbered words (so now it corresponds to the rows in the table)
  temp <- match(as.vector(top.words),wordlist)
  labels <- split(temp, rep(1:nrow(twm), each=M))
  
  #Note this could be done with recursion in an elegant way, but let's just be simpler about it.
  sem <- function(ml,cross) {
    m <- ml[1]
    l <- ml[2]
    log(.01 + cross[m,l]) - log(cross[l,l] + .01)
  }
  result <- vector(length=nrow(twm))
  for(k in 1:nrow(twm)) {
    grid <- expand.grid(labels[[k]],labels[[k]])
    colnames(grid) <- c("m", "l") #corresponds to original paper
    grid <- grid[grid$m > grid$l,]
    calc <- apply(grid,1,sem,cross)
    result[k] <- sum(calc)
  }
  return(result)
}

#' Search prate-optimal models
#' @param objects object finally computed
#' @param tdm a docs by keywords matrix, see replication.R
#' @param sub 0 means we are calculating for super topics
#' 
searchModel<-function(objects, tdm, sub=1){
  if(length(objects)==0) stop("Need at least one object")
  
  data <- data.frame(K=c(1:length(objects)), 
                     SemanticCoherence=c(1:length(objects)), 
                     Exclusivity=c(1:length(objects)))   
  
  for(i in 1:length(objects)){
    object <- objects[[i]]
    
    if(sub){
      tops <- create_sub_topics(object)
      beta <- object$mus
    } else {
      tops <- create_super_topics(object)
      beta <- object$gammas
    }
    print(paste("Calculating semcoh and excl for: ", sprintf("(%d,%d)", ncol(object$cs), nrow(object$cs)), "..."))
    semcoh <- mean(semanticCoherence(tops, tdm))
    excl <- mean(exclusivity(beta))
    print(paste("result: ", semcoh, excl))
    
    data$K[i] <- sprintf("(%d,%d)", ncol(object$cs), nrow(object$cs))
    data$SemanticCoherence[i] <- semcoh
    data$Exclusivity[i] <- excl
  } 
  
  title <- sprintf("Exclusivity and semantic coherence, Pareto front line (%s)", ifelse(sub,"sub-topic","sup-topic"))
  p <- ggplot(data, aes(SemanticCoherence,Exclusivity))+
     geom_point(size = 2.5, alpha = 0.7, show.legend = FALSE) + 
     geom_text(aes(label=K), nudge_x=.01, nudge_y=ifelse(sub,.0015,.02), show.legend = FALSE) +
     geom_step(direction ="vh", show.legend = FALSE) +
     labs(x = "Semantic coherence",
          y = "Exclusivity",
          title = title)+
     theme_bw()
  print(p)
  #Sys.sleep(5)
  #browser()
  pic <- sprintf("graph/sem_exc_pareto_%s.jpg", ifelse(sub,"sub","sup"))
  ggsave(pic, width = 8, height = 8)
  dev.off()
  return(data)
}