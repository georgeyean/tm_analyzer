##############
##############
##
##
## Replication file 
##
##
##
###############

options(show.error.locations = TRUE)

library(MCMCpack)
library(Compositional)

##loading the code for the nested topics
source('nestedtopics.R')
source('parallelworker.R')

tdm<- read.delim('ShortTermDoc.csv', sep=',')
comb_final<- c('god', 'god', 'priest', 'minister', 'king', 'minister', 'state')
#tdm<- tdm[,-which(colnames(tdm)=='X0')]
#god_combine<- which(colnames(tdm)=='god')
#gods<- apply(tdm[,which(colnames(tdm)=='lord_cap'|colnames(tdm)=='allah'|colnames(tdm)=='god')], 1, sum)

tdm2<- tdm
#tdm2[,which(colnames(tdm2)=='god')]<- gods
#tdm2<- tdm2[,-which(colnames(tdm2)=='lord_cap'|colnames(tdm2)=='allah')]

#priest_combine<- which(colnames(tdm2)=='priest')
#priests<- apply(tdm2[, c(priest_combine, which(colnames(tdm2)=='imam'))], 1, sum)

tdm3<- tdm2
#tdm3[,which(colnames(tdm3)=='priest')]<- priests
#tdm3<- tdm3[,-which(colnames(tdm3)=='imam')]


#king_combine<- which(colnames(tdm3)=='king')
#kings<- apply(tdm3[,which(colnames(tdm3)=='king'| colnames(tdm3)=='malik')], 1, sum)

tdm4<- tdm3
#tdm4[,which(colnames(tdm4)=='king')]<- kings
#tdm4<- tdm4[,-which(colnames(tdm4)=='malik')]


extra<- c('hanlfa', 'dirham', 'cabd', 'dimna', 'shafici', 'mu', 'oh', 'schanzabeh', 'culama', 'll', 'got', 'telemachu', 'ulyss', 'idomeneu', 'itali', 'ithaca', 'england', 'french', 'philip', 'adrastu', 'troy', 'thoma', 'paul', 'cicero', 'ye', 'whereof', 'loui', 'henri', 'israel', 'st', 'aforesaid') 

#tdm5<- tdm4[,-which(colnames(tdm4) %in% extra)]
tdm5<-tdm4

#remove NewMirror
new_rems<- read.delim('NewMirrorRemTerms.csv', sep=',')

rem_term<- ifelse(as.character(new_rems[,3])=='AM'|as.character(new_rems[,4])=='X', 1, 0)
rem_words<- gsub('\\W', '', as.character(new_rems[which(rem_term==1),1]))

tdm6<- tdm5[, -which(colnames(tdm5) %in% rem_words)]

##putting together the synonym set.  
syns<- strsplit(as.character(new_rems[,2]), split=';')
syn_set<- list()
a<- 0 
for(z in 1:length(syns)){
  if(length(syns[[z]])>0){
    a<- a + 1
    syn_set[[a]]<- c(as.character(new_rems[z,1]), syns[[z]])
  }
}

syn_set2<- syn_set[-9]


##identifies the redundant synonym sets
combs<- list()
for(z in 1:length(syn_set2)){
  ert<- z
  for(k in 1:length(syn_set2)){
    ee<- any(syn_set2[[z]] %in% syn_set2[[k]])
    if(ee==T & k!= z){
      ert<- c(ert, k)}
  }
  combs[[z]]<- ert
}

##we'll use this to put together the final synonym set
use<- 1:76
syn_final<- list()
a<- 0
for(z in 1:76){
  if(z %in% use){
    a<- a + 1
    ee<- combs[[z]]
    out<- unique(c(unlist(syn_set2[ee])))
    syn_final[[a]]<- out
    use<- use[-which(use %in% ee)]
  }
}
syn_abs<- syn_final[-c(4, 8, 12, 15, 21, 24,25,  29, 33, 34, 35, 45)]

tdm7<- tdm6

for(z in 1:length(syn_abs)){
  ee<- syn_abs[[z]]
  p1<- which(colnames(tdm7)==ee[1])
  p2<- which(colnames(tdm7)==ee[2])
  if(length(p1)>0 & length(p2)>0){
    combs<- apply(tdm7[,c(p1, p2)], 1, sum)
    tdm7[,p1]<- combs
    tdm7<- tdm7[,-p2]
  }
}

tdm8<- tdm7[,-which(colnames(tdm7) %in% c('chapter', 'year', 'franc', 'think'))]


book_info<- read.csv2('ShortBook.csv', sep=',')

un_docs<- unique(as.character(book_info[,1]))
authors<- matrix(NA, nrow=len(un_docs), ncol=2)
for(z in 1:nrow(authors)){
  authors[z,1]<- min(which(book_info[,1]==un_docs[z]))
  authors[z,2]<- max(which(book_info[,1]==un_docs[z]))
}

sup_tdm8 <- matrix(NA, nrow=nrow(authors), ncol=ncol(tdm8))
for(z in 1:nrow(sup_tdm8)){
  sup_tdm8[z,] <- apply(tdm8[authors[z,1]:authors[z,2], ], 2, sum)
}
colnames(sup_tdm8) <- colnames(tdm8)


##this is our final term document matrix
tdm8<- as.matrix(tdm8)

##norming the document for preparation in statistical model
normed<- tdm8


for(z in 1:nrow(normed)){
  normed[z,]<- normed[z,]/(sqrt(normed[z,]%*%normed[z,]))
}


#.GlobalEnv$g_60_c_4 = NA
process_tdm <-function(is, js, sup_docs, sub_docs){
  
      Log("starting (%d, %d) %s", is, js, getwd())
      ##Replicating the model run
      set.seed(3991260)
      Log(format(Sys.time(), "%a %b %d %X %Y"))
      g_60_c_4<- exp.agenda.vonmon(normed, authors, is, js, verbose=T, kappa= 1000)
      Log("Finishing (%d, %d)", is, js)
      Log(format(Sys.time(), "%a %b %d %X %Y"))
      save(g_60_c_4, file='Model_4_60_1000_1.RData')
          
      return(g_60_c_4)
} 



sup_range = c(3:5)
sub_range = c(40:55)
# param_list <- list(list(3,40, un_docs, book_info[,2]), list(3,43, un_docs, book_info[,2]),
#                    list(3,46, un_docs, book_info[,2]), list(3,49, un_docs, book_info[,2]),
#                    list(4,46, un_docs, book_info[,2]), list(4,47, un_docs, book_info[,2]))
param_list <-  list(list(3,40, un_docs, book_info[,2]), list(3,43, un_docs, book_info[,2]),
                    list(3,46, un_docs, book_info[,2]), list(3,49, un_docs, book_info[,2]),
                    list(4,46, un_docs, book_info[,2]), list(4,47, un_docs, book_info[,2]),
                    list(4,48, un_docs, book_info[,2]), list(4,49, un_docs, book_info[,2]),#8
                    list(4,50, un_docs, book_info[,2]), list(4,51, un_docs, book_info[,2]),
                    list(4,52, un_docs, book_info[,2]), list(4,53, un_docs, book_info[,2]),
                    list(4,54, un_docs, book_info[,2]), list(4,55, un_docs, book_info[,2]),
                    list(4,56, un_docs, book_info[,2]), list(4,57, un_docs, book_info[,2]),#16
                    list(4,58, un_docs, book_info[,2]), list(4,59, un_docs, book_info[,2]),
                    list(5,50, un_docs, book_info[,2]), list(5,52, un_docs, book_info[,2]),
                    list(5,54, un_docs, book_info[,2]), list(5,56, un_docs, book_info[,2]),
                    list(5,58, un_docs, book_info[,2]), list(5,60, un_docs, book_info[,2]))


# models <- list()
# g_60_c_4 <- process_tdm(5,61, un_docs, book_info[,2])
# models <- append(models, g_60_c_4)
# g_60_c_4 <- process_tdm(5,60, un_docs, book_info[,2])
# models <- append(models, g_60_c_4)
# param_list <- list(list(4,51, un_docs, book_info[,2]), list(4,51, un_docs, book_info[,2]),
#                    list(4,52, un_docs, book_info[,2]), list(4,53, un_docs, book_info[,2]),
#                    list(4,54, un_docs, book_info[,2]), list(4,55, un_docs, book_info[,2]))
models <- paralWorkerBatch(process_tdm, param_list, corenum=8)
#mdoels <- process_tdm(3,40, un_docs, book_info[,2])
#system("say debug")
#browser()


tm_analyzer(models, sup_range, sub_range, sup_docs, sub_docs, tdm8, sup_tdm8)




system("say finished!")
  
