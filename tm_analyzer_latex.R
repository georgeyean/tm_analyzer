##creating some table functions
source('tm_analyzer_parseTopics.R')

create_latex_output<- function(model, sup, sub, sup_docs, sub_docs){
  
  #parse model content
	sub_pis<- create_subs(model)
	sup_pis<- create_sups(model, sub_pis)
	subdoc_sub_pis = model$rs
	s_average<- apply(sup_pis, 2, mean)
	ords<- order(s_average, decreasing=T)
	
	super_tops<- create_super_topics(model)
	sub_tops<- create_sub_topics(model)
	topic.set<- apply(model$cs, 1, which.max)
	
	#create latex output
	print_output(ords, sup_pis, sub_pis, super_tops, sub_tops, topic.set)
}

##
# Create latex output
##
print_output <- function(ords, sup_pis, sub_pis, super_tops, sub_tops, topic.set){
  sup <- "sup topic"
  sub <- "sub topic"
  pw_log('\\begin{table}[hbt!]', '\n')
  caps<- paste(paste('\\caption{', paste(paste(sup, 'Coarse', sep =' '), paste(sub, 'Granular', sep=' '), sep=','), sep=''), '}', sep='')
  pw_log(caps, '\n')
  pw_log('\\begin{center}', '\n')
  pw_log('\\begin{footnotesize}', '\n')
  pw_log('\\begin{tabular}{lll}', '\n')
  
  ##first we want to reorder the topics
  for(z in 1:len(ords)){
    keys<- super_tops[ords[z], 1:10]
    nums<- mean(sup_pis[,ords[z]])
    ee<- paste(z, keys[1], sep='. ')
    for(m in 2:10){
      ee<- paste(ee, keys[m], sep=',')
    }
    pw_log('\\hline', '\n')	
    pw_log(paste(paste(ee, round(nums,3), sep='-'), '\\\\'), '\n')
    pw_log('\\hline', '\n')
    
    other<- which(topic.set==ords[z])
    if(len(other)>1){
      ert<- apply(sub_pis[, other], 2, mean)}
    if(len(other)==1){
      ert<- mean(sub_pis[,other])}
    new_ords<- other[order(ert, decreasing=T)]
    props<- ert[order(ert, decreasing=T)]
    for(h in 1:len(new_ords)){
      n_key<- sub_tops[new_ords[h],1:10]
      ff<- paste(h, n_key[1], sep='&')
      for(g in 2:10){
        ff<- paste(ff, n_key[g], sep =',')}
      pw_log(paste(paste(ff, round(props[h],3), sep='&'), '\\\\', sep=''), '\n')
    }
    pw_log('\\hline', '\n')
  }
  pw_log('\\end{tabular}', '\n')
  pw_log('\\end{footnotesize}', '\n')
  pw_log('\\end{center}', '\n')
  pw_log('\\end{table}', '\n')
}

