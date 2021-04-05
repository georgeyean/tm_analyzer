#' tm_analyzer provides GUI-based utility for nested topic model analysis (see Grimmer, 2013)
#' 
#' The code is extended from:
#' BLAYDES, L., GRIMMER, J., MCQUEEN, A. "Mirrors for Princes and Sultans: Advice on the Art of Governance 
#' in the Medieval Christian and Islamic Worlds. Journal of Politics.
#' 
#'  @author George Yean (george.yean@mail.mcgill.ca/georgeyean@gmail.com)
#'  
#' Current features include:
#' 1. Search for the best model based on exclusivity and semantic coherence
#' 2. Web-based GUI for qualitative analysis
#' 3. Latex output 
#' 
source('tm_analyzer_latex.R')
source('tm_analyzer_html.R')
source('tm_analyzer_searchModel.R')

.GlobalEnv$tma_dir <- "tm_analyzer"
graph_dir <- .GlobalEnv$graph_dir <- paste0(.GlobalEnv$tma_dir, "/graph")

## 
#' @title tm_analyzer
#' @description
#' Main function for building the analyzer
#' @param models model list for analysis
#' @param sup_range range of super topics
#' @param sub_range range of sub topics
#' @param sup_docs list of super documents
#' @param sub_docs list of segment documents
#' @param tdm term document matrix
#' @param sup_tdm
tm_analyzer <- function(models=NULL, sup_range=0, sub_range=0, sup_docs=NULL, sub_docs=NULL, tdm=NULL, sup_tdm=NULL){
     
    if (is.null(models))
        stop("models needed!")
    if (sup_range == 0)
        stop("sup_range needed!")
    if (sup_range == 0)
        stop("sup_range needed!")
    if (is.null(sup_docs))
        stop("sup_docs needed!")
    if (is.null(sub_docs))
        stop("sub_docs needed!")
  
    ## remove and recreate graph directory
    unlink(tma_dir, recursive=TRUE)
    dir.create(tma_dir)
    dir.create(paste(tma_dir, "graph", sep="/"))
    
    ## search optimal model
    searchModel(models, tdm, sub=1)
    searchModel(models, sup_tdm, sub=0)
    

    ######## Build HTML GUI ##############################
    pw_log("Bulding GUI")
    html_top(sup_range, sub_range)
    
    ## parse models to html and latex output
    if (!is.list(models)) {
        model <- models
        tm_analyzer_output(model, sup_range, sub_range, sup_docs, sub_docs)
        html_main_output(model, sup_docs, sub_docs)
    } else {
        for (model in models) {
            tm_analyzer_output(model, sup_range, sub_range, sup_docs, sub_docs)
            html_main_output(model, sup_docs, sub_docs)
        }
    }
    html_bottom()    
}


## 
#' @title tm_analyzer_output
#' @description
# Parse models to three types of output: 
#   1. html, 2. latex, 3. graphs
#' @param model model for output
#' @param sup_docs
#' @param sub_docs
tm_analyzer_output <- function(model, sup_range=0, sub_range=0, sup_docs=NULL, sub_docs=NULL){

    if (is.null(model))
       stop("model needed!")
    if (is.null(sup_docs))
       stop("sup_docs needed!")
    if (is.null(sub_docs))
       stop("sub_docs needed!")  
  
    ## parse the numbers of sup, sub from model
    i <- ncol(model$cs)
    j <- nrow(model$cs)   
    ## defining a length function
    len <- length
    
    
    ######## Print Latex output ##########################
    pw_log("Drawing latex table for model(%d, %d)", i, j)
    create_latex_output(model, i, j, sup_docs, sub_docs)
    
    
    ######## Plot graphs #################################
    
    #############
    #############
    ##Figure 1
    #############
    #############
    pw_log("Drawing Figure 1 for (%d, %d)", i, j)
    
    ##first organizing the output to correspond with the order in the table.
    sub_pis<- create_subs(model) #doc*sub
    sup_pis<- create_sups(model, sub_pis) #doc*sup
    s_average<- apply(sup_pis, 2, mean)
    ords<- order(s_average, decreasing=T)
    
    #sup*10
    super_tops<- create_super_topics(model)#words in supers are auto generated
    #sub*10
    sub_tops<- create_sub_topics(model)
    ##let's put together the corresponding texts for each 
    ##putting together the topics for each section
    
    ##
    sub_topic_doc<- apply(model$rs, 1, which.max)
    super_topic<- apply(model$cs, 1, which.max)
    
    sup_pis<- sup_pis[, ords]
    super_tops<- super_tops[ords, ]
    
    new_super<- c()
    for(z in 1:j){
      new_super[z]<- ords[super_topic[z]]
    }
    
    peace_label<- read.delim('BooksPeaceComm.csv', sep=',')
    un_docs<- unique(as.character(book_info[,1]))
    
    peace_label<- peace_label[match(un_docs, peace_label[,1]),]
    peace<- peace_label[,2]
    
    ################
    ################
    ##Figure 2
    ################
    ################
    pw_log("Drawing Figure 2 for (%d, %d)", i, j)
    
    rows = ifelse(i%%2==0, (i%/%2), (i%/%2+1))
    pic_name2 = sprintf("%s/%d-%d-fig2.jpg", graph_dir, i, j)
    jpeg(pic_name2, type = "cairo", bg = "white", width = 1080, height = 540*rows)
    
    par(mfrow=c(rows, 2))
    par(mar=c(4, 4, 2, 2))
    par(las = 1)
    for(z in 1:i){
      if(z%%2 == 0){
        par(mar = c(4, 2, 2, 2))
      }
      if(z%%2 == 1){
        par(mar = c(4, 4, 2, 2))
      }
      ##creating the over time plots
      plot(sup_pis[,z]~peace_label[,3], pch='', cex=0.8, col=ifelse(peace_label[,2]==1, 'black', 'red'), main = paste('Super ', z, sep=''), xlab = 'Year', ylab = 'Proportion', ylim=c(0,0.7))
      christ<- ifelse(peace_label[,2]==1, 1, 0)
      if(z ==1){
        legend(c(600, 600), c(0.7, 0.6), pch = 20, col=c(gray(0.5), 'black'), legend = c('Peace', 'Comm'), text.col = c(gray(0.5), 'black'), bty = 'n')}
      points(sup_pis[,z]~peace_label[,3], pch=20, col=ifelse(peace_label[,2]==1, 'black', grey(0.5)))
      
      lines(lowess(sup_pis[which(peace==1), z]~peace_label[which(peace==1),3], iter = 0, f = 7/8), col='black', lwd = 3)
      lines(lowess(sup_pis[which(peace==0), z]~peace_label[which(peace==0),3], iter = 0, f = 7/8), col=gray(0.5), lwd = 3)
    }
    dev.off()
    
    ################
    ################
    ##Figure 3
    ################
    ################
    pw_log("Drawing Figure 3 for (%d, %d)", i, j)
    
    pic_name3 = sprintf("%s/%d-%d-fig3.jpg", graph_dir, i, j)
    jpeg(pic_name3, type = "cairo", bg = "white", width = 1080, height = 1080)
    
    return_pis<- function(z, y){
      subset<- which(new_super==z)
      ordered_subset<- subset[order(apply(sub_pis[,subset], 2, mean), decreasing=T)]
      use<- sub_pis[,ordered_subset[y]]
      return(use)
    }
    
    one.one<- return_pis(1, 1)
    three.one<- return_pis(3,1)
    three.three<- return_pis(3,3)
    three.five<- return_pis(3, 5)
    
    create.plot<- function(obj, super, sub){
      
      label<- paste(paste('Super', super), paste('Sub', sub) , sep = ', ')
      
      plot(obj~peace_label[,3], pch=20, cex=0.8, col=ifelse(peace_label[,2]==1, 'black', gray(0.5)), main = label ,  xlab = 'Year', ylab = 'Proportion', ylim=c(0,0.2))
      lines(lowess(obj[which(peace==1)]~peace_label[which(peace==1),3], iter = 0, f = 7/8), col='black', lwd = 3)
      lines(lowess(obj[which(peace==0)]~peace_label[which(peace==0),3], iter = 0, f = 7/8), col=gray(0.5), lwd = 3)
    }
    one.two<- return_pis(1, 2)
    
    create.plot(one.two, super = 1, sub = 2)
    
    
    one.eight<- return_pis(1, 8)
    create.plot(one.eight, super = 1, sub = 2)
    
    two.one<- return_pis(2, 1)
    create.plot(two.one, super = 1, sub = 2)
    
    dev.off()
    
    ################
    ################
    ##Figure 4
    ################
    ################
    pw_log("Drawing Figure 4 for (%d, %d)", i, j)
    
    pic_name3 = sprintf("%s/%d-%d-fig4.jpg", graph_dir, i, j)
    jpeg(pic_name3, type = "cairo", bg = "white", width = 1080, height = 1080)      
    
    par(mfrow=c(3,1))
    par(las = 1)
    par(mar = c(2, 4, 2, 2))
    create.plot(one.one, 1, 1)
    legend(c(575, 575), c(0.2, 0.15), legend = c('Peace', 'Comm'), col=c(grey(0.5), 'black'), text.col=c(grey(0.5), 'black'), bty = 'n', pch = 20 )
    par(mar = c(3, 4, 2.5, 2))
    create.plot(one.two, 1, 2)
    par(mar = c(3, 4, 2.5, 2))
    #create.plot(one.eight, 1, 8)
    #par(mar = c(4, 4, 2, 2))
    create.plot(two.one, 2, 1)
    
    
    three.one<- return_pis(3, 1)
    three.three<- return_pis(3, 3)
    three.five<- return_pis(3, 5)
    three.four<- return_pis(3, 4)
    
    create_plot_christ<- function(obj, super, sub){
      
      label<- paste(paste('Super', super), paste('Sub', sub) , sep = ', ')
      subs<- which(peace==1 & peace_label[,3]>1158)
      plot(obj[subs]~peace_label[subs,3], pch=20, cex=0.8, main = label ,  xlab = 'Year', ylab = 'Proportion', ylim=c(0,0.15))
      lines(lowess(obj[subs]~peace_label[subs,3], iter = 0, f = 7/8), col='black', lwd = 3)
    }
    
    ##the remaining three 
    par(mfrow= c(2, 2))
    par(mar = c(2, 4, 2, 2))
    create.plot(three.one, 3, 1)
    legend(c(575, 575), c(0.2, 0.15), legend = c('Peace', 'Comm'), col=c(grey(0.5), 'black'), text.col=c(grey(0.5), 'black'), bty = 'n', pch = 20 )
    create.plot(three.three, 3, 3)
    create.plot(three.four, 3, 4)
    create.plot(three.five, 3, 5)
    
    dev.off()
}

