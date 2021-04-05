#' Functions for output the main html file, which is the GUI of the analyzer
#'

gui_file <- paste(.GlobalEnv$tma_dir, "tm_analyzer_gui.html", sep="/")
css_js <- "tm_analyzer_js_css_patch"


##
# Create html output
##
html_main_output<- function(model, sup_docs, sub_docs){

  ## parse the numbers of sup, sub from model
  sup <- ncol(model$cs)
  sub <- nrow(model$cs)
  
  ## parse model content
  sub_pis<- create_subs(model)
  sup_pis<- create_sups(model, sub_pis)
  subdoc_sub_pis = model$rs
  s_average<- apply(sup_pis, 2, mean)
  ords<- order(s_average, decreasing=T)
  
  ## generate html 
  super_tops<- create_super_topics(model)
  sub_tops<- create_sub_topics(model)
  topic.set<- apply(model$cs, 1, which.max) #which sub belongs to which sup 
  finalstr <- sprintf("<div class='run_wrap'>\
                         <div class='run_title z-depth-1' >\
                           <a id='t-%s-%s'></a>Model (%d, %d) \
                         </div>\
                           <a href='#topics_header'>Back to Top</a>", sup, sub, sup, sub)
  graph_str <- sprintf("<div class='img_wrap'>
                          <i class='material-icons'>photo_library</i>\
                          <span class='img_btn z-depth-1'>%s-%s-fig2.jpg</span>\
                          <span class='img_btn z-depth-1'>%s-%s-fig3.jpg</span>\
                          <span class='img_btn z-depth-1'>%s-%s-fig4.jpg</span>\
                        </div>", sup, sub, sup, sub, sup, sub)
  finalstr= sprintf("%s%s", finalstr, graph_str)
  
  ## get subdoc for each subtopic
  subbook_subtopic_m <- apply(subdoc_sub_pis, 1, which.max) 
  
  ##first we want to reorder the topics
  for(z in 1:len(ords)){
    keys<- super_tops[ords[z], 1:10]
    nums<- mean(sup_pis[, ords[z]])
    ee<- paste(z, keys[1], sep='. ')
    for(m in 2:10){
      ee<- paste(ee, keys[m], sep=',')
    }
    finalstr <- sprintf("%s<div class='sup_keys z-depth-1'>\
                            <i class='adown'></i>\
                            <span class='keys'>%s</span>\
                        <div class='keys_books'>", finalstr, 
                                                   paste(ee, round(nums, 3), sep=' | '))
    #find top ten documents for the super topic
    topbooks <- order(sup_pis[, ords[z]], decreasing=T)[0:10]
    for(t in 1:len(topbooks)){
        book_str <- sprintf("<div class='sup_keys_book' val='%s'>%s | %s</div>", 
                             sup_docs[topbooks[t]],
                             sup_docs[topbooks[t]], 
                             round(sup_pis[topbooks[t], ords[z]], 3))
        finalstr <- sprintf("%s\n%s", finalstr, book_str)
    }
    finalstr <- sprintf("%s\n</div></div>", finalstr)
    
    finalstr <- sprintf("%s<div class='sub_wrap z-depth-1'>", finalstr)
    other <- which(topic.set==ords[z]) #get sup's all subs
    if(len(other)>1){
        ert <- apply(sub_pis[, other], 2, mean)}
    if(len(other)==1){
        ert <- mean(sub_pis[, other])}
    
    new_ords <- other[order(ert, decreasing=T)]
    props <- ert[order(ert, decreasing=T)]
    for(h in 1:len(new_ords)){
        n_key <- sub_tops[new_ords[h],1:10]
        ff <- paste(h, n_key[1], sep='. ')
        for(g in 2:10){
            ff <- paste(ff, n_key[g], sep =',')}
        finalstr <- sprintf("%s<div class='sub_keys  z-depth-1'>\
                              <i class='adown'></i>\  
                              <span class='keys'>%s</span>\
                              <div class='keys_books'>", finalstr, 
                                                         paste(ff, round(props[h],4), sep=' | '))
        subbooks <- sub_docs[which(subbook_subtopic_m==new_ords[h])]
        for(b in 1:len(subbooks)){
            book_str <- sprintf("<div class='sub_keys_book' val='%s'>%s</div>", subbooks[b],subbooks[b])
            finalstr <- sprintf("%s\n%s", finalstr, book_str)
        }
        finalstr <- sprintf("%s</div></div>", finalstr)
    }
    finalstr <- sprintf("%s</div>", finalstr)
  }
  finalstr <- sprintf("%s</div>", finalstr)
  
  write(finalstr, file=gui_file, append=TRUE)
}

##
# The top part of the GUI
##
html_top <- function(sup_range, sub_range){
  
    write("<html>", file=gui_file, append=TRUE)
  
    jquerycdn <- "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"
    jqueycss <- "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"
    ggicon <- "https://fonts.googleapis.com/icon?family=Material+Icons"
    js_css <- readChar(css_js, file.info(css_js)$size)
    js_css <- gsub("\n", "", js_css)
    htmlheader <- sprintf("<head>\
                            <script src='%s'></script><link rel='stylesheet' href='%s'>\
                            <link rel='stylesheet' href='%s'>%s\
                          </head><div>", jquerycdn, jqueycss, ggicon, js_css)
    write(htmlheader, file=gui_file, append=TRUE)
    
    write("<div id='book_rootlink' val='' sub_val=''> \
            <div class='intro z-depth-1'><b>Semantic Coherence:</b> \
              In models which are semantically coherent the words which are most probable under a topic should co-occur within the same document. \
            </div>\
            <div class='intro z-depth-1'><b>Exclusivity:</b> \
              Words with high probability under a topic have low probabilities under other topics.\
            </div> \
            <div class='intro'>*Install Chrome extention \
                <a href='https://chrome.google.com/webstore/detail/office-editing-for-docs-s/gbkeegbaiigmenfmjfclcdgdpimamgkj' target='_blank'>\
          Office Editing for Docs, Sheets & Slides</a> for fast opening of docx (right click to open in new tab).\
            </div>\
          <div class='pareto_pic intro'> \
            <img src='graph/sem_exc_pareto_sup.jpg' width='39%'> \
            <img src='graph/sem_exc_pareto_sub.jpg' width='39%'>\
          </div>\
         </div>", file=gui_file, append=TRUE)
    
    topics_header <- "<div class='run_title z-depth-1'>\
                        <a id='topics_header'></a>Model List\
                      </div>\
                      <div class='topics_header' >"
    for (i in sup_range){
      for (j in sub_range){
        model_link <- sprintf("<span class='topic_btn z-depth-1'>\
                                 <a href='#t-%s-%s'>(%s, %s)</a>\
                              </span>", i, j, i, j)
        topics_header <- sprintf("%s%s", topics_header, model_link)
      }
    }
    topics_header <- sprintf("%s%s", topics_header, "</div>")
    write(topics_header, file=gui_file, append=TRUE)
}


##
# The bottom part of the GUI
##
html_bottom <- function(){
  
    write("</div><html>", file=gui_file, append=TRUE)
    browseURL(gui_file)
}
