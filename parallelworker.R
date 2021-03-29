#do this first: install.packages("doParallel") 
#Note: for Mac users, in order to save images (call graphics) in parallel processes on Mac, you need X11 support
#run: capabilities() on R terminal, to see if X11 is TRUE
#if FASE, download and install the latest R package (need isntall XQuatz too, check https://cran.r-project.org/bin/macosx/) 
#install.packages("processx")
library(foreach)
library(doParallel)
library(processx)
if (!require(snowfall))
  install.packages("snowfall")
library(snowfall)
if (!require(unix))
  install.packages("unix") 
library(unix)

#rlimit_stack(600000) #increase stack mem, no need for now, as it returns address

port <- as.integer(runif(1, min=1000, max=9999))
system(sprintf('osascript -e \'tell app "Terminal" to do script "nc -l %d"\'', port))
Sys.sleep(2) #wait 2s for socket up
log.socket <<- make.socket(port=port)




paralWorkerBatch<- function(func=func, params_list=params_list, corenum=corenum){
  
  #on Quad-core Mac, cores=4 issues 12 workers; core=8 issue 24 workers
  registerDoParallel(cores = ifelse(missing(corenum), detectCores(), corenum)) 
  
  system.time({
      r <- foreach(i=1:len(params_list)) %dopar% {
          params = params_list[[i]]
          re <- do.call(func, params)
          return(re)
      }
      # sfInit(parallel=TRUE, cpus=8)
      # result <- sfLapply(params_list, func)
      # sfStop()
  })
  stopImplicitCluster()
  print(paste("return size of r: ", object.size(r)))
  print("current limit (to increase: e.g., rlimit_stack(60000000)  :")
  print(rlimit_all())
  return(r)
}




#write log on terminal (e.g., mac/linux).
Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
  cat(msg)
  write.socket(log.socket, msg)
}