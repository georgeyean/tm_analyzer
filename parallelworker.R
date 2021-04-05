#' paralworker wraps parallel computing and display realtime logs in console.
#' It redirects logs of all processes to a socket terminal
#' 
#' Note: for Mac users, in order to save images (call graphics) in parallel processes on Mac, you need X11 support
#' run: 
#'    capabilities() on R terminal, to see if X11 is TRUE
#' if FALSE, download and install the latest R package (need isntall XQuatz too, check https://cran.r-project.org/bin/macosx/) 
#' 
library(foreach)
library(doParallel)
library(processx)
library(snowfall)
library(unix)

#rlimit_stack(600000) #increase stack mem, no need for now, as it returns address
port <- as.integer(runif(1, min=1000, max=9999))
system(sprintf('osascript -e \'tell app "Terminal" to do script "nc -l %d"\'', port))
Sys.sleep(2) #wait 2s for socket to come up
log.socket <<- make.socket(port=port)


#' @title paral_worker_batch
#' @description
#' batch process jobs passed in.
#' @param func function to run
#' @param params parameters of the function
#' @param corenum numbers of cores. On Quad-core Mac, cores=4 issues 12 workers; core=8 issue 24 workers
parallel_worker_batch <- function (func=func, params_list=params_list, corenum=4){
  
  registerDoParallel(corenum) 
  
  system.time({
      res <- foreach(i=1:len(params_list)) %dopar% {
          params = params_list[[i]]
          re <- do.call(func, params)
          return(re)
      }
      # sfInit(parallel=TRUE, cpus=8)
      # result <- sfLapply(params_list, func)
      # sfStop()
  })
  
  stopImplicitCluster()
  print(paste("return size of r: ", object.size(res)))
  print("current limit (to increase: e.g., rlimit_stack(60000000)  :")
  print(rlimit_all())
  return(res)
}

#' @title Log
#' @description
#' write log onto socket terminal.
#' @param text text to print on console
pw_log <- function(text, ...) {
  msg <- sprintf(paste0("(pid: ", as.character(Sys.getpid()), ") ", 
                        as.character(Sys.time()), ": ", text, "\n"), ...)
  cat(msg)
  write.socket(log.socket, msg)
}