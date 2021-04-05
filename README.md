## The tm_analyzer

extends the "nested topic model" from:


> Blaydes, Lisa; Grimmer, Justin; McQueen, Alison. 2017. "Mirrors for Princes and Sultans:Advice on the Art of Governance in the Medieval Christian and Islamic Worlds". Journal of Politics
> https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CV9AYE  

<br/><br/>

The tm_analyzer provides a visualized web GUI-based analytical tool for one or a list of models passed in, with features including:   

1. Parallel computing for mutiple model runs and bootstrap runs.

1. Search for the optimal model based on Semantic Coherence and Exclusivity metrics.  

1. The web GUI for quanlitative analysis by associating documents and sub-documents with the nested topics.  

1. Output latex format of topics.  


## Usage: 

Environment:
> I have been using Mac for most of the time, but it should be compatible with Linux.
> For Windows, you can try the code, and fix platform-related errors accordingly. It should be fairly straightforward.

<br/>
<br/>

Modify replication.R accordingly for your needs. Secifically, look at these places as below.

You can wrap the core machine learning logic within a fuction to be called by parallelworker, like this:
```javascript 
process_tdm <-function(is, js, sup_docs, sub_docs){
 
      pw_log("starting (%d, %d) %s", is, js, getwd())
  
      ##Replicating the model run
      set.seed(3991260)
      pw_log(format(Sys.time(), "%a %b %d %X %Y"))
      g_60_c_4<- exp.agenda.vonmon(normed, authors, is, js, verbose=T, kappa= 1000)
      pw_log("Finishing (%d, %d)", is, js)
      pw_log(format(Sys.time(), "%a %b %d %X %Y"))
          
      return(g_60_c_4)
} 
```
<br/>
Modify your sup and sub range
```javascript
sup_range <- c(3:5)
sub_range <- c(48:55)
```

run parallel jobs
```javascript
models <- parallel_worker_batch(process_tdm, param_list, corenum=8)
```
<br/>

Lauch analyzer for returned models<br/>

```javascript
tm_analyzer(models, sup_range, sub_range, un_docs, book_info[, 2], tdm8, sup_tdm8)
```
<br/>
<br/>
When parallelworker starts, all logs for each process will be redirected to a socket console:<br/><br/>

 <img src="https://github.com/georgeyean/tm_analyzer/blob/main/images/log.png" width="600">
 
<br/>
<br/>
Here's the final GUI:<br/><br/>

 <img src="https://github.com/georgeyean/tm_analyzer/blob/main/images/log.png" width="600">







