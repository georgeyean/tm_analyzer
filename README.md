## The tm_analyzer extends the "nested topic model" from:

Blaydes, Lisa; Grimmer, Justin; McQueen, Alison. 2017. "Mirrors for Princes and Sultans:Advice on the Art of Governance in the Medieval Christian and Islamic Worlds". Journal of Politics
https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CV9AYE  

The tm_analyzer provides a visualized web GUI-based analytical tool for one or a list of models passed in, with features including:   

1. Search for the optimal model based on Semantic Coherence and Exclusivity metrics.  

1. The web GUI for quanlitative analysis by associating documents and sub-documents with the nested topics.  

1. Output latex format of topics.  


## Usage: 

1. in replication.R, simply modify as you wish:

modify your sup and sub range
```javascript
sup_range <- c(3:5)
sub_range <- c(48:55)
```

run parallel jobs
```javascript
models <- parallel_worker_batch(process_tdm, param_list, corenum=8)
```

lauch analyzer for models
```javascript
tm_analyzer(models, sup_range, sub_range, un_docs, book_info[, 2], tdm8, sup_tdm8)
```

 <img src="https://github.com/georgeyean/tm_analyzer/blob/main/images/log.png" width="500">






