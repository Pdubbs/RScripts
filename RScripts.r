list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
   args <- as.list(match.call())
   args <- args[-c(1:2,length(args))]
   length(value) <- length(args)
   for(i in seq(along=args)) {
     a <- args[[i]]
     if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
   }
   x
} #taken from https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html

sample_split<-function(data,percentile=.2){
  sample_list<-sample(1:nrow(data),nrow(data)*percentile,replace=FALSE)
  train<-data[-sample_list,]
  test<-data[sample_list,]
  return(list(train,test))
}

na_filler<-function(data,ints_as_factors=FALSE,character_fill=""){
  for(col in colnames(data)){
    if(class(data[,col])=="numeric"|(class(data[,col])=="integer"&ints_as_factors==FALSE)){
      data[is.na(data[,col]),col]<-mean(data[!is.na(data[,col]),col])
    } else if(class(data[,col])=="factor"|class(data[,col])=="logical"|(class(data[,col])=="integer"&ints_as_factors==TRUE)){
      data[is.na(data[,col]),col]<-which.max(table(train$blood_type))
    } else if(class(data[,col])=="character"){
      data[is.na(data[,col]),col]<-character_fill
    }
  }
  return(data)
}


