#taken from https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html allows you to return lists from functions
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
} 

#splits a sample into test and train sets, remember to set your seed!
sample_split<-function(data,percentile=.2){
  sample_list<-sample(1:nrow(data),nrow(data)*percentile,replace=FALSE)
  train<-data[-sample_list,]
  test<-data[sample_list,]
  return(list(train,test))
}

#fills in NA values according to the Paul Wyatt rules for doing so
na.fill<-function(data,ints_as_factors=FALSE,character_fill=""){
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

#this function will take in a range of mutually-exclusive binary values and return a factor vector labeled with their column titles
factorize<-function(indata,rowrange){
  fact<-vector("numeric",nrow(indata))
  levels<-NULL
  j<-0
  for(i in rowrange){
    j<-j+1
    fact[base_limited[,i]==1]<-j
    levels<-c(levels,colnames(base_limited)[i])
  }
  fact<-factor(fact,1:j,levels)
  return(fact)
}

#SQL coalesce, shamelessly stolen from http://www.cureffi.org/2013/05/02/r-equivalent-of-sql-coalesce/ Note values need to be ordered
coalesce <- function(...) {
apply(cbind(...), 1, function(x) x[which(!is.na(x))[1]])
}