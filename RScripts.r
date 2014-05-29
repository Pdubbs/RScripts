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