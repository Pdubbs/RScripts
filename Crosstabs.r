library(descr)
col.table <- function(var1, var2, weights=rep(1,length(var1)), margins=TRUE){
  # Creating table of (weighted) relative frequencies by column, and adding row variable margins as the last column
  crosstab <- prop.table(xtabs(weights ~ var1 + var2), margin=2)
  t <- cbind(crosstab, Total=prop.table(xtabs(weights ~ var1)))
  # Adding column sums in the last row
  t <- rbind(t,Total = colSums(t))
  # Naming rows and columns of the table after var1 and var2 used, and returning result
  names(dimnames(t)) <- c(deparse(substitute(var1)), deparse(substitute(var2)))
  return(round(100*t,2))
}

row.table <- function(var1, var2, weights=rep(1,length(var1)), margins=TRUE){
  t <- rbind(prop.table(xtabs(weights ~ var1 + var2), margin=1),
             Total=prop.table(xtabs(weights ~ var2)))
  t <- cbind(t,Total = rowSums(t))
  names(dimnames(t)) <- c(deparse(substitute(var1)), deparse(substitute(var2)))
  return(round(100*t,2))
}