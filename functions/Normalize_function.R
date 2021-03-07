scale_function <- function(df1, df2, datatype){
  if (datatype=="train") {
    for (i in 1:length((df1))){
      if (sapply(df1[,i], is.numeric) == T){
        xmin = min(df1[i])
        xmax = max(df1[i])
        df1[i] <- (df1[i]-xmin)/(xmax - xmin)
      }
    }
  }
  if (datatype == "test"){
    for (i in names(df2)){
      if(sapply(df1[,i], is.numeric) == T){
        xmin = min(df1[i])
        xmax = max(df1[i])
        df2[i] <- (df2[i]-xmin)/(xmax - xmin)
      }
    }
    df1 <-  df2
  }
  return(df1)
}