generatedLaggedPredictors <- function(data,varnames,lags,specificLags = F){
  library(dplyr)
  data %<>% data.frame(data) %>% 
    select(!!varnames)
  for(i in 1:length(varnames)){
    if(!specificLags){
      for(j in 1:lags[i]){
        data <- cbind(data,lag(data[,i],j))
        colnames(data)[ncol(data)] <- paste0(varnames[i],".lag",j)
      } # for(j in 1:lags[i])
    } else {
      data <- cbind(data,lag(data[,i],lags[i]))
      colnames(data)[ncol(data)] <- paste0(varnames[i],".lag",lags[i])
      #data %<>% select(contains("lags"))
    }
    
    
  } # for(i in 1:length(varnames))
  if(!specificLags) return(data) else{
   # browser()
    data %<>% select(contains("lag"))
  }
}

generateDifferencedPredictors <- function(data,varnames,lags){
  library(dplyr)
  data %<>% data.frame(data) %>% 
    select(!!varnames)
  for(i in 1:length(varnames)){
    data <- cbind(data,c(rep(NA,lags[i]),diff(data[,i],lags[i])))
    colnames(data)[ncol(data)] <- paste0(varnames[i],".diff",lags[i])
  }
  return(data)
}