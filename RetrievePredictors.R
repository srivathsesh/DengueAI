RetrievePredictors <- function(data = tsTrainImputedSj,currentdate,lags,varnames){
  browser()
  if(!is.Date(currentdate)){
    currentdate <- lubridate::ymd(currentdate)
  }
  data[currentdate - (lags+1)*7,varnames] %>% `colnames<-`(paste0(varnames,".lag",lags))
  
}

