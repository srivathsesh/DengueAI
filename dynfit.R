
L <- function(var,lags = 1) {
  browser()
  stats::lag(var,k=-lags)
}

dynfit <- function(x,y,varnames,lags){
  browser()
  list2env(data.frame(cbind(x,y)),.GlobalEnv)
  # lmdl <- eval(parse(text = paste0('lm(y ~',paste0('stats::lag(',varnames,',',-lags,')',collapse = '+'),',data = zooreg(cbind(x,y)))')))
  
  lmdl <- eval(parse(text = paste0('dyn$lm(total_cases ~',paste0('stats::lag(',varnames,',',-lags,')',collapse = '+'),')')))
  # data = zoo(cbin
  
 lmdl
}



