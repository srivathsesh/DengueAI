
L <- function(var,lags = 1) {
  dplyr::lag(var,k=lags)
}

dynfit <- function(x,y,varnames,lags){
  # list2env(data.frame(cbind(x,y)),.GlobalEnv)
  # lmdl <- eval(parse(text = paste0('lm(y ~',paste0('stats::lag(',varnames,',',-lags,')',collapse = '+'),',data = zooreg(cbind(x,y)))')))
  sample <- data.frame(cbind(x,y))
  lmdl <- eval(parse(text = paste0('dyn$lm(total_cases ~',paste0('L(',varnames,',',lags,')',collapse = '+'),',data = sample)')))
  # data = zoo(cbin
  
 lmdl
}


