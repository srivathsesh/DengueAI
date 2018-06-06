linearfit <- function(x,y,varnames,lags,RespLag,specificLags = F){
  LaggedPreds <- generatedLaggedPredictors(x,varnames,lags,specificLags)
  if(RespLag > 0){
    if(!specificLags){
      LaggedResp <- generatedLaggedPredictors(y,colnames(y),RespLag,specificLags = F)[,-1]
    } else {
      LaggedResp <- generatedLaggedPredictors(y,colnames(y),RespLag,specificLags)
    }
    
    data = cbind(LaggedPreds,LaggedResp)
    lm(y ~ ., data = data)
  } else {
    data = cbind(data.frame(y),LaggedPreds)
   eval(parse(text = paste0("lm(",colnames(y),"~.,data = data)")))
  }
  
  
}


