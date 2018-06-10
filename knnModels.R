#*****************************************************************
#                 K - Nearest Neighbor Forecasting
#*****************************************************************


# naive method using lags

knnforecasting <- function(k){
  library(tsfknn)
  
  trainingSet  <- ModelResults$y[[3]]
  predKnn <- knn_forecasting(ts(trainingSet$total_cases),h = 300, k = k, msas = 'recursive', lags = 1:6)
  predictionknn <- predictionknn <- matrix(predKnn$prediction,ncol = 1)
  predictionknn <- xts(predictionknn,order.by = time(y.boxcox[625:924,]))
  # plot(y.boxcox[624:924,1])
  # lines(predictionknn,col = 'red')
  
  MAE.Knn.Naive <- mean(abs(y[624:924,] - InvBoxCox(predictionknn,lambda)))
  data.frame(k = k, MAE.Knn.Naive)
  
}

MAE.KnnNaive <- 2:10 %>% purrr::map_df(knnforecasting) %>% summarise(MAE = min(MAE.Knn.Naive))


# knnn regression for forecasting


  knnTune <- train(data.frame(laggedPred2[468:623,]),
                  as.numeric(coredata(y.boxcox)[468:623,]),
                   method = 'knn',
                   preProc = c("center","scale"),
                   tuneGrid = data.frame(.k = 2:10)
                   )

# oneSE(knnTune$results,'RMSE',maximize = F, 10) was 7
  
  refitknn <- train(data.frame(laggedPred2[468:623,]),
                             as.numeric(coredata(y.boxcox)[468:623,]),
                             method = 'knn',
                             preProc = c("center","scale"),
                             tuneGrid = data.frame(.k = 2:10),
                             selectionFunction = "oneSE"
  )
  
  
fctknnTuned <- predict(refitknn,newdata = data.frame(laggedPred2[624:936,]))

fctknnTuned <- forecast::InvBoxCox(fctknnTuned,lambda)
actuals <- forecast::InvBoxCox( y.boxcox[624:936,],lambda) 


fctknnTunedHist <- predict(refitknn, newdata = data.frame(laggedPred2[80:467,]))
plot(y.boxcox[80:467,])
lines(xts(fctknnTunedHist,order.by = time(y.boxcox[80:467,])),col = 'red')

actualHist <- y[80:467,]
fctknnTunedHist.Actuals <- forecast::InvBoxCox(fctknnTunedHist,lambda)

plot(actualHist)
lines(xts(fctknnTunedHist.Actuals,order.by = time(actualHist)), col = 'red')

mean(abs(actualHist - fctknnTunedHist.Actuals))
