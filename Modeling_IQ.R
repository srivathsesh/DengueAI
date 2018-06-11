# Modeling for Iq

#*************************************************************
#      This is a continuation of the code from markdown
#*************************************************************

# What has already been done
#---------------------------

# 1. Data imputed
# 2. RFE using random forest by eleminating the year 2001 - similar

# [1] "reanalysis_specific_humidity_g_per_kg" "reanalysis_precip_amt_kg_per_m2"      
# [3] "station_max_temp_c"                    "reanalysis_relative_humidity_percent" 
# [5] "reanalysis_air_temp_k"                 "reanalysis_tdtr_k"                    
# [7] "station_avg_temp_c"                   

# reanalysis_tdr_k is correlated with reanalysis_relative_humidity_percent. Needing to be mindful of that

# ------------------------------------------------------------
# Try remove the effect of time from total_cases for rfRFE
#------------------------------------------------------------

auto.arima(trainImputedIq$total_cases)

arimaIq <- sarima(trainImputedIq$total_cases, p =1,d =0,q = 4)


rfRFEiq_RemoveTimeEffect <- rfe(x = trainImputedIq[,c(-1:-3,-21)],
               y = arimaIq$fit$residuals,
               sizes = varSeq,
               metric = "RMSE",
               rfeControl = ctrl,
               ntree = 500)
# [1] "reanalysis_tdtr_k"                     "reanalysis_relative_humidity_percent"  "reanalysis_air_temp_k"                
# [4] "ndvi_sw"                               "reanalysis_max_air_temp_k"             "station_diur_temp_rng_c"              
# [7] "reanalysis_precip_amt_kg_per_m2"       "reanalysis_specific_humidity_g_per_kg" "station_avg_temp_c"                   
# [10] "ndvi_nw"                               "station_max_temp_c"                   


#--------------------------------------------------------------
# lasso methods
#-------------------------------------------------------------

x.iq <- model.matrix(total_cases ~ ., data = trainImputedIq[,-1:-3])[,-1]
cv.out <- glmnet::cv.glmnet(x.iq,y = trainImputedIq$total_cases, family = "poisson", alpha = 1)
plot(cv.out)
out <- glmnet::glmnet(x.iq,trainImputedIq$total_cases, family = 'poisson',lambda = cv.out$lambda.min, alpha = 1)
topPredLasso_PresentTime.Iq <- abs(predict(out,type = "coefficients",s=cv.out$lambda.min))

topPredL_presentTime.Iq <- data.frame(Predictors = topPredLasso_PresentTime.Iq@Dimnames[[1]][topPredLasso_PresentTime.Iq@i+1],
                                   Coefs = topPredLasso_PresentTime.Iq@x) 

topPredL_presentTime.Iq %<>% arrange(desc(Coefs)) %>% filter(!Predictors == "(Intercept)")

#                             Predictors      Coefs
# 1                               ndvi_sw 0.09934793
# 2             reanalysis_min_air_temp_k 0.06572561
# 3 reanalysis_specific_humidity_g_per_kg 0.06123807
# 4                 reanalysis_air_temp_k 0.02212834
# 5                    station_min_temp_c 0.01993035


cv.out <- glmnet::cv.glmnet(x.iq,y = arimaIq$fit$residuals, alpha = 1)
plot(cv.out)
out <- glmnet::glmnet(x.iq,arimaIq$fit$residuals,lambda = cv.out$lambda.min, alpha = 1)
topPredLasso.Iq <- abs(predict(out,type = "coefficients",s=cv.out$lambda.min))

topPredL.Iq <- data.frame(Predictors = topPredLasso.Iq@Dimnames[[1]][topPredLasso.Iq@i+1],
                                      Coefs = topPredLasso.Iq@x) 

topPredL.Iq %<>% arrange(desc(Coefs)) %>% filter(!Predictors == "(Intercept)")

#   Predictors     Coefs
#1 (Intercept) 0.3178234
#2     ndvi_sw 1.2480178


#***************************************************************************
#             Transform y
#***************************************************************************

y.iq <- tsTrainImputedIq$total_cases
lambda.iq <- forecast::BoxCox.lambda(y.iq)
y.iq.boxcox <- forecast::BoxCox(y.iq,lambda.iq)

#        Check stationarity
#-----------------------------------------

# 3. Differenced y

# acf2(y.iq) # Notice the autocorrelation
y.iq.diff <- diff(y.iq)

# 4. Difference y.boxcoc

acf2(y.iq.boxcox)
y.iq.boxcox.diff <- diff(y.iq.boxcox)
# acf2(y.iq.boxcox.diff) - Looks okay a bit


#----------------------------------------
#     Model Matrix
#---------------------------------------
x.iq.train <- model.matrix(total_cases ~ ., data = tsTrainImputedIq)[,-1]
x.iq.train <- zoo(x.iq.train,order.by = time(y.iq))


#-----------------------------------------
# Reconciliation of top predictors
#----------------------------------------

# Top 6 predictors from each feature selection methods were taken, common ones were included in the list accounting for 
# between predictor correlations

# The final list

varnames.iq <- c("reanalysis_tdtr_k",
                 "reanalysis_specific_humidity_g_per_kg",
                 "ndvi_sw",
                 "reanalysis_precip_amt_kg_per_m2",
                 "reanalysis_air_temp_k" ,
                 "reanalysis_min_air_temp_k")


#*************************************************************************************
# When the predictors and response variables are differenced, what predictors jump out?
#*************************************************************************************


StationaryX.iq.train <- generateDifferencedPredictors(x.iq.train,varnames.iq,rep(1,6))

StationaryX.iq.train <- data.frame(StationaryX.iq.train) %>% 
  select(contains("diff")) %>% bind_cols(
    data.frame(x.iq.train)
  ) %>% as.matrix.data.frame(.)


varSeq <- seq(1,15,by = 2)
ctrl <- rfeControl(method = "repeatedcv",
                   repeats = 5,
                   verbose = F,
                   functions = rfFuncs,
                   allowParallel = T
)
rfRFE.iq.Differenced <- rfe(x = StationaryX.iq.train[-1*which(!complete.cases(StationaryX.iq.train)),],
                         y = as.matrix(y.iq.diff[-1*which(!complete.cases(StationaryX.iq.train))]),
                         sizes = varSeq,
                         metric = "RMSE",
                         rfeControl = ctrl,
                         ntree = 500)

rfRFE.iq.boxDifferenced <- rfe(x = StationaryX.iq.train[-1*which(!complete.cases(StationaryX.iq.train)),],
                         y = as.matrix(y.iq.boxcox.diff[-1*which(!complete.cases(StationaryX.iq.train))]),
                         sizes = varSeq,
                         metric = "RMSE",
                         rfeControl = ctrl,
                         ntree = 500)




#------------------------------------------------
#                 Bossted Trees
# -----------------------------------------------

library(gbm)

gbmGrid <- expand.grid(interaction.depth = seq(1,7,by = 2),
                       n.trees = seq(100,1000, by = 50),
                       shrinkage = c(0.01,0.1),
                       n.minobsinnode = 20)

set.seed(10)

gbmTune.iq <- train(x.iq.train,as.numeric(y.iq),
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 verbose = F)

plot(varImp(gbmTune.iq))

gbmTuneStationary.iq <- train(StationaryX.iq.train,as.numeric(y.iq.boxcox),
                           method = "gbm",
                           tuneGrid = gbmGrid,
                           verbose = F)

plot(varImp(gbmTuneStationary.iq))

# Our variable selection has converged  for most part.


#**************************************************************************
# Window data for training and OOS testing
#**************************************************************************

inputlist.iq <- data.frame(starts = c(1,157,313),
                        ends = c(156,312,442))

windowdraw2 <- function(starts,ends,data = index(x.iq.train)){
  window(data,start = starts, end = ends)
}

start_up.iq <- pmap(inputlist.iq,windowdraw2) %>% tibble(.)
start_up.iq$Pred = pmap(start_up.iq,.f = function(.x) x.iq.train[.x,])
start_up.iq$y <- map(start_up.iq$.,.f = function(.x) y.iq[.x,]) 
start_up.iq$yBoxCox <- map(start_up.iq$.,.f = function(.x) y.iq.boxcox[.x,])


suggestedLags.iq <- pairwiseAcf(ts(x.iq.train,start = c(2002,1), frequency = 52),ts(y.boxcox,start = c(2002,1), frequency = 52),varnames.iq)

#                                 predictor lags correlation
# 1                     reanalysis_tdtr_k   10   0.1979843
# 2 reanalysis_specific_humidity_g_per_kg    0   0.2702861
# 3                               ndvi_sw   10   0.1936320
# 4       reanalysis_precip_amt_kg_per_m2   10   0.1173220
# 5                 reanalysis_air_temp_k    5   0.2242304
# 6             reanalysis_min_air_temp_k    0   0.3236602


# *****************************************************************************
#                  Linear Regression Model
#******************************************************************************
fullmodel.iq <-
  lm(
    total_cases ~ L(reanalysis_tdtr_k, 10) + reanalysis_specific_humidity_g_per_kg +
      L(ndvi_sw, 10) + L(reanalysis_precip_amt_kg_per_m2, 10) + L(reanalysis_air_temp_k, 5) +
      reanalysis_min_air_temp_k +L(total_cases, 1), data = data.frame(start_up.iq$Pred[[2]],start_up.iq$yBoxCox[[2]])
  )

library(MASS)

selectionmdl.iq <- stepAIC(fullmodel.iq,direction = "both")

detach("package:MASS", unload = T)

library(dplyr)

selectionmdlfct.iq <- MakePredictions(x.iq.train,y.iq.boxcox,currentTime = 312,model = selectionmdl,442)

plot(y.iq.boxcox[313:442,])
lines(selectionmdlfct.iq[313:442], col = 'red')

stepwiseActuals.iq <- InvBoxCox(selectionmdlfct.iq,lambda.iq)
MAE.StepWise.iq <- mean(abs(stepwiseActuals.iq[313:442] - y.iq[313:442,]))

#********************************************************************************
# How does our boosting model perform?
#******************************************************************************
boostingPred <- predict(gbmTune.iq,newdata = x.iq.train[313:442,])
boostingPred <- xts(boostingPred,order.by = time(y.iq[313:442,]))
plot(y.iq[313:442,])
lines(boostingPred, col = 'red')

MAE.Bossting.iq <- mean(abs(boostingPred - y.iq[313:442,]))

#*****************************************************************************
#                    Arima
#***************************************************************************
autoarima.iq <- auto.arima(y.iq.boxcox[157:312,], stepwise = F)
autoarimafct.iq <- forecast(autoarima.iq,h=130)
autoarimafct.iq <- xts(InvBoxCox(autoarimafct.iq$mean,lambda.iq),order.by = time(y.iq.boxcox[313:442,]))

plot(y.iq.boxcox[313:442,])
lines(autoarimafct.iq, col = 'red')
MAE.AutoArima <- MAE(coredata(autoarimafct.iq),y.iq[313:442,])

autoarima.iqXreg <- auto.arima(y.iq[157:312,], xreg = x.iq.train[157:312,colnames(x.iq.train) %in% varnames.iq],lambda = lambda.iq)
autoarimafct.iqXreg <- forecast(autoarima.iqXreg, h = 130, xreg = x.iq.train[313:442,colnames(x.iq.train) %in% varnames.iq],lambda.iq )
plot(autoarimafct.iqXreg)
MAE.AutoArimaXreg <- accuracy(autoarimafct.iqXreg,y.iq[313:442,])[2, 3]

#*****************************************************************************
#                   KNN
#*****************************************************************************


laggedPred.iq <- generatedLaggedPredictors(cbind(x.iq.train,y.iq.boxcox), c(
  suggestedLags.iq$predictor,
  
  "total_cases"
),c(suggestedLags.iq$lags,1),
specificLags = T)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     selectionFunction = "oneSE")

knnTune.iq <- train(data.frame(laggedPred.iq[157:312,]),
                 as.numeric(coredata(y.iq.boxcox)[157:312,]),
                 method = 'knn',
                 preProc = c("center","scale"),
                 tuneGrid = data.frame(.k = 2:10),
                 trControl = ctrl
                 
)

knnPred.iq <- MakePredictions(laggedPred.iq,y.iq.boxcox,currentTime = 312,model = knnTune.iq,442)
plot(y.iq.boxcox [313:442,])
lines(xts(knnPred.iq, order.by = time(y.iq.boxcox))[313:442],col = 'red')

MAE.KNN.iq <- MAE(InvBoxCox(knnPred.iq,lambda.iq)[313:442], y.iq[313:442,])


#************************************************************************
# Ensemble method
#**********************************************************************

EnsemblePrediction <- function(forecasts,MAES){
  Wts <- matrix(sum(MAES^-1/sum(MAES^-1)),nrow = length(MAES))
  forecasts %*% Wts
  
}


# Retrain Linear model
linearRetrain.iq <- lm(formula = total_cases ~ L(reanalysis_tdtr_k, 10) + L(total_cases, 
                                                                            1), data = data.frame(x.iq.train[156:442,],y.iq.boxcox[156:442]))

linearRetrain.iq <- dyn(linearRetrain.iq)
