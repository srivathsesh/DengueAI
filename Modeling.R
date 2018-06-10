# Modeling DengueAI data

# --------------------------------
#   Libraries
#---------------------------------
library(forecast)
library(astsa)
library(dyn)
library(caret)
library(tidyr)
library(caret)
library(purrr)
library(dplyr)
source('generateLaggedPredictors.R')
source('LinearFit.R')
source('RetrievePredictors.R')
source('dynfit.R')

#**************************************************
# THE CODE IS A CONTINUATION FROM MARKDOWN
#**************************************************


#-------------------------------------------
# Response variables options 
#--------------------------------------------

# 1. As is 
#----------------------------------------
y <- tsTrainImputedSj$total_cases # Expecting this to be a zoo object

# 2. Transformed to stabilize variation (BoxCox)
#----------------------------------------
# plot(y) # See the high spikes in 1994

lambda <- forecast::BoxCox.lambda(y)
y.boxcox <- forecast::BoxCox(y,lambda)
plot(y.boxcox)
hist(y.boxcox)

# 3. Differenced y

# acf2(y) # Notice the autocorrelation
y.diff <- diff(y)

# 4. Difference y.boxcoc

acf2(y.boxcox)
y.boxcox.diff <- diff(y.boxcox)
# acf2(y.boxcox.diff) - Looks okay a bit


#----------------------------------------
#     Model Matrix
#---------------------------------------
x.train <- model.matrix(total_cases ~ ., data = tsTrainImputedSj)[,-1]
x.train <- zoo(x.train,order.by = time(y))

#---------------------------------------------------
# Quick check on feature selection from the markdown 
#----------------------------------------------------

# A. Check with non transformed 

# reanalysis_relative_humidity_percent, reanalysis_precip_amt_kg_per_m2, reanalysis_max_air_temp_k, reanalysis_min_air_temp_k, ndvi_ne


# B. Stationarize the above predictors

StationaryX.train <- generateDifferencedPredictors(x.train,c('reanalysis_relative_humidity_percent', 'reanalysis_precip_amt_kg_per_m2', 'reanalysis_max_air_temp_k', 'reanalysis_min_air_temp_k', 'ndvi_ne'),rep(1,5))

StationaryX.train <- data.frame(StationaryX.train) %>% 
  select(contains("diff")) %>% bind_cols(
    data.frame(x.train)
  ) %>% as.matrix.data.frame(.)


varSeq <- seq(1,15,by = 2)
ctrl <- rfeControl(method = "repeatedcv",
                   repeats = 5,
                   verbose = F,
                   functions = rfFuncs,
                   allowParallel = T
                   )
rfRFE.Differenced <- rfe(x = StationaryX.train[-1*which(!complete.cases(StationaryX.train)),],
             y = as.matrix(y.diff[-1*which(!complete.cases(StationaryX.train))]),
             sizes = varSeq,
             metric = "RMSE",
             rfeControl = ctrl,
             ntree = 500)

rfRFE.Differenced <- rfe(x = StationaryX.train[-1*which(!complete.cases(StationaryX.train)),],
                         y = as.matrix(y.boxcox.diff[-1*which(!complete.cases(StationaryX.train))]),
                         sizes = varSeq,
                         metric = "RMSE",
                         rfeControl = ctrl,
                         ntree = 500)
#ntree = 1000)

predictors(rfRFE.Differenced)

#----------------------------------------------------------------------------------
#  Generate rolling window of time 
#----------------------------------------------------------------------------------


inputlist <- data.frame(starts = c(1,52*3, 52*6, 52*9,52*12),
ends = c(52*6, 9*52, 12*52, 52*15,52*18))

windowdraw <- function(starts,ends,data = index(x.train)){
  window(data,start = starts, end = ends)
}

start_up <- pmap(inputlist,windowdraw) %>% tibble(.)
start_up$Pred = pmap(start_up,.f = function(.x) x.train[.x,])
start_up$y <- map(start_up$.,.f = function(.x) y[.x,]) 
start_up$yBoxCox <- map(start_up$.,.f = function(.x) y.boxcox[.x,])


#------------------------------------------------
#                 Bossted Trees
# -----------------------------------------------

library(gbm)

gbmGrid <- expand.grid(interaction.depth = seq(1,7,by = 2),
                       n.trees = seq(100,1000, by = 50),
                       shrinkage = c(0.01,0.1),
                       n.minobsinnode = 20)

set.seed(10)

gbmTune <- train(x.train,as.numeric(y),
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 verbose = F)

plot(varImp(gbmTune))

gbmTuneStationary <- train(StationaryX.train,as.numeric(y.boxcox),
                 method = "gbm",
                 tuneGrid = gbmGrid,
                 verbose = F)

plot(varImp(gbmTuneStationary))


#-----------------------------------------------------------------------------------
#                   Apply Model Functions
#-----------------------------------------------------------------------------------

# What do we have so far...?
# A good idea of candidate predictors...
# [1] "ndvi_ne"                              "ndvi_nw"                             
#[3] "ndvi_se"                              "ndvi_sw"                             
#[5] "reanalysis_max_air_temp_k"            "reanalysis_min_air_temp_k"           
#[7] "reanalysis_precip_amt_kg_per_m2"      "reanalysis_relative_humidity_percent"
#[9] "reanalysis_sat_precip_amt_mm"         "reanalysis_tdtr_k"   
#

predictors(gbmTune)


# Apply model functions to data

#******************************************************************************
#                   Naive regression model
#******************************************************************************

start_up %<>% mutate(varnames = rep(list(
  c(
    'reanalysis_relative_humidity_percent',
    'reanalysis_precip_amt_kg_per_m2',
    'reanalysis_max_air_temp_k',
    "ndvi_ne",
    "ndvi_nw",
    "ndvi_se",
    "ndvi_sw",
    'total_cases'
  )
), 5)) %>%
  mutate(lags = rep(list(c(8, 8, 4, 4, 4, 4, 4, 1)), 5)) 

model_list <- list(Mdl = dynfit)

ModelFrame <- enframe(rep(model_list,5),name = 'modelname',value = 'model')

ModelResults <- start_up %>% select(-1,-3) %>% 
  `colnames<-`(c('x','y','varnames','lags')) %>% 
  mutate(params = pmap(.,function(x,y,varnames,lags){
    list(x = x, y = y, varnames = varnames, lags = lags)
  }))
  
ModelResults %<>% bind_cols(.,ModelFrame) %>% 
  mutate(mdlResult = invoke_map(model,params))


LinerFitResults <- ModelResults %>% map(.x = .$mdlResult, .f = ~glance(.x))

#****************************************************************************
#                         UTILITY FUNCTIONS
#****************************************************************************


#----------------------------------------------------------------------------
#                         Get newdata
#-----------------------------------------------------------------------------


getnewdata <-  function(x,y,currentTime){
  #start <- time(head(x,1))
  end <- currentTime + 1
  newdata = zooreg(cbind(x = x[1:end,], y = y[1:end,]),order.by = time(y[1:end,]))

  return(newdata)
}



# ---------------------------------------------------------------------------
#               One ahead forecast
#----------------------------------------------------------------------------

oneaheadForecast <- function (x,y,currentTime,model,...){
  if('ARIMA' %in% class(model)){
    newdata <- getnewdata4Arima(x,y,currentTime)
  } else {
    newdata <- getnewdata(x,y,currentTime)
  }
  
  # x.newfit <- x[1:currentTime,]
  # y.newfit <- y[1:currentTime]
  # model = dynfit(x.newfit,y.newfit,ModelResults$varnames[[1]], ModelResults$lags[[1]])
  if('dyn' %in% class(model)){
    prediction <- predict(model,newdata)
  } else{
    if('ARIMA' %in% class(model))
      #browser()
      prediction <- predict(model,n.step = 1, newxreg = newdata)
      return(tail(prediction$pred,1))
  }
  
  tail(prediction$mean,1)
}

#----------------------------------------------------------------------------
#                         Make Predictions
#-----------------------------------------------------------------------------

MakePredictions <- function(x, y, currentTime, model, until) {

  # predictions <-window(y, start = '1990-04-30', end = currentTime)
  # predictions <- rbind.zoo(coredata(predictions), rep(NA,ceiling(difftime(strptime(until,"%Y-%m-%d"),strptime(currentTime + 1, "%Y-%m-%d"),units ='weeks'))))
  #predictions <- zoo(predictions,order.by = time(window(x,start = '1990-04-30', end = until)))
  #startindex = which(index(y)==currentTime) + 1
  future = currentTime + 1
  y[future : nrow(y),] <- NA
  while (currentTime < until) {
    oneaheadPred <- oneaheadForecast(x,y, currentTime, model)
    currentTime<- currentTime + 1
    y[currentTime,1] <- oneaheadPred
    
  }
  return(y)
}



#******************************************************************
#              Shoot from the hip linear model
#******************************************************************

statrtTime <- as.Date('1990-04-30')
currentTime <- 312 #as.Date('1996-04-29')


mdl1 <- ModelResults$mdlResult[[1]]
fct <- MakePredictions(x.train,y.boxcox,currentTime = 312,model = mdl1,400)
plot(y.boxcox[313:400])
lines(fct,col = 'red')

mdl2 <- ModelResults$mdlResult[[2]]
head(start_up$Pred[[2]])


currentTime = 468
fct2 <- MakePredictions(x.train,y.boxcox,currentTime = 468,model = mdl2,936)
plot(y.boxcox[468:936])
lines(fct2,col = 'red')

currentTime = 623
mdl3 <- ModelResults$mdlResult[[3]]
fct3 <- MakePredictions(x.train,y.boxcox,currentTime = 623,model = mdl3, 936)


mdl4 <-  ModelResults$mdlResult[[4]]
fct4 <- MakePredictions(x.train,y.boxcox,currentTime = 780,model = mdl4, 936)
plot(y.boxcox[623:936])
lines(fct3,col='blue')
lines(fct4,col = 'red')

# ********************************************************************
#             Choice of lags
#---------------------------------------------------------------------

suggestedLags <- pairwiseAcf(ts(x.train,start = c(1990,18), frequency = 52),ts(y.boxcox,start = c(1990,18), frequency = 52),varnames)

# revamped Start_up

start_up %<>% mutate(varnames2 = rep(list(
  c(
    'reanalysis_relative_humidity_percent',
    'reanalysis_precip_amt_kg_per_m2',
    'reanalysis_max_air_temp_k',
    "ndvi_ne","ndvi_nw",
    "ndvi_se",
    "ndvi_sw",
    "reanalysis_sat_precip_amt_mm",
    "reanalysis_tdtr_k",
    "total_cases"
  )
), 5)) %>%
  mutate(lags2 = rep(list(c(suggestedLags$lags,1)), 5)) 

fullmodel <-
  lm(
    total_cases ~ L(reanalysis_relative_humidity_percent, 9) + L(reanalysis_precip_amt_kg_per_m2, 5) +
      L(reanalysis_max_air_temp_k, 8) + L(ndvi_ne, 8) + L(ndvi_nw, 10) + L(ndvi_se, 10) +
      L(ndvi_sw, 5) + L(reanalysis_sat_precip_amt_mm, 5) + L(reanalysis_tdtr_k, 2) +
      L(total_cases, 1), data = data.frame(cbind(ModelResults$x[[4]],ModelResults$y[[4]]))
      )
library(MASS)
selectionmdl <- stepAIC(fullmodel,direction = "both")
selectionmdl <- dyn(selectionmdl)
#**************************************************************************
#  test out selectionnmdl
#*************************************************************************

selectionmdlfct <- MakePredictions(x.train,y.boxcox,currentTime = 780,model = selectionmdl,936)

plot(y.boxcox[780:936])
lines(selectionmdlfct,col = 'red')
lines(fct4,col ='blue')

# Very similar to shoot from the hip model


#************************************************************************
#    ARIMA with xreg
#************************************************************************

# Naive  out of the  box model

laggedPred <- generatedLaggedPredictors(cbind(x.train,y.boxcox), c(
  'reanalysis_relative_humidity_percent',
  'reanalysis_precip_amt_kg_per_m2',
  'reanalysis_max_air_temp_k',
  "ndvi_ne","ndvi_nw",
  "ndvi_se",
  "ndvi_sw",
  "reanalysis_sat_precip_amt_mm",
  "reanalysis_tdtr_k",
  "total_cases"
),c(suggestedLags$lags,1),
specificLags = T)


laggedPred2 <- generatedLaggedPredictors(cbind(x.train,y.boxcox), c(
#  'reanalysis_relative_humidity_percent',
#  'reanalysis_precip_amt_kg_per_m2',
  'reanalysis_max_air_temp_k',
#  "ndvi_ne","ndvi_nw",
# "ndvi_se",
  # "ndvi_sw",
  # "reanalysis_sat_precip_amt_mm",
  "reanalysis_tdtr_k",
  "total_cases"
),c(suggestedLags$lags[c(-1,-2,-4,-5,-6,-7,-8)],1),
specificLags = T)

AutoArimamdl <- auto.arima(y.boxcox[468:623], xreg = laggedPred[468:623,])

AutoArimamdl2 <- auto.arima(y.boxcox[468:623], xreg = laggedPred2[468:623,])

#**********************************************************************
# get new data for ARIMA
#***********************************************************************
getnewdata4Arima <- function(x,ypred,currentTime) {
  total_cases.lag1 <- L(ypred[1:currentTime],1)
  newdata <- cbind(zooreg(x[currentTime,-ncol(x)]),total_cases.lag1[currentTime,1])
  colnames(newdata)[ncol(newdata)] <- 'total_cases.lag1'
  return(newdata)
}


ArimaFcts <- MakePredictions(laggedPred,y.boxcox,currentTime = 623,model = AutoArimamdl, 936)

plot(y.boxcox[623:936,1])
lines(ArimaFcts,col = 'red')
lines(fct3,col = 'blue')

ArimaFcts2 <- MakePredictions(laggedPred2,y.boxcox,currentTime = 623,model = AutoArimamdl2, 936)

plot(y.boxcox[623:936,1])
# lines(ArimaFcts[623:936],col = 'red')
# lines(fct3,col = 'blue')
lines(ArimaFcts2[623:936],col = 'purple')


#***********************************************************************
#                OUT OF SAMPLE PREDICTIONS
#***********************************************************************

# Here we take the middle of the time series as a benchmark of training set and then predict both the history and future of the training set

plot(y,grid.ticks.on = F)
lines(y[468:623,], col = 'red')
lines(y[624:936,], col = 'blue', lwd = 2)
lines(y[80:467,], col = 'green', lwd = 2)
addLegend(legend.loc = "topright", legend.names = c('Training','Hold out future', 'Holdout past'),col = c('red','blue','green'), lwd = 2)

# 1. Linear models
#****************
# Linear model with  the following as predictors based on Recurrsive Feature Elimination
# 
# reanalysis_relative_humidity_percent',
# 'reanalysis_precip_amt_kg_per_m2',
# 'reanalysis_max_air_temp_k',
# "ndvi_ne",
# "ndvi_nw",
# "ndvi_se",
# "ndvi_sw",
# 'total_cases'


# MEtric MAE.
#************

# PErformance of mdl1 on future hold out

fctHoloutFuture <- MakePredictions(x.train,y.boxcox,currentTime = 623,model = mdl1,936)

plot(y.boxcox[623:936],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by linear model 1 on hold out set")
lines(fctHoloutFuture,col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','LinearModel1 prediction'),col = c('black','red'),lty = 1)

fctHoloutFuture.Actuals <- InvBoxCox(fctHoloutFuture,lambda)

MAE.mdl1 <- mean(abs(fctHoloutFuture.Actuals[624:936] - y[624:936,]))

# PErformance of mdl12 on future hold out
fct2HoloutFuture <- MakePredictions(x.train,y.boxcox,currentTime = 623,model = mdl2,936)

plot(y.boxcox[623:936],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by linear model 2 on hold out set")
lines(fct2HoloutFuture,col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','LinearModel2 prediction'),col = c('black','red'),lty = 1)

fct2HoloutFuture.Actuals <- InvBoxCox(fct2HoloutFuture,lambda)

MAE.mdl2 <- mean(abs(fct2HoloutFuture.Actuals[624:936] - y[624:936,]))

# PErformance of mdl3  on future hold out
fct3HoloutFuture <- MakePredictions(x.train,y.boxcox,currentTime = 623,model = mdl3,936)

plot(y.boxcox[623:936],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by linear model 3 on hold out set")
lines(fct3HoloutFuture,col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','LinearModel3 prediction'),col = c('black','red'),lty = 1)

fct3HoloutFuture.Actuals <- InvBoxCox(fct3HoloutFuture,lambda)

MAE.mdl3 <- mean(abs(fct3HoloutFuture.Actuals[624:936] - y[624:936,]))




# Performance of Linear model based on stepwise feature selection 
#****************************************************************

# Features selected based on RFE and CCF. 

suggestedLags

# The below features and their lags were included in the models for a stepwise search

#                               predictor lags correlation
# 1 reanalysis_relative_humidity_percent    9  0.30837942
# 2      reanalysis_precip_amt_kg_per_m2    5  0.19337887
# 3            reanalysis_max_air_temp_k    8  0.42293961
# 4                              ndvi_ne    8  0.07305284
# 5                              ndvi_nw   10  0.14804305
# 6                              ndvi_se   10  0.06995947
# 7                              ndvi_sw    5  0.02764198
# 8         reanalysis_sat_precip_amt_mm    5  0.13448162
# 9                    reanalysis_tdtr_k    2  0.13876079


# The resultant model was 

summary(selectionmdl)

# The predictors were narrowed to reanalysis_max_air_temp_k with lag of 8 (2 months),  reanalysis_tdtr_k with lag of 2 and lag 1 of the response 

selectionmdlfct <- MakePredictions(x.train,y.boxcox,currentTime = 623,model = selectionmdl,936)

plot(y.boxcox[623:936],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by reduced Linear model on hold out set")
lines(selectionmdlfct,col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','Reduced Linear prediction'),col = c('black','red'),lty = 1)

stepwiseActuals <- InvBoxCox(selectionmdlfct,lambda)

MAE.StepWise <- mean(abs(stepwiseActuals[624:936] - y[624:936,]))


#************************************************************************
#              Arima model performance
#************************************************************************

ArimaFcts <- MakePredictions(laggedPred,y.boxcox,currentTime = 623,model = AutoArimamdl, 936)

plot(y.boxcox[623:936,1],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by Arima model(larger predictor pool) on hold out set")
lines(ArimaFcts,col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','Arima - larger predictors pool'),col = c('black','red'),lty = 1)

ArimaFctActuals <- InvBoxCox(ArimaFcts,lambda)
MAE.Arima1 <- mean(abs(ArimaFctActuals[624:936] - y[624:936,]))




ArimaFcts2 <- MakePredictions(laggedPred2,y.boxcox,currentTime = 623,model = AutoArimamdl2, 936)


plot(y.boxcox[623:936,1],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by Arima model(smaller predictor pool) on hold out set")
# lines(ArimaFcts[623:936],col = 'red')
# lines(fct3,col = 'blue')
lines(ArimaFcts2[623:936],col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','Arima - smaller predictors pool'),col = c('black','red'),lty = 1)

ArimaFct2Actuals <- InvBoxCox(ArimaFcts2,lambda)
MAE.Arima2 <- mean(abs(ArimaFct2Actuals[624:936] - y[624:936,]))

#****************************************************************************
#                         Knn forecasts
#****************************************************************************

plot(knnTune, main = "Choice of K for KNN" )

fctknnTuned <- predict(refitknn,newdata = data.frame(laggedPred2[624:936,]))

plot(y.boxcox[623:936,1],grid.ticks.on = F, main = "BoxCox Transformed total cases forecast by KNN on hold out set")
lines(xts(fctknnTuned,order.by = time(y.boxcox[624:936,1])),col = 'red')
addLegend(legend.loc = "topleft", legend.names = c('BoxCox Total_Cases','KNN forecast'),col = c('black','red'),lty = 1)

fctknnTuned.Actuals <- forecast::InvBoxCox(fctknnTuned,lambda)
MAE.KNN <- mean(abs(fctknnTuned.Actuals - y[624:936,]))


# Summary sofar

ModelCategory <- c(rep("Linear Model", 4) , rep("ARIMA",2), rep("KNN",2))
Modelnames <- c("mdl1", "mdl2", "mdl3", "stepwise", "ARIMA1", "ARIMA2", "KNN -naive lags", "KNN")
Description <- c(rep("Predictors based on rfRFE & lasso",3),"Predictors - Stepwise selection", "Predictors based on rfRFE"," predictors by stepwise & CCF", "Lagged response - ccf", "Predictors - Stepwise selection & CCFs based lags")
MAES <- c(MAE.mdl1[[1]],MAE.mdl2[[1]],MAE.mdl3[[1]],MAE.StepWise[[1]], MAE.Arima1[[1]], MAE.Arima2[[1]],MAE.KnnNaive[[1]],MAE.KNN[[1]])

summaryMAE <- data.frame(ModelCategory ,Modelnames,
                         Description,MAES)

