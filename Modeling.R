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
source('~/Documents/MSPA/PREDICT 413/DengAI/generateLaggedPredictors.R')
source('~/Documents/MSPA/PREDICT 413/DengAI/LinearFit.R')
source('~/Documents/MSPA/PREDICT 413/DengAI/RetrievePredictors.R')

#**************************************************
# THE CODE IS A CONTINUATION FROM MARKDOWN
#**************************************************


#-------------------------------------------
# Response variables options 
#--------------------------------------------

# 1. As is 
#----------------------------------------
y <- tsTrainImputedSj$total_cases

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
window(zoo::index(x.train), start = 1, end = )

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

# initial model

LaggedX <- generatedLaggedPredictors(x.train,head(predictors(gbmTune),8),rep(8,8))
intialmdl <- linearfit(x.train,as.numeric(y.boxcox),head(predictors(gbmTune),8),lags = rep(8,8),RespLag = 1)
revisedmdl2 <- linearfit(x.train,y.boxcox,c('reanalysis_relative_humidity_percent','reanalysis_precip_amt_kg_per_m2','reanalysis_max_air_temp_k',"ndvi_ne","ndvi_nw","ndvi_se","ndvi_sw"),lags = c(8,8,4,4,4,4,4),RespLag = 2,specificLags = T)

# Apply model functions to data

start_up %<>% mutate(varnames = rep(list(c('reanalysis_relative_humidity_percent','reanalysis_precip_amt_kg_per_m2','reanalysis_max_air_temp_k',"ndvi_ne","ndvi_nw","ndvi_se","ndvi_sw")),5)) %>% 
  mutate(lags = rep(list(c(8,8,4,4,4,4,4)),5)) %>% 
  mutate(RespLag = 1) %>% 
  mutate(specificLags = T)


ModelFrame <- enframe(rep(model_list,5),name = 'modelname',value = 'model')

ModelResults <- start_up %>% select(-1,-3) %>% 
  `colnames<-`(c('x','y','varnames','lags','RespLag','specificLags')) %>% 
  mutate(params = pmap(.,function(x,y,varnames,lags,RespLag,specificLags){
    list(x = x, y = y, varnames = varnames, lags = lags,RespLag = RespLag, specificLags = specificLags)
  }))
  
ModelResults %<>% bind_cols(.,ModelFrame) %>% 
  mutate(mdlResult = invoke_map(model,params))


LinerFitResults <- sModelResults %>% map(.x = .$mdlResult, .f = ~glance(.x))


#---------------------------------------------------------------
# Make Predictons
# -------------------------------------------------

testing <- start_up %>% select(varnames,lags) %>% unnest() %>% head(7) %>% mutate(currentdate = "1996-04-29") %>% mutate(data = list(tsTrainImputedSj)) %>% mutate(lags = as.integer(lags))

testing %>% pmap(.f = RetrievePredictors) %>% Reduce(cbind,.) %>% colMeans(.,na.rm = T) %>% rbind(.)
