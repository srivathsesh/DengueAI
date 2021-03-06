# This file processes the test set and makes predictions for each of the model.


library(magrittr)
library(tidyverse)
library(skimr)
library(forecast)
library(lubridate)
library(xts)
library(astsa)
library(caret)
library(dplyr)

#**************************************************************************
#                     Model objects will require .RData from this project
#                       TO SPEED UP PROCESS
#***************************************************************************

# 1. Read test file in
test <- read_csv("DengAI_Predicting_Disease_Spread_-_Test_Data_Features.csv")

# 2 Break file into cities

testSJ <- test %>% dplyr::filter(city == "sj")
testIQ <- test %>% dplyr::filter(city == "iq")

#*******************************************************
#             Imputation
#*******************************************************

# 2. Fill in missing values (should not affect prediciton for most part)
Amelia::missmap(testSJ)
Amelia::missmap(testIQ)

imputedtestSJ <- VIM::kNN(testSJ)
imputedtestIQ <- VIM::kNN(testIQ)

plot(imputedtestSJ$reanalysis_tdtr_k~imputedtestSJ$week_start_date, 
     col = ifelse(imputedtestSJ$reanalysis_tdtr_k_imp == T,"red", "black"),type = "b")

plot(imputedtestSJ$reanalysis_max_air_temp_k~imputedtestSJ$week_start_date, 
     col = ifelse(imputedtestSJ$reanalysis_max_air_temp_k == T,"red", "black"),type = "b")

plot(imputedtestIQ$reanalysis_tdtr_k~imputedtestIQ$week_start_date, 
     col = ifelse(imputedtestIQ$reanalysis_tdtr_k_imp == T,"red", "black"),type = "b")

plot(imputedtestIQ$reanalysis_min_air_temp_k~imputedtestIQ$week_start_date, 
     col = ifelse(imputedtestIQ$reanalysis_min_air_temp_k == T,"red", "black"),type = "b")

#************************************************************
#             3. Make tottal_cases column with NA
#************************************************************

imputedtestSJ %<>% mutate(total_cases = NA)
imputedtestIQ %<>% mutate(total_cases = NA)

#***********************************************************
#            4. Make test sets identical to the training
#**********************************************************

tsTestImputedSj <- imputedtestSJ %>%
  select(-city, -year, -weekofyear, -week_start_date) %>%
  select(-one_of(!!HighCorSj)) %>%
  select(-ends_with("imp"))

tsTestImputedSj <- xts(tsTestImputedSj, order.by = imputedtestSJ$week_start_date, frequency = 52)


tsTestImputedIq <- imputedtestIQ %>% 
  select(-city,-year,-weekofyear,-week_start_date) %>% 
  select(-one_of(!!HighCorIq)) %>% 
  select(-ends_with("imp"))

tsTestImputedIq <- xts(tsTestImputedIq,order.by = imputedtestIQ$week_start_date, frequency = 52)


#*******************************************************************
#                     5. Create Model matrices
#*******************************************************************

# Are xts and zoo classes not sure if there will be an issue
x.test.sj <- tsTestImputedSj[,-ncol(tsTestImputedSj)]
x.test.iq <- tsTestImputedIq[,-ncol(tsTestImputedIq)]

# y matrix of NA with time stamp

y.boxcox.test.sj <- tsTestImputedSj$total_cases
y.boxcox.test.iq <- tsTestImputedIq$total_cases

#******************************************************************************
#   6. bind training sets and test sets to create a single time series object
#*******************************************************************************
x.pred.sj <- xts::rbind.xts(x.train,x.test.sj)
x.pred.iq <- xts::rbind.xts(x.iq.train,x.test.iq)

y.pred.sj <- xts::rbind.xts(y.boxcox,y.boxcox.test.sj)
y.pred.iq <- xts::rbind.xts(y.iq.boxcox,y.boxcox.test.iq)


#******************************************************************************
#                        7. Make submission file
#******************************************************************************
Makefile <- function(sjpreds,iqpreds,filename){
  submissionfile <- read.csv('submission_format.csv', header = T)
  # Make variables
  city <- c(rep('sj',260),rep('iq',156))
  year <- c(year(time(sjpreds)),year(time(iqpreds)))
  #weekofyear <- c(isoweek(time(sjpreds)),isoweek(time(iqpreds)))
  weekofyear <- submissionfile$weekofyear
  total_cases <- c(round(sjpreds$total_cases),round(iqpreds$total_cases))
  
  # Make dataframe to write to file
  df <- data.frame(city,year,weekofyear,total_cases)
  write.csv(df,file = filename,row.names = F)
}


#*******************************************************************************
#                       8. PREDICTIONS
#*******************************************************************************

# index 936 is the ending of training set , after which there should be NAs for response variable
# Current time is set to be 936 & until 1196 for san juan
# Current time is set to 442 & until 598 for iq

# 1. Stepwise model - SJ

sj.stepwise <- MakePredictions(x.pred.sj,y.pred.sj,currentTime = 936,model = selectionmdl,until = 1196 )
sj.stepwise <- InvBoxCox(sj.stepwise$total_cases,lambda)
sj.stepwise.pred <- sj.stepwise[937:1196,]


# 2. Boosting model - IQ
iq.boosting <- predict(gbmTune.iq, newdata = x.pred.iq[443:598,])
iq.boosting.pred <- xts(iq.boosting,order.by = time(x.pred.iq[443:598,]))
colnames(iq.boosting.pred) <- "total_cases"

Makefile(sj.stepwise.pred,iq.boosting.pred,"stepwiseBoosting.csv")

#****************************************************************
#                    Ensemble Predictions - SJ
#*****************************************************************

laggedPredTestsj <- generatedLaggedPredictors(cbind(x.pred.sj,y.pred.sj), c(
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

# Arima predictions

sj.Arima2 <- MakePredictions(laggedPredTestsj,y.pred.sj,currentTime = 936,model = AutoArimamdl2,until = 1196)
sj.Arima2fcts <- InvBoxCox(sj.Arima2,lambda)
sj.Arima2fcts <- sj.Arima2fcts[937:1196]

# knn predictions

knntestPreds <- MakePredictions(laggedPredTestsj,y.pred.sj,currentTime = 936,model = refitknn,1196)
knntestPredsActuals <- InvBoxCox(knntestPreds,lambda)
knntestPredsActuals <- knntestPreds[937:1196]
# Linear regression retrained model

sj.linear <- MakePredictions(x.pred.sj,y.pred.sj, currentTime = 936, model = finalmdl, until = 1196)
sj.linear <- InvBoxCox(sj.linear,lambda)
sj.linearFcts <- sj.linear[937:1196]

# naive knn predictions

# retain Naive Knn
knnNaivePred <- knn_forecasting(timeS = ts(y.pred.sj[625:936]),h = 260, k = 2, msas = 'recursive', lags = 1:6 )
KnnNaiveFcts <- xts(InvBoxCox(knnNaivePred$prediction,lambda), order.by = time(y.pred.sj[937:1196,]))



SJPredMatrix <- xts::cbind.xts(sj.linearFcts,sj.Arima2fcts,KnnNaiveFcts,knntestPredsActuals)
Wts <- summaryMAE$MAES[c(4,6,7,8)]
EnsemblePred <- EnsemblePrediction(SJPredMatrix, Wts)
EnsemblePredxts <- xts(EnsemblePred, order.by = time(y.pred.sj[937:1196,]))

#****************************************************************
#                    Ensemble Predictions - IQ
#*****************************************************************

# retrained Linear model predictions
linearPred.iq <- MakePredictions(x.pred.iq,y.pred.iq,currentTime = 442, model = linearRetrain.iq, until = 598)
linearPredFctActual.iq <- InvBoxCox(linearPred.iq,lambda.iq)

# Arima prediction
ArimaPred.iq <- MakePredictions(x.pred.iq,y.pred.iq,currentTime = 442, model = autoarima.iq, until = 598, bypass = T)
ArimaPreffctActual <- InvBoxCox(ArimaPred.iq,lambda.iq)

ArimaIQXregFct <- auto.arima(y.iq[157:442,], xreg = x.iq.train[157:442,colnames(x.iq.train) %in% varnames.iq],lambda = lambda.iq)
ArimaIQXregFctActuals <- forecast(ArimaIQXregFct, h = 156, xreg = x.pred.iq[443:598,colnames(x.iq.train) %in% varnames.iq],lambda = lambda.iq )
ArimaIQXregFctActuals <- xts(ArimaIQXregFctActuals$mean, order.by = time(y.pred.iq[443:598,]))
colnames(ArimaIQXregFctActuals) <- "total_cases"

laggedPredTest.iq <- generatedLaggedPredictors(cbind(x.pred.iq,y.pred.iq), c(
  suggestedLags.iq$predictor,
  
  "total_cases"
),c(suggestedLags.iq$lags,1),
specificLags = T)

knnpredtest.iq <- MakePredictions(laggedPredTest.iq,y.pred.iq,currentTime = 442,model = knnTune.iq,598)
knnpredfctActual <- InvBoxCox(knnpredtest.iq, lambda.iq)

IQPredMatrix <- xts::cbind.xts(linearPredFctActual.iq[443:598,],iq.boosting.pred,ArimaIQXregFctActuals,knnpredfctActual[443:598,])


EnsemblePred.iq <- EnsemblePrediction(IQPredMatrix,summaryMAE.iq$MAES.iq)
EnsemblePred.iqxts <- xts(EnsemblePred.iq, order.by = time(y.pred.iq[443:598,]))

colnames(EnsemblePred.iqxts) <- "total_cases"
colnames(EnsemblePredxts) <- "total_cases"

Makefile(EnsemblePredxts,EnsemblePred.iqxts,'ensemblefct.csv')


