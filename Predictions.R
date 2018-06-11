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
#          5. Create Model matrices
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


