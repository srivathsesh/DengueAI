# Amelia::missmap(trainImputedSj,y.cex = 0.5,x.cex = 0.5)

## Make a window of data 

trainImputedSj <-  zoo(trainImputedSj, order.by = trainImputedSj$week_start_date)

xtrainingsample1 <- window(x.train,start = '1990-04-30', end = '1996-04-30')
ytrainingsample1 <- window(y.boxcox ,start = '1990-04-30', end = '1996-04-30')
xoneahead <- window(x.train,start = '1990-04-30', end = '1996-05-07')
yoneahead <- window(y.boxcox,start = '1990-04-30', end = '1996-05-07')
dim(trainingsample1)
dim(oneahead)
varnames = c('reanalysis_relative_humidity_percent','reanalysis_precip_amt_kg_per_m2','reanalysis_max_air_temp_k',"ndvi_ne","ndvi_nw","ndvi_se","ndvi_sw",'total_cases')

lags =c(8,8,4,4,4,4,4,1)

mdl1 <- dynfit(xtrainingsample1,ytrainingsample1,varnames,lags)
dyn::fitted.dyn(mdl1)

preds <- predict(mdl1,newdata = newdata)
oneahead <- getnewdata(x.train,y.boxcox,as.Date('1996-04-30'))

pred2 <- predict(mdl1,newdata = oneahead)
