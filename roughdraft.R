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



library(dyn)
set.seed(123)
tz <- zoo(cbind(Y = 0, x = rnorm(10), z = rnorm(10)))

# simulate values
for(i in 2:10) {
  tz$Y[i] <- with(as.data.frame(tz), 
                  2*Y[i-1] + 3*z[i] +4* x[i] + 5*x[i-1] + rnorm(1))
}

# keep copy of tz to compare later to simulated Y's
tz.orig <- tz

# NA out Y's that are to be predicted
tz[7:10, "Y"] <- NA

L <- function(x, k = 1) stats::lag(x, -k)

# predict 1 ahead each iteration
for(i in 7:10) {
  # fit based on first i-1 values
  fit <- dyn$lm(Y ~ L(Y) + z + L(x, 0:1), tz, subset = seq_len(i-1))
  # get prediction for ith value
  tz[i, "Y"] <- tail(predict(fit, tz[1:i,]), 1)
}
cbind(pred = tz[7:10, "Y"], act = tz.orig[7:10, "Y"])

