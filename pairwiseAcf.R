#****************************************************************
#                    Optimal lags decision
#****************************************************************

pairwiseAcf <- function(x, y, varnames) {
  bestlag <- function(p) {
    # browser()
    cf.df <- forecast::Ccf(y[,1], x[, p], plot = T,lag.max = 10)
    data.frame(
      predictor = p,
      lags = which.max(abs(cf.df[["acf"]][11:21, 1, 1]))-1,
      correlation = abs(cf.df[["acf"]][11:21, 1, 1])[which.max(abs(cf.df[["acf"]][11:21, 1, 1]))]
    )
  }
  varnames %>% map_df(bestlag)
  
}


varnames <- c('reanalysis_relative_humidity_percent',
              'reanalysis_precip_amt_kg_per_m2',
              'reanalysis_max_air_temp_k',
              "ndvi_ne","ndvi_nw",
              "ndvi_se",
              "ndvi_sw",
              "reanalysis_sat_precip_amt_mm",
              "reanalysis_tdtr_k")

lagstouse <- pairwiseAcf(ts(x.train,start = c(1990,18), frequency = 52),ts(y,start = c(1990,18), frequency = 52),varnames)

#lagCcf(tsTotalcasesSj,ts(trainImputedSj$reanalysis_relative_humidity_percent,start = c(1990,18), frequency = 52))

lagstouse <- pairwiseAcf(ts(x.train.diff,start = c(1990,19), frequency = 52),ts(y.boxcox.diff,start = c(1990,18), frequency = 52),varnames)

