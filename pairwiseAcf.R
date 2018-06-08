#****************************************************************
#                    Optimal lags decision
#****************************************************************

pairwiseAcf <- function(x, y, varnames) {
  bestlag <- function(p) {
    #browser()
    cf.df <- Pacf(data.frame(y, x[, p]), plot = T)
    data.frame(
      predictor = p,
      lags = which.max(abs(cf.df[["acf"]][, 1, 2])),
      correlation = abs(cf.df[["acf"]][, 1, 2])[which.max(abs(cf.df[["acf"]][, 1, 2]))]
    )
  }
  varnames %>% map_df(bestlag)
  
}


varnames <- c('reanalysis_relative_humidity_percent',
              'reanalysis_precip_amt_kg_per_m2',
              'reanalysis_max_air_temp_k',
              "ndvi_ne","ndvi_nw",
              "ndvi_se",
              "ndvi_sw")

lagstouse <- pairwiseAcf(x.train,y,varnames)

Ccf(tsTotalcasesSj,ts(trainImputedSj$reanalysis_relative_humidity_percent,start = c(1990,18), frequency = 52))