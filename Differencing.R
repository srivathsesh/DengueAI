# Stainarize dependant and independant variables

# Dependant variable 

autoplot(tsTrainImputedSj$total_cases)
# Box cox transformation to stabilize variation

lambda <-  BoxCox.lambda(tsTrainImputedSj$total_cases)
total_cases_Trans <-  BoxCox(tsTrainImputedSj$total_cases,lambda)
autoplot(total_cases_Trans)

acf2(total_cases_Trans)

acf2(diff(total_cases_Trans))
hist(diff(total_cases_Trans))
 y <- diff(total_cases_Trans)

 # Independant variables
 
# reanalysis_relative_humidity_percent, , , station_avg_temp_c, 
 
 autoplot(tsTrainImputedSj$reanalysis_relative_humidity_percent)
 # lambda = BoxCox.lambda(tsTrainImputedSj$reanalysis_relative_humidity_percent)
 # humidityPercTrans <- BoxCox(tsTrainImputedSj$reanalysis_relative_humidity_percent,lambda)
 autoplot(humidityPercTrans)
 seasonplot(ts(trainImputedSj$reanalysis_relative_humidity_percent, start = c(as.Date('1990-04-30'),52)))
 
 plot(diff(tsTrainImputedSj$reanalysis_relative_humidity_percent))
 acf2(tsTrainImputedSj$reanalysis_relative_humidity_percent)
acf2(diff(tsTrainImputedSj$reanalysis_relative_humidity_percent,1)) 
# So single differencing and potentially a single lag can be useful

# ---------------------------------------------------------------
# reanalysis_precip_amt_kg_per_m2
# ---------------------------------------------------------------

autoplot(tsTrainImputedSj$reanalysis_precip_amt_kg_per_m2)
tsTrainImputedSj$reanalysis_precip_amt_kg_per_m2[which(tsTrainImputedSj$reanalysis_precip_amt_kg_per_m2 > 400)] <- 275
acf2(diff(tsTrainImputedSj$reanalysis_precip_amt_kg_per_m2))


# ---------------------------------------------------------------
# reanalysis_min_air_temp_k
# ---------------------------------------------------------------

autoplot(tsTrainImputedSj$reanalysis_min_air_temp_k)
ggseasonplot(ts(trainImputedSj$reanalysis_min_air_temp_k, start = c(1990,18), frequency = 52)) +  theme_classic() + theme(legend.position = "none",plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size = 5)) + ggtitle("Reanalysis_min_air_temp_K")

acf2(diff(tsTrainImputedSj$reanalysis_min_air_temp_k))
# So single differencing and potentially a single lag can be useful

# ---------------------------------------------------------------
# reanalysis_max_air_temp_k
# ---------------------------------------------------------------

# likewise as above (may not need this as it is correlated with Min temp)

acf2(diff(tsTrainImputedSj$reanalysis_tdtr_k,1))


