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

