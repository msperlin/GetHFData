## ----notrun, eval=FALSE-------------------------------------------------------
#  library(GetHFData)
#  
#  first.time <- '10:00:00'
#  last.time <- '17:00:00'
#  
#  first.date <- '2016-08-18'
#  last.date <- '2016-08-18'
#  
#  type.output <- 'raw' # aggregates data
#  
#  my.assets <- 'PETR4F'
#  type.matching <- 'exact'
#  type.market = 'equity-odds'
#  type.data <- 'orders' # order data
#  
#  df.out <- ghfd_get_HF_data(my.assets =my.assets,
#                             type.data= type.data,
#                             type.matching = type.matching,
#                             type.market = type.market,
#                             first.date = first.date,
#                             last.date = last.date,
#                             first.time = first.time,
#                             last.time = last.time,
#                             type.output = type.output)
#  
#  df.lob <- ghfd_build_lob(df.out)
#  
#  

