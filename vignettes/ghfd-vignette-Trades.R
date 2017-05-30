## ----example1------------------------------------------------------------
library(GetHFData)

out.file <- system.file("extdata", 'NEG_OPCOES_20151126.zip', package = "GetHFData")
df.tickers <- ghfd_get_available_tickers_from_file(out.file)
print(head(df.tickers)) # show only 10

## ----example2------------------------------------------------------------

my.assets <- df.tickers$tickers[1:3] # ticker to find in zip file

type.matching <- 'exact' # defines how to match assets in dataset
start.time <- '10:00:00' # defines first time period of day
last.time <- '17:00:00'  # defines last time period of day

my.df <- ghfd_read_file(out.file, 
                        type.matching = type.matching,
                        my.assets = my.assets,
                        first.time = '10:00:00',
                        last.time = '17:00:00',
                        type.output = 'raw',
                        agg.diff = '15 min')


## ------------------------------------------------------------------------
head(my.df)

## ------------------------------------------------------------------------
names(my.df)

## ----plot.prices, fig.width=7, fig.height=2.5----------------------------
library(ggplot2)
 
p <- ggplot(my.df, aes(x = TradeDateTime, y = TradePrice, color = InstrumentSymbol))
p <-  p + geom_line()
print(p)

## ----notrun, eval=FALSE--------------------------------------------------
#  library(GetHFData)
#  
#  first.time <- '11:00:00'
#  last.time <- '17:00:00'
#  
#  first.date <- '2015-11-01'
#  last.date <- '2015-11-10'
#  type.output <- 'agg'
#  type.data <- 'trades'
#  agg.diff <- '15 min'
#  
#  # partial matching is available
#  my.assets <- c('PETR','VALE')
#  type.matching <- 'partial'
#  type.market  <- 'equity'
#  
#  df.out <- ghfd_get_HF_data(my.assets =my.assets,
#                             type.matching = type.matching,
#                             type.market = type.market,
#                             type.data = type.data,
#                             first.date = first.date,
#                             last.date = last.date,
#                             first.time = first.time,
#                             last.time = last.time,
#                             type.output = type.output,
#                             agg.diff = agg.diff)
#  

