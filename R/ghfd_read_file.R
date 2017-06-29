#' Reads zip file downloaded from Bovespa ftp (trades or orders)
#'
#' @param out.file Name of zip file
#' @inheritParams ghfd_get_HF_data
#'
#' @return A dataframe with the raw (tick by tick/order by order) dataset
#' @export
#'
#' @examples
#'
#' my.assets <- c('ABEVA20', 'PETRL78')
#'
#' ## getting data from local file (in practice it would be downloaded from ftp)
#' out.file <- system.file("extdata", 'NEG_OPCOES_20151126.zip', package = "GetHFData")
#'
#' df.out <- ghfd_read_file(out.file, my.assets)
#' print(head(df.out))
ghfd_read_file <- function(out.file,
                           my.assets = NULL,
                           type.matching = 'exact',
                           type.data = 'trades',
                           first.time = '10:00:00',
                           last.time = '17:00:00',
                           type.output = 'agg',
                           agg.diff = '15 min'){

  # check my.assets

  if (!is.null(my.assets)){
    my.assets <- as.character(my.assets)

    if (class(my.assets)!='character'){
      stop('The input my.assets should have class equal to character')
    }
  }

  if (type.data == 'trades') {
    t.out <- ghfd_read_file.trades(out.file,
                                   my.assets = my.assets,
                                   type.matching = type.matching,
                                   first.time = first.time,
                                   last.time = last.time,
                                   type.output = type.output,
                                   agg.diff = agg.diff)

  } else if (type.data == 'orders') {


    t.out <- ghfd_read_file.orders(out.file,
                                   my.assets = my.assets,
                                   type.matching = type.matching,
                                   first.time = first.time,
                                   last.time = last.time,
                                   type.output = type.output,
                                   agg.diff = agg.diff)

  }


  if (type.output == 'agg'){
    cat(paste('\n   -> Aggregation resulted in dataframe with',nrow(t.out), 'rows'))
  }

  return(t.out)
}

#' Reads zip file downloaded from Bovespa ftp (trades) - INTERNAL USE
#'
#' @inheritParams ghfd_read_file
#'
#' @return A dataframe with trade data (aggregated or raw)
#'
#' @examples
#'
#' # no example
ghfd_read_file.trades <- function(out.file,
                                  my.assets = NULL,
                                  type.matching = NULL,
                                  first.time = '10:00:00',
                                  last.time = '17:00:00',
                                  type.output = 'agg',
                                  agg.diff = '15 min') {

  if (is.null(type.matching)) type.matching <- 'exact'

    # import data
  col.names <- c('SessionDate','InstrumentSymbol','TradeNumber', 'TradePrice', 'TradedQuantity','Tradetime',
                 'TradeIndicator', 'BuyOrderDate', 'SequentialBuyOrderNumber','SecondaryOrderID',
                 'AggressorBuyOrderIndicator','SellOrderDate','SequentialSellOrderNumber','SecondaryOrderID2',
                 'AggressorSellOrderIndicator','CrossTradeIndicator', 'BuyMember','SellMember')


  my.col.types <- readr::cols(
    SessionDate = readr::col_date(format = ""),
    InstrumentSymbol = readr::col_character(),
    TradeNumber = readr::col_character(),
    TradePrice = readr::col_character(),
    TradedQuantity = readr::col_character(),
    Tradetime = readr::col_character(),
    TradeIndicator = readr::col_integer(),
    BuyOrderDate = readr::col_date(format = ""),
    SequentialBuyOrderNumber = readr::col_character(),
    SecondaryOrderID = readr::col_character(),
    AggressorBuyOrderIndicator = readr::col_integer(),
    SellOrderDate = readr::col_date(format = ""),
    SequentialSellOrderNumber = readr::col_character(),
    SecondaryOrderID2 = readr::col_character(),
    AggressorSellOrderIndicator = readr::col_integer(),
    CrossTradeIndicator = readr::col_integer(),
    BuyMember = readr::col_character(),
    SellMember = readr::col_character()
  )
  # fix for "no visible binding" NOTE from CHECK
  #utils::suppressForeignCheck(col.names)   # DOESNT WORK
  # Unfortunately this seems to be the easiest way to keep CHECK quiet about the variables

  InstrumentSymbol <- TradeIndicator <- SessionDate <- TradePrice <-  NULL
  TradedQuantity <- Tradetime <-  AggressorBuyOrderIndicator <- TradeDateTime <- NULL
  last.price <- TradeSign <- BuyMember <- CrossTradeIndicator <- SellMember <- NULL

  # read data (warning are from last line)
  suppressWarnings(suppressMessages(
    my.df <- readr::read_csv2(file = out.file,
                              skip = 1,
                              col_names = col.names,
                              progress = F,
                              col_types = my.col.types)
  ) )

  if(nrow(my.df)<10){
    return(data.frame())
  }

  # remove cancelled trades
  my.df <- dplyr::filter(my.df,
                         TradeIndicator == 1)


  cat(paste(' - Imported ', nrow(my.df), 'lines,',length(unique(my.df$InstrumentSymbol)),'unique tickers'))
  cat(paste('\n   -> Processing file'))

  # filter raw data for assets and cancelled trades

  # if no my.assets are suppied, import everything

  if (is.null(my.assets)){
    my.assets <- unique(my.df$InstrumentSymbol)
  }

  if (type.matching == 'exact') {
    # exact matching
    idx <- my.df$InstrumentSymbol %in% my.assets

  } else if (type.matching == 'partial'){
    # partial match my.assets
    my.pattern <- paste0('(',paste0(my.assets, collapse = '|'),')')
    idx <- grepl(x = my.df$InstrumentSymbol, pattern = my.pattern)
  }

  my.df <- my.df[idx, ]

  # if no data is returned, output empty dataframe
  if (nrow(my.df)==0){
    cat(' - Found no data for my.assets.')
    return(data.frame())
  }

  # keep only columns with interesting information
  my.df <- dplyr::select(my.df,
                         SessionDate,
                         InstrumentSymbol,
                         TradePrice,
                         TradedQuantity,
                         Tradetime,
                         AggressorBuyOrderIndicator,
                         CrossTradeIndicator,
                         BuyMember,
                         SellMember)

  # Set objet classes correct
  my.df$TradePrice <- as.numeric(my.df$TradePrice)
  my.df$TradedQuantity <- as.numeric(my.df$TradedQuantity)
  my.df$SessionDate <- as.Date(my.df$SessionDate)
  my.df$TradeDateTime <- lubridate::ymd_hms(paste(my.df$SessionDate,my.df$Tradetime),
                                            tz = "America/Sao_Paulo")

  my.df$BuyMember <- as.numeric(my.df$BuyMember)
  my.df$SellMember <- as.numeric(my.df$SellMember)

  # calculate trading signs
  my.df$TradeSign <-   (my.df$AggressorBuyOrderIndicator ==1)*1 - (my.df$AggressorBuyOrderIndicator ==2)*1
  my.df$AggressorBuyOrderIndicator <- NULL

  cat(paste0(' - Found ', nrow(my.df), ' lines for ',
             length(unique(my.df$InstrumentSymbol)),' selected tickers'))


  # remove day times outside period from first.time and last.time

  unique.dates <- unique(my.df$SessionDate)

  if (!any(is.null(c(first.time, last.time)))){
    first.time <-
      as.POSIXct(paste0(unique.dates[1], ' ', first.time, ' BRT'))
    last.time <-
      as.POSIXct(paste0(unique.dates[1], ' ', last.time, ' BRT'))


    #browser()
    idx <- ( my.df$TradeDateTime >= first.time)&( my.df$TradeDateTime <= last.time)

    my.df <- my.df[idx, ]
  }


  if (type.output == 'raw') {
    return(my.df)

  } else {
    # Aggregate the data

    my.seq <- seq(from = first.time, to = last.time, by = agg.diff)

    my.breaks <- paste(as.character(unique.dates),
                       rep(format(my.seq, '%H:%M:%S'),
                           length(unique.dates)),
                       'BRT')

    my.grid <-
      expand.grid(format(my.seq, '%H:%M:%S'), as.character(unique.dates))

    my.breaks <-
      as.POSIXct(paste(my.grid[[2]], my.grid[[1]], 'BRT'))

    my.df$TradeDateTime <- cut(my.df$TradeDateTime, breaks = my.breaks)

    # dplyr magic!

    my.G <- dplyr::group_by(my.df, InstrumentSymbol, SessionDate, TradeDateTime)

    t.out <- dplyr::summarise(my.G,
                              n.trades = length(TradePrice),
                              last.price     = TradePrice[length(TradePrice)],
                              weighted.price = sum(TradePrice*(TradedQuantity*TradePrice)/sum(TradedQuantity*TradePrice)),
                              period.ret     = last.price/TradePrice[1] -1,
                              period.ret.volat = stats::sd(TradePrice[2:length(TradePrice)]/TradePrice[1:length(TradePrice)-1] -1, na.rm = T),
                              sum.qtd        = sum(TradedQuantity),
                              sum.vol        = sum(TradedQuantity*TradePrice),
                              n.buys         = sum(TradeSign == 1),
                              n.sells        = sum(TradeSign == -1))

    t.out$Tradetime <- format(as.POSIXct(t.out$TradeDateTime), '%H:%M:%S')

    t.out$TradeDateTime <- as.POSIXct(t.out$TradeDateTime, tz = 'America/Sao_Paulo')

    # remove NA (trades outside of time interval)
    t.out <- stats::na.omit(t.out)

    return(t.out)

  }

}

#' Reads zip file downloaded from Bovespa ftp (orders) - INTERNAL USE
#'
#' @inheritParams ghfd_read_file
#'
#' @return A dataframe with trade data (aggregated or raw)
#'
#' @examples
#'
#' # no example
ghfd_read_file.orders <- function(out.file,
                                  my.assets = NULL,
                                  type.matching = NULL,
                                  first.time = '10:00:00',
                                  last.time = '17:00:00',
                                  type.output = 'agg',
                                  agg.diff = '15 min') {

  if (is.null(type.matching)) type.matching <- 'exact'

  # set column meanings (ftp://ftp.bmf.com.br/MarketData/OFER_CPA_LAYOUT_english.txt)
  tab.order.side <- data.frame(x.in = (1:2),
                               x.out = c('Buy','Sell'))

  tab.execution.type <- data.frame(x.in = c(1:9, 11,12),
                                   x.out = c('New','Update','Cancel','Trade','Reentry',
                                     'New Stop Price', 'Rejected', 'Removed',
                                     'Stop Price Triggered', 'Expired', 'Eliminated'))

  tab.order.status <- data.frame(x.in = c('0','1','2','4','5','8','C'),
                                  x.out = c('New','Partially Filled','Filled','Canceled','Replaced',
                                            'Rejected','Expired'))

  tab.aggressor.indicator <- data.frame(x.in = 0:2,
                                        x.out = c('Neutral', 'Aggressor', 'Passive'))

  # import data
  col.names <- c('SessionDate','InstrumentSymbol','OrderSide', 'SequentialOrderNumber', 'SecondaryOrderID','ExecutionType',
                 'PriorityTime', 'PriorityIndicator', 'OrderPrice','TotalQuantity', 'TradedQuantity','OrderDate',
                 'OrderDatetime','OrderStatus','AgressorIndicator','Member')


  my.col.types <- readr::cols(
    SessionDate = readr::col_date(format = ""),
    InstrumentSymbol = readr::col_character(),
    OrderSide = readr::col_integer(),
    SequentialOrderNumber = readr::col_double(),
    SecondaryOrderID = readr::col_integer(),
    ExecutionType = readr::col_integer(),
    PriorityTime = readr::col_guess(),
    PriorityIndicator = readr::col_character(),
    OrderPrice = readr::col_character(),
    TotalQuantity = readr::col_double(),
    TradedQuantity = readr::col_double(),
    OrderDate = readr::col_skip(),
    OrderDatetime = readr::col_datetime(format = ""),
    OrderStatus = readr::col_character(),
    AgressorIndicator = readr::col_integer(),
    Member = readr::col_integer()
  )

  # fix for "no visible binding" NOTE from CHECK
  #utils::suppressForeignCheck(col.names)   # DOESNT WORK
  # Unfortunately this seems to be the easiest way to keep CHECK quiet about the variables

  SessionDate  <- InstrumentSymbol <- OrderSide <- NULL
  SequentialOrderNumber <- SecondaryOrderID <- ExecutionType <- NULL
  PriorityTime <- PriorityIndicator <- OrderPrice <- TotalQuantity <- NULL
  TradedQuantity <- OrderDate <- OrderDatetime <- OrderStatus <- AgressorIndicator  <- Member <- NULL

  # read data (warning are from last line)
  suppressWarnings(suppressMessages(
    my.df <- readr::read_csv2(file = out.file,
                              skip = 1,
                              col_names = col.names,
                              progress = F,
                              col_types = my.col.types,
                              locale = readr::locale(tz =  'America/Sao_Paulo', decimal_mark = '.'))

  ))

  # replace col values for intuitive factors
  idx <- match(my.df$OrderSide, tab.order.side$x.in)
  my.df$OrderSide <- as.factor(tab.order.side$x.out[idx])

  idx <- match(my.df$ExecutionType, tab.execution.type$x.in)
  my.df$ExecutionType <- as.factor(tab.execution.type$x.out[idx])

  idx <- match(my.df$OrderStatus, tab.order.status$x.in)
  my.df$OrderStatus <- as.factor(tab.order.status$x.out[idx])

  if(nrow(my.df)<10){
    return(data.frame())
  }

  cat(paste(' - Imported ', nrow(my.df), 'lines,',length(unique(my.df$InstrumentSymbol)),'unique tickers'))
  cat(paste('\n   -> Processing file'))

  # filter for NA data
  my.df <-stats::na.omit(my.df)

  # if no my.assets are suppied, import everything

  if (is.null(my.assets)){
    my.assets <- unique(my.df$InstrumentSymbol)
  }

  if (type.matching == 'exact') {
    # exact matching
    idx <- my.df$InstrumentSymbol %in% my.assets

  } else if (type.matching == 'partial'){
    # partial match my.assets
    my.pattern <- paste0('(',paste0(my.assets, collapse = '|'),')')
    idx <- grepl(x = my.df$InstrumentSymbol, pattern = my.pattern)
  }

  my.df <- my.df[idx, ]

  # if no data is returned, output empty dataframe
  if (nrow(my.df)==0){
    cat(' - Found no data for my.assets.')
    return(data.frame())
  }

  # Set objet classes correct
  my.df$OrderPrice <- as.numeric(my.df$OrderPrice)
  my.df$PriorityDateTime <- lubridate::ymd_hms(paste(my.df$SessionDate,my.df$PriorityTime),
                                            tz = "America/Sao_Paulo")

  cat(paste0(' - Found ', nrow(my.df), ' lines for ',
             length(unique(my.df$InstrumentSymbol)),' selected tickers'))


  # remove day times outside period from first.time and last.time

  unique.dates <- unique(my.df$SessionDate)

  first.time <-
    as.POSIXct(paste0(unique.dates[1], ' ', first.time, ' BRT'))
  last.time <-
    as.POSIXct(paste0(unique.dates[1], ' ', last.time, ' BRT'))

  idx <- ( my.df$OrderDatetime >= first.time)&( my.df$OrderDatetime <= last.time)

  my.df <- my.df[idx, ]

  if (type.output == 'raw') {
    return(my.df)

  } else {
    # Aggregate the data

    my.seq <- seq(from = first.time, to = last.time, by = agg.diff)

    my.breaks <- paste(as.character(unique.dates),
                       rep(format(my.seq, '%H:%M:%S'),
                           length(unique.dates)),
                       'BRT')

    my.grid <-
      expand.grid(format(my.seq, '%H:%M:%S'), as.character(unique.dates))

    my.breaks <-
      as.POSIXct(paste(my.grid[[2]], my.grid[[1]], 'BRT'))

    my.df$OrderDatetime <- cut(my.df$OrderDatetime, breaks = my.breaks)

    # dplyr magic!

    my.G <- dplyr::group_by(my.df, InstrumentSymbol, SessionDate, OrderDatetime)

    names(my.df)
    t.out <- dplyr::summarise(my.G,
                              n.order = length(OrderPrice),
                              avg.order.price     = mean(OrderPrice),
                              max.order.price = max(OrderPrice),
                              min.order.price = min(OrderPrice),
                              type.order = OrderSide[1],
                              n.new.order = sum(ExecutionType == 'New'),
                              n.update.order = sum(ExecutionType == 'Update'),
                              n.cancelled.order = sum(ExecutionType == 'Cancel'))

    # remove NA (trades outside of time interval)
    t.out <- stats::na.omit(t.out)

    return(t.out)

  }

}
