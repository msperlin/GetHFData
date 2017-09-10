#' Organizes LOB (internal function)
#'
#' This internal recursive function organizes the lob by making sure that all prices and time are ordered.
#' Every time that prices in the bid and ask matches, it will create a trade and modify the lob accordingly.
#'
#' @param my.lob A LOB (order book)
#' @inheritParams ghfd_build_lob
#'
#' @return An organized LOB
#'
#' @examples
#'
#' # no examples (internal)
organize.lob <- function(my.lob, silent = TRUE) {

  if (is.na(my.lob$last.update)){
    return(my.lob)
  }

  # order by price and time
  idx.ask <- order(my.lob$ask.price, my.lob$ask.time)
  my.lob$ask.price <- my.lob$ask.price[idx.ask]
  my.lob$ask.vol <- my.lob$ask.vol[idx.ask]
  my.lob$ask.time <- my.lob$ask.time[idx.ask]

  idx.bid <- order(my.lob$bid.price, my.lob$bid.time, decreasing = TRUE)
  my.lob$bid.price <- my.lob$bid.price[idx.bid]
  my.lob$bid.vol <- my.lob$bid.vol[idx.bid]
  my.lob$bid.time <- my.lob$ask.time[idx.bid]

  # fix for empty lob
  if (length(my.lob$bid.price) ==0 ) return(my.lob)
  if (length(my.lob$ask.price) ==0 ) return(my.lob)

  if (any(is.na(c(my.lob$bid.price[1], my.lob$ask.price[1])))) return(my.lob)
  #rowser()

  # check trades in top of lob
  if (my.lob$bid.price[1] >= my.lob$ask.price[1]){

    if (!silent) cat('\tFound trades!')

    diff.vol <- my.lob$bid.vol[1] -  my.lob$ask.vol[1]

    if (diff.vol < 0 ){
      my.lob$ask.vol[1] = abs(diff.vol)

      my.lob$bid.price <- my.lob$bid.price[-1]
      my.lob$bid.vol <- my.lob$bid.vol[-1]
      my.lob$bid.time <- my.lob$bid.time[-1]
      my.lob$bid.id <- my.lob$bid.id[-1]


    } else if (diff.vol > 0) {
      my.lob$bid.vol[1] = abs(diff.vol)

      my.lob$ask.price <- my.lob$ask.price[-1]
      my.lob$ask.vol <- my.lob$ask.vol[-1]
      my.lob$ask.time <- my.lob$ask.time[-1]
      my.lob$ask.id <- my.lob$ask.id[-1]
    } else if (diff.vol ==0){

      my.lob$bid.price <- my.lob$bid.price[-1]
      my.lob$bid.vol <- my.lob$bid.vol[-1]
      my.lob$bid.time <- my.lob$bid.time[-1]
      my.lob$bid.id <- my.lob$bid.id[-1]

      my.lob$ask.price <- my.lob$ask.price[-1]
      my.lob$ask.vol <- my.lob$ask.vol[-1]
      my.lob$ask.time <- my.lob$ask.time[-1]
      my.lob$ask.id <- my.lob$ask.id[-1]

    }

    #browser()
    my.lob <- organize.lob(my.lob)
  }

  #print.lob(my.lob)

  return(my.lob)
}

#' Adds an order to the LOB
#'
#' @inheritParams organize.lob
#' @param order.in An order from the data
#'
#' @return An LOB with the new order
#'
#' @examples
#' # no example (internal)
add.order <- function(my.lob, order.in, silent = TRUE) {

  my.lob$last.update <- order.in$time

  if (!silent) cat('\t', order.in$type.order)

  # new order
  if (order.in$type.order == 'New' ) {

    if (order.in$side == 'Buy') {
      my.lob$bid.price <- c(my.lob$bid.price, order.in$price)
      my.lob$bid.vol <-   c(my.lob$bid.vol  , order.in$vol)
      my.lob$bid.id <-    c(my.lob$bid.id   , order.in$id)
      my.lob$bid.time <-  c(my.lob$bid.time , order.in$time)
    }

    if (order.in$side == 'Sell') {
      my.lob$ask.price <- c(my.lob$ask.price, order.in$price)
      my.lob$ask.vol <-   c(my.lob$ask.vol  , order.in$vol)
      my.lob$ask.id <-    c(my.lob$ask.id   , order.in$id)
      my.lob$ask.time <-  c(my.lob$ask.time , order.in$time)

    }
  }

  # cancel order
  if (order.in$type.order == 'Cancel' ) {

    if (order.in$side == 'Buy') {
      idx <- which(my.lob$bid.id == order.in$id)
      #browser()
      if (length(idx) == 0){
        if (!silent) cat('\tCant match id for cancel order..')
        return(my.lob)
      }

      my.lob$bid.price <-  my.lob$bid.price[-idx]
      my.lob$bid.vol <-  my.lob$bid.vol[-idx]
      my.lob$bid.id <-  my.lob$bid.id[-idx]
      my.lob$bid.time <- my.lob$bid.time[-idx]
    }

    if (order.in$side == 'Sell') {
      idx <- which(my.lob$ask.id == order.in$id)

      if (length(idx) == 0){
        if (!silent) cat('\tCant match id for cancel order..')
        return(my.lob)
      }

      my.lob$ask.price <-  my.lob$ask.price[-idx]
      my.lob$ask.vol <-  my.lob$ask.vol[-idx]
      my.lob$ask.id <-  my.lob$ask.id[-idx]
      my.lob$ask.time <- my.lob$ask.time[-idx]

    }

  }

  # update order
  if (order.in$type.order == 'Update' ) {

    if (order.in$side == 'Buy') {
      idx <- which(my.lob$bid.id == order.in$id)

      if (length(idx) == 0){
        if (!silent) cat('\tCant match id for Update order..')
        return(my.lob)
      }

      my.lob$bid.price[idx] <-  order.in$price
      my.lob$bid.vol[idx] <-  order.in$vol
      my.lob$bid.id[idx] <-  order.in$id
      my.lob$bid.time[idx] <- order.in$time
    }

    if (order.in$side == 'Sell') {
      idx <- which(my.lob$ask.id == order.in$id)

      if (length(idx) == 0){
        if (!silent) cat('\tCant match id for Update order..')
        return(my.lob)
      }

      my.lob$ask.price[idx] <-  order.in$price
      my.lob$ask.vol[idx] <-  order.in$vol
      my.lob$ask.id[idx] <-  order.in$id
      my.lob$ask.time[idx] <- order.in$time

    }

  }


  my.lob <- organize.lob(my.lob)


  return(my.lob)

}

#' Prints the LOB
#'
#' @inheritParams organize.lob
#' @param max.level Max level of lob to print
#'
#' @return nothing
#'
#' @examples
#' # no example (internal)
print.lob <- function(my.lob, max.level = 3) {

  cat(paste0('Last update: ', my.lob$last.update, '\n') )

  cat('\nASK price: ', paste0(format(my.lob$ask.price,digits = 4), collapse = '\t'))
  cat('\nBID price: ', paste0(format(my.lob$bid.price, digits = 4), collapse = '\t'))

  cat('\nASK vol: ', paste0(format(my.lob$ask.vol,digits = 4), collapse = '\t'))
  cat('\nBID vol: ', paste0(format(my.lob$bid.vol, digits = 4), collapse = '\t'))

}

#' Process LOB from asset dataframe
#'
#' @param asset.df A dataframe with orders for a single asset
#' @inheritParams ghfd_build_lob
#'
#' @return The lob for the single asset
#'
#' @examples
#' # no example (internal)
process.lob.from.df <- function(asset.df, silent = T) {

  # sort df by priority time
  asset.df <- asset.df[order(asset.df$PriorityDateTime), ]

  # get first new order to fill book
  idx.bid <- sort(which(asset.df$OrderSide == 'Buy'& asset.df$ExecutionType == 'Trade'))[1:3]
  idx.ask <- sort(which(asset.df$OrderSide == 'Sell'& asset.df$ExecutionType == 'Trade'))[1:3]

  if (any(is.na(c(idx.bid,idx.ask)))){
    idx.bid <- sort(which(asset.df$OrderSide == 'Buy'& asset.df$ExecutionType == 'New'))[1:3]
    idx.ask <- sort(which(asset.df$OrderSide == 'Sell'& asset.df$ExecutionType == 'New'))[1:3]

  }

  my.lob <- list(bid.price = asset.df$OrderPrice[idx.bid],
                 ask.price = asset.df$OrderPrice[idx.ask],
                 bid.id = asset.df$SequentialOrderNumber[idx.bid],
                 ask.id = asset.df$SequentialOrderNumber[idx.ask],
                 bid.vol = asset.df$TotalQuantity[idx.bid],
                 ask.vol = asset.df$TotalQuantity[idx.ask],
                 bid.time = asset.df$OrderDatetime[idx.bid],
                 ask.time = asset.df$OrderDatetime[idx.ask],
                 last.update = NA)


  #browser()
  delta.sec <- 5 # in sec
  lob.info <- data.frame()
  my.l <- list()
  for (i.row in seq(1, nrow(asset.df))) {

    i.df <- asset.df[i.row, ]

    my.asset <- unique(asset.df$InstrumentSymbol)

    if (!silent) {
      cat(paste0("\n\tProcessing ", my.asset,'\t',i.df$PriorityDateTime, '\t(',i.row,'|', nrow(asset.df),')' ) )
    }


    #if (i.row ==6) browser()
    #browser()
    if (i.row == 1) my.lob <- organize.lob(my.lob)

    order.in <- list()
    order.in$price = i.df$OrderPrice
    order.in$vol = i.df$TotalQuantity[1]
    order.in$side = i.df$OrderSide[1]
    order.in$type.order = i.df$ExecutionType[1]
    order.in$id = i.df$SequentialOrderNumber[1]
    order.in$time = i.df$OrderDatetime

    #print(as.character(order.in$type.order))

    my.lob <- add.order(my.lob, order.in)

    df.lob <- data.frame(InstrumentSymbol = my.asset,
                         best.ask  = my.lob$ask.price[1],
                         best.bid  = my.lob$bid.price[1],
                         mid.quote = (my.lob$ask.price[1] + my.lob$bid.price[1])/2,
                         spread = my.lob$ask.price[1] - my.lob$bid.price[1],
                         update.time = my.lob$last.update)

    # create list, later rbind it
    my.l <- c( my.l, list(df.lob))

    # OLD code with rbind (slower)

    #lob.info <- rbind(lob.info, data.frame(InstrumentSymbol = my.asset,
    #                                      best.ask  = my.lob$ask.price[1],
    #                                     best.bid  = my.lob$bid.price[1],
    #                                    mid.quote = (my.lob$ask.price[1] + my.lob$bid.price[1])/2,
    #                                   spread = my.lob$ask.price[1] - my.lob$bid.price[1],
    #                                  update.time = my.lob$last.update))
    #print.lob(my.lob)

  }

  lob.info <- do.call(what = dplyr::bind_rows, args = my.l)

  return(lob.info)

}
