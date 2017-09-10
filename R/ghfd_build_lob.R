#' Building LOB (limit order book) from orders
#'
#' @param df.orders A dataframe, output from ghfd_GetHFData
#' @param silent Should the function print progress ? (TRUE (default) or FALSE)  
#'
#' @return A dataframe with information about LOB
#' @export
#'
#' @examples
#' \dontrun{
#' library(GetHFData)
#' first.time <- '11:00:00'
#' last.time <- '17:00:00'
#' first.date <- as.Date('2015-11-03')
#' last.date <- as.Date('2015-11-03')
#' type.output <- 'raw'
#' type.data <- 'orders'
#' type.market = 'equity-odds'
#' 
#' df.out <- ghfd_get_HF_data(my.assets =my.assets,
#'                           type.market = type.market,
#'                           type.data = type.data,
#'                           first.date = first.date,
#'                           last.date = last.date,
#'                           first.time = first.time,
#'                           last.time = last.time,
#'                           type.output = type.output)
#'                           
#' df.lob <- ghfd_build_lob(df.out)
#' }
ghfd_build_lob <- function(df.orders, silent = TRUE) {
    
  # check inputs
  if (class(df.orders) != 'data.frame'){
    stop('Input df.orders is not a dataframe..')
  }
  
  unique.assets <- unique(df.orders$InstrumentSymbol)
  
  df.lob <- data.frame()
  for (i.asset in unique.assets) {
    
    temp.df <- df.orders[df.orders$InstrumentSymbol == i.asset, ]
    cat(paste0('\nBuilding LOB for ', i.asset, ' - ', nrow(temp.df), ' orders') )
    
    temp.lob <- process.lob.from.df(temp.df, silent = F)
    
    df.lob <- dplyr::bind_rows(df.lob, temp.lob)

  }
  
  return(df.lob)
}
