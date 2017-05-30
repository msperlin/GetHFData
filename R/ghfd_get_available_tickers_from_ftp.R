#' Function to get available tickers from ftp
#'
#' This function will read the Bovespa ftp for a given market/date and output
#' a numeric vector where the names of the elements represents the different tickers
#' and the numeric values as the number of trades for each ticker
#'
#' @param my.date A single date to check tickers in ftp (e.g. '2015-11-03')
#' @inheritParams ghfd_get_HF_data
#'
#' @return A data.frame with the tickers, number of found trades and file name
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  df.tickers <- ghfd_get_available_tickers_from_ftp(my.date = '2015-11-03',
#'  type.market = 'BMF')
#'
#'  print(head(df.tickers))
#' }
ghfd_get_available_tickers_from_ftp <- function(my.date = '2015-11-03',
                                                type.market = 'equity',
                                                type.data = 'trades',
                                                dl.dir = 'ftp files',
                                                max.dl.tries = 10){

  if (length(my.date)!=1){
    stop('ERROR: input my.date should have length = 1')
  }

  # check date class
  my.date <- as.Date(my.date)
  if (class(my.date) != 'Date') {
    stop('ERROR: Input my.date can either be a Date object or a string with the standard data format (YYYY-MM-DD)')
  }

  if (!dir.exists(dl.dir)) {
    dir.create(dl.dir)
  }

  # check type.market
  # check type.market
  possible.names <- c('equity','equity-odds','options','BMF')

  idx <- type.market %in% possible.names

  if (!any(idx)){
    stop(paste(c('Input type.market not valid. It should be one of the following: ', possible.names), collapse = ', '))
  }

  # test for internet
  test.internet <- curl::has_internet()

  # set ftp site
  if (type.market == 'equity')
    my.ftp <- "ftp://ftp.bmf.com.br/marketdata/Bovespa-Vista/"
  if (type.market == 'equity-odds')
    my.ftp <- "ftp://ftp.bmf.com.br/marketdata/Bovespa-Vista/"
  if (type.market == 'options')
    my.ftp <- "ftp://ftp.bmf.com.br/MarketData/Bovespa-Opcoes/"
  if (type.market == 'BMF')
    my.ftp <- "ftp://ftp.bmf.com.br/marketdata/BMF/"

  # get contents
  df.ftp <- ghfd_get_ftp_contents(type.market = type.market,
                                  type.data = type.data,
                                  max.dl.tries = max.dl.tries)

  idx <- which(df.ftp$dates == my.date)

  if  (length(idx)==0){

    closest.date <- df.ftp$dates[which.min(abs(df.ftp$dates - my.date))]

    warning(paste('Cant find date', my.date,' in ftp. Selecting the closest date,',closest.date))

    idx <- which(df.ftp$dates == closest.date)

  }

  files.to.dl <- df.ftp$files[idx]

  my.links <- paste0(my.ftp, files.to.dl)

  my.url <- my.links[1]
  out.file <- paste0(dl.dir, '/', files.to.dl[1])

  ghfd_download_file(my.url, out.file, max.dl.tries)

  suppressWarnings(suppressMessages(
    my.df <- readr::read_csv2(file = out.file,
                              skip = 1,
                              progress = F,
                              col_names = F,
                              col_types = readr::cols() )
  ))


  out <- sort(table(my.df$X2), decreasing = T)

  df.out <- data.frame(tickers = names(out),
                       n.obs = as.numeric(out),
                       type.market = type.market,
                       f.name = out.file)
  return(df.out)

}

