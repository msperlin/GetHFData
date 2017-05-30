#' Function to get available tickers from downloaded zip file
#'
#' This function will read the zip file downloaded from Bovespa and output
#' a numeric vector where the names of the elements represents the different tickers
#' and the numeric values as the number of trades for each ticker
#'
#' @inheritParams ghfd_download_file
#'
#' @return A dataframe with the  number of trades for each ticker found in file
#' @export
#'
#' @examples
#'
#' ## get file from package (usually this would be been downloaded from the ftp)
#' out.file <- system.file("extdata", 'NEG_OPCOES_20151126.zip', package = "GetHFData")
#'
#' df.tickers <- ghfd_get_available_tickers_from_file(out.file)
#'
#' print(head(df.tickers))
ghfd_get_available_tickers_from_file <- function(out.file){

  if (length(out.file)!=1){
    stop('ERROR: input out.file should have length 1')

  }

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
                       f.name = out.file)
  return(df.out)


}

