#' Gets the contents of Bovespa ftp
#'
#' This function will access the Bovespa ftp and return a vector with all files related to trades (all others are ignored)
#'
#' @inheritParams ghfd_get_HF_data
#'
#' @return A list with all files from the ftp that are related to executed trades
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ftp.files <- ghfd_get_ftp_contents(type.market = 'equity')
#' print(ftp.files)
#' }
ghfd_get_ftp_contents <- function(type.market = 'equity',
                                  max.dl.tries = 10){

  # check type.market
  possible.names <- c('equity','equity-odds','options','BMF')

  idx <- type.market %in% possible.names

  if (!any(idx)){
    stop(paste(c('Input type.market not valid. It should be one of the following: ', possible.names), collapse = ', '))
  }

  # test for internet
  test.internet <- curl::has_internet()

  if (!test.internet){
    stop('No internet connection found...')
  }

  # set ftp site
  if (type.market == 'equity')      my.ftp <- "ftp://ftp.bmf.com.br/marketdata/Bovespa-Vista/"
  if (type.market == 'equity-odds') my.ftp <- "ftp://ftp.bmf.com.br/marketdata/Bovespa-Vista/"
  if (type.market == 'options') my.ftp <- "ftp://ftp.bmf.com.br/MarketData/Bovespa-Opcoes/"
  if (type.market == 'BMF')     my.ftp <- "ftp://ftp.bmf.com.br/marketdata/BMF/"

  # set time stop (ftp seems to give wrong files sometimes..)
  Sys.sleep(1)

  i.try <- 1
  while (TRUE){
    cat(paste('\nReading ftp contents for ',type.market, ' (attempt = ', i.try,'|',max.dl.tries,')',sep = ''))
    files.at.ftp <- NULL
    try({
      files.at.ftp <- RCurl::getURL(my.ftp,
                                    verbose=F,
                                    ftp.use.epsv=FALSE,
                                    dirlistonly = TRUE)
    })


    files.at.ftp <- stringr::str_extract_all(files.at.ftp,pattern = 'NEG_(.*?).zip')[[1]]

    # remove or not FRAC market files
    idx <- stringr::str_detect(files.at.ftp, pattern = stringr::fixed('FRAC'))

    if (type.market=='equity-odds'){
      files.at.ftp <- files.at.ftp[idx]
    } else {

      files.at.ftp <- files.at.ftp[!idx]
    }

    # remove BMF files in Bovespa equity (why are these files there??)

    if ((type.market =='equity')|(type.market=='equity-odds')){
      idx <- stringr::str_detect(files.at.ftp, pattern = stringr::fixed('BMF'))
      files.at.ftp <- files.at.ftp[!idx]

      idx <- stringr::str_detect(files.at.ftp, pattern = stringr::fixed('OPCOES'))
      files.at.ftp <- files.at.ftp[!idx]
    }

    # remove larger zip files with several txt files (only a couple of months)
    idx <- sapply(files.at.ftp, FUN = function(x) return(stringr::str_count(x,pattern = '_')))<3

    files.at.ftp <- files.at.ftp[idx]

    # check if html.code and size makes sense. If not, download it again

    if ( is.null(files.at.ftp)|(length(files.at.ftp)<400) ){
      cat(' - Error in reading ftp contents. Trying again..')
    } else {
      break()
    }

    if (i.try==max.dl.tries){
      stop('Reached maximum number of attempts to read ftp content. Exiting now...')
    }

    i.try <- i.try + 1

    Sys.sleep(1)
  }



  # find dates from file names

  ftp.dates <- unlist(stringr::str_extract_all(files.at.ftp,
                                               pattern = paste0(rep('[0-9]',8),
                                                                collapse = '')))
  ftp.dates <- as.Date(ftp.dates,format = '%Y%m%d')

  return(data.frame(files = as.character(files.at.ftp),
                    dates = ftp.dates,
                    link = as.character(paste0(my.ftp,as.character(files.at.ftp) ))))

}
