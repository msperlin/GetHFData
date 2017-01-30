#' Downloads a single file from Bovespa ftp
#'
#' This function will take as input a ftp addresss, the name of the downloaded file in the local drive,
#' and it will download the corresponding file. Returns TRUE if it worked and FALSE otherwise.
#'
#' @param my.ftp A complete, including file name, ftp address to download the file from
#' @param out.file Name of downloaded file with HFT data from Bovespa
#' @inheritParams ghfd_get_HF_data
#'
#' @return TRUE if sucessfull, FALSE if not
#' @export
#'
#' @examples
#'
#' my.ftp <- 'ftp://ftp.bmf.com.br/MarketData/Bovespa-Opcoes/NEG_OPCOES_20151229.zip'
#' out.file <- 'temp.zip'
#'
#' ghfd_download_file(my.ftp = my.ftp, out.file=out.file)
#'
#' ## check if exists
#' file.exists(out.file)
#'
#' ## clean up example
#' file.remove(out.file)
ghfd_download_file <- function(my.ftp,
                               out.file,
                               dl.dir = 'Dl Files',
                               max.dl.tries = 10){

  if (length(my.ftp)!=1){
    stop('ERROR: input my.ftp should have length 1')
  }

  i.try <- 1

  while (TRUE){
    cat(paste0(' Attempt ', i.try))

    if (file.exists(out.file)&(file.size(out.file)>100)){
      cat(' - File exists, skipping dl')
      break()

    } else {
      # DO TRIES for download

      try({
        utils::download.file(url = my.ftp ,
                             method = 'auto',
                             destfile = out.file ,
                             quiet = T)
      })


      if (file.size(out.file) < 100  ){
        cat(' - Error in downloading. Trying again..')
      } else {
        return(TRUE)
        break()
      }
    }

    if (i.try==max.dl.tries){
      warning('Reached maximum number of attempts to read ftp content. Exiting now...')
      return(FALSE)
    }

    i.try <- i.try + 1

    Sys.sleep(1)
  }



}
