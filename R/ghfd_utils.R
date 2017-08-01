#' Extracts information from options using ticker symbol
#'#'
#' @param ticker.in The ticker of the options (e.f. GGBRB15)
#' @param session.date The date of the trading session  (e.g. as.Date('2010-01-01'))
#'
#' @return A list with the following items: \describe{
#' \item{type.option}{The type of options (CALL or PUT)}
#' \item{strike.price}{The strike price of option}
#' \item{maturity.date}{The maturity date of option (see <http://www.bmfbovespa.com.br/pt_br/produtos/listados-a-vista-e-derivativos/renda-variavel/opcoes-sobre-acoes.htm>)} }
#' @export
#'
#' @examples
#'
#' info <- get.info.opt('GGBRA18', as.Date('2010-01-01'))
get.info.opt <- function(ticker.in, session.date) {

  df.exp <- data.frame(month = c(1:12,1:12),
                       str = c(LETTERS[1:12],
                               LETTERS[13:24]),
                       type.opt = c(rep('CALL',12), rep('PUT', 12)))


  # get expirations month
  expiration.symbol <- stringr::str_sub(ticker.in, 5,5)

  if ( !(expiration.symbol %in% df.exp$str)){
    #browser()
    stop('ERROR: Cant find symbol in table')
  }

  expiration.month <- df.exp$month[expiration.symbol == df.exp$str]

  # get type option
  type.option <- as.character(df.exp$type.opt[expiration.symbol == df.exp$str])
  # get strike price
  strike.price <- as.numeric(stringr::str_sub(ticker.in,6,nchar(ticker.in)))

  # get expiration date (third monday of month/year)
  year.in <- as.numeric(format(session.date,'%Y'))
  my.weekday <- 'Monday'
  my.pos <- 3

  maturity.date <- GetNWeekDayOfMonth(expiration.month, year.in, my.weekday, my.pos)

  if (session.date > maturity.date){
    my.year <- year.in +1
    maturity.date <- GetNWeekDayOfMonth(expiration.month,  my.year , my.weekday, my.pos)
  }

  #browser()

  out.l <- list(type.option = type.option,
                strike.price = as.character(strike.price),
                maturity.date = as.character(maturity.date))

  return(out.l)
}


#' Title
#'
#' @param ticker.in
#'
#' @return
#' @export
#'
#' @examples
find.type.opt <- function(ticker.in) {
  df.exp <- data.frame(month = c(1:12,1:12),
                       str = c(LETTERS[1:12],
                               LETTERS[13:24]),
                       type.opt = c(rep('CALL',12), rep('PUT', 12)))


  # get expirations month
  expiration.symbol <- stringr::str_sub(ticker.in, 5,5)
  # get type option
  type.option <- as.character(df.exp$type.opt[match(expiration.symbol,df.exp$str)])
  return(type.option)

}


#' Find the N weekday of a month/year
#'
#' @param my.month The month, as numeric (e.g. 02)
#' @param my.year  The year, as numeric (e.g. 2010)
#' @param my.weekday The weekday, not abbreviated (e.g. 'Monday')
#' @param my.pos The position of weekday within the month, as numeric (e.g. third monday of month: my.pos = 3)
#'
#' @return Returns a Date object
#' @export
#'
#' @examples
#'
#' date.out <-  GetNWeekDayOfMonth(my.month = 02, my.year = 2010, my.weekday = 'Monday', my.pos = 3)
GetNWeekDayOfMonth <- function(my.month, my.year, my.weekday="Monday", my.pos = 3) {

  # first 7 days of month
  #browser()

  my.date <- as.Date(paste0(my.year,'-',my.month,'-01'))

  #browser()

  addmonth <- function(x) {
    if (x==12){
      x <- 1
    } else {
      x <- x + 1
    }
    return(x)
  }
  s <- seq(from = my.date,
           to =  my.date + 35, by='day')

  my.weedaysvec <- weekdays(s)
  my.dummy <- my.weedaysvec == my.weekday
  my.cumsum <- cumsum(my.dummy)

  idx <- which(my.cumsum == my.pos)[1]

  date.out <- s[idx]

  return(date.out)
}
