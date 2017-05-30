## Download and Aggregate High Frequency Trading Data from Bovespa

Recently, Bovespa, the Brazilian financial exchange company, allowed external access to its [ftp site](ftp://ftp.bmf.com.br/). In this address one can find several information regarding the Brazilian financial system, including datasets with high frequency (tick by tick) trading data for three different markets: equity, options and BMF.

Downloading and processing these files, however, can be exausting. The dataset is composed of zip files with the whole trading data, separated by day and market. These files are huge in size and processing or aggregating them in a usefull manner requires specific knowledge for the structure of the dataset. 

The package GetHFData make is easy to access this dataset directly by allowing the easy importation and aggregations of it. Based on this package the user can:

* Access the contents of the Bovespa ftp using function function `ghfd_get_ftp_contents`
* Get the list of available ticker in the trading data using `ghfd_get_available_tickers_from_ftp`
* Download individual files using `ghfd_download_file` 
* Download and process a batch of dates and assets codes with `ghfd_get_HF_data` 

In the next example we will only use a local file from the package. Given the size of the files in the ftp and the CHECK process of CRAN, it makes sense to keep this vignette compact and fast to run. More details about the usage of the package can be found in my [RBFIN paper](http://bibliotecadigital.fgv.br/ojs/index.php/rbfin/article/view/64587/65702 ). 

## Instalation

You can install the development version from github:

```
devtools::install_github('msperlin/GetHFData')
``` 
    
The stable version is availabe in CRAN:

```
install.packages('GetHFData')
``` 

## Usage:

```
library(GetHFData)

first.time <- '11:00:00' # first time period of day (anything before is deleted)
last.time <- '17:00:00'  # last time period of day (anything after is deleted)

first.date <- '2015-11-01'
last.date <- '2015-11-10'
type.output <- 'agg' # 'agg' or 'raw'
type.data <- 'trades' # 'trades' or 'orders'
agg.diff <- '15 min' # e.g. '15 sec', '20 min', '1 hour', ..

# partial matching is available
my.assets <- c('PETR','VALE')
type.matching <- 'partial'
type.market  <- 'equity'

df.out <- ghfd_get_HF_data(my.assets =my.assets,
                           type.matching = type.matching,
                           type.market = type.market,
                           type.data = type.data,
                           first.date = first.date,
                           last.date = last.date,
                           first.time = first.time,
                           last.time = last.time,
                           type.output = type.output,
                           agg.diff = agg.diff)

```





