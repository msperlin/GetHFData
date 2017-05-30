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

## Downloading and aggregating TRADE data

Package GetHDData supports batch downloads and processing of several different tickers using start and end dates. In this vignette we are not running the code given the large size of the downloaded files. You should try the next example in your own computer (just copy, paste and run the code in R).

In this example we will download files from the ftp for all stocks related to Petrobras (PETR) and Vale do Rio Doce (VALE). The data will be processed, resulting in a dataframe with aggregated data.

```
library(GetHFData)

first.time <- '11:00:00'
last.time <- '17:00:00'

first.date <- '2015-11-01'
last.date <- '2015-11-10'
type.output <- 'agg'
type.data <- 'trades'
agg.diff <- '15 min'

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

## Downloading and aggregating ORDER data

Version 1.3 of `GetHFData` makes it possible to download and aggregate order data from Bovespa. The data comprises  buy and sell orders sent by market operators. Tabular data includes type of orders (buy or sell, new/update/cancel/..), date/time of submission, priority time, prices, order quantity, among many other information.

**Be aware that these are very large files.** One day of buy and sell orders in the equity market is around 100 MB zipped and close to 1 GB unzipped. If you computer is not suited to store this data in its memory, **it will crash**.  

Here's an example of usage that will download and aggregate order data for all option contracts related to Petrobras (PETR):

```
library(GetHFData)

first.time <- '10:00:00'
last.time <- '17:00:00'

first.date <- '2015-08-18' 
last.date <- '2015-08-18'

type.output <- 'agg' # aggregates data 
agg.diff <- '5 min' # interval for aggregation

my.assets <- 'PETR' # all options related to Petrobras (partial matching)
type.matching <- 'partial' # finds tickers from my.assets using partial matching
type.market = 'options' # option market
type.data <- 'orders' # order data

df.out <- ghfd_get_HF_data(my.assets =my.assets, 
                           type.data= type.data,
                           type.matching = type.matching,
                           type.market = type.market,
                           first.date = first.date,
                           last.date = last.date,
                           first.time = first.time,
                           last.time = last.time,
                           type.output = type.output,
                           agg.diff = agg.diff)

```
