Recently, Bovespa, the Brazilian financial exchange company, allowed external access to its [ftp site](ftp://ftp.bmf.com.br/). In this address one can find several information regarding the Brazilian financial system, including datasets with high frequency (tick by tick) trading data for three different markets: equity, options and BMF.

Downloading and processing these files, however, can be exausting. The dataset is composed of zip files with the whole trading data, separated by day and market. These files are huge in size and processing or aggregating them in a usefull manner requires specific knowledge for the structure of the dataset. 

The package GetHFData make is easy to access this dataset directly by allowing the easy importation and aggregations of it. Based on this package the user can:

* Access the contents of the Bovespa ftp using function function `ghfd_get_ftp_contents`
* Get the list of available ticker in the trading data using `ghfd_get_available_tickers_from_ftp`
* Download individual files using `ghfd_download_file` 
* Download and process a batch of dates and assets codes with `ghfd_get_HF_data` 

More details about the usage of the package can be found in my [SSRN paper](https://ssrn.com/abstract=2824058) and its [vignette](https://cran.r-project.org/web/packages/GetHFData/vignettes/ghfd-vignette.html). 


## Instalation

You can install `GetHFData` from CRAN:

```
install.packages('GetHFData')
``` 




