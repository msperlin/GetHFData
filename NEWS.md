## Version 1.7 (2019-04-08)

Minor update:

* Fixed bug regarding files at ftp (see [issue 5](https://github.com/msperlin/GetHFData/issues/5))

## Version 1.6 (2018-10-10)

Minor update:

* Fixed bug in ghfd_get_ftp_contents for 'equity' option

## Version 1.5 (2017-11-27)

Minor update:

* Added support for milsecond in LOB

## Version 1.4 (2017-09-10)

Major update:

* Users can now recreate the LOB (limit order book) using order data from Bovespa
* fixed bug for only.dl = TRUE

## Version 1.3 (2017-05-29)

Major update:

* Users can now download and aggregate order files (input type.data)
* Fixed link to paper
* Partial matching for assets is now possible (e.g. use PETR for all stocks or options related to Petrobras)
* implement option for only downloading files (this is helpful if you are dealing with order data and will process the files in other R session or software)
* muted message "Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control." 

## Version 1.2.4 (2017-01-30)

Minor update:

* Fixed bug in msg output when length(my.assets) > 2

## Version 1.2.3 (2017-01-13)

Minor update:

* Fixed bug for non existing assets in first date of download process
* Changed input Date for simpler format (e.g. '2016-01-01' and not as.Date('2016-01-01'))

## Version 1.2.2 (2016-12-05)

Minor update:

* Revised apa citation on attach
* Fixed some typos in vignette and added link to SSRN paper

## Version 1.2.1 (2016-11-07)

Minor update with the following changes:

* The user can now download data from the odd lots equity market (type.market='equity-odds')
* Added Henrique Ramos as a contributor
* Other minor changes

## Version 1.2.0 (2016-10-14)

Minor update with the following changes:

* The function  ghfd_get_HF_data now allows for partial matching of asset names and also the download of all assets available in ftp files
* Function ghfd_get_available_tickers_from_ftp also returns the type of market in data.frame 

## Version 1.1.0 (2016-08-15)

Major update from initial version with the following changes:

* The function for finding tickers in the ftp now looks for the closest date in the case that the actual date is missing from the ftp
* The function for finding tickers now returns a dataframe with the tickers and number of trades
* Added control for bad files
* The output for raw and agg type of output were revised
* The vignette is revised

## Version 1.0.0 - First commit (2016-07-21)
