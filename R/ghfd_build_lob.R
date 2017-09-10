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
