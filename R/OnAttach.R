.onAttach <- function(libname, pkgname) {

  citation.apa <- 'Perlin, M., Ramos, H. (2016). GetHFData: A R Package for Downloading and Aggregating High Frequency Trading Data from Bovespa. Available at SSRN.'
  citation.bibtex <- '@article{perlin2016gethfdata,
  title={GetHFData: A R Package for Downloading and Aggregating High Frequency Trading Data from Bovespa},
  author={Perlin, Marcelo and Henrique, Ramos},
  journal={Available at SSRN},
  year={2016}
}'
  my.message <- paste('Thank you for using GetHFData! More details about the package can be found in:\n\n',
                      'https://ssrn.com/abstract=2824058','\n\n',
                      'If applicable, please use the following citations in your research report. Thanks!',
                      '\n\nAPA:\n',citation.apa,
                      '\n\nbibtex:\n',citation.bibtex )
  packageStartupMessage(my.message)
}
