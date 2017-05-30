.onAttach <- function(libname, pkgname) {

  citation.apa <- 'Perlin, M., Ramos, H. (2016). GetHFData: A R Package for Downloading and Aggregating High Frequency Trading Data from Bovespa. Brazilian Review of Finance, V. 14, N. 3.'
  citation.bibtex <- '@article{perlin2016gethfdata,
  title={GetHFData: A R Package for Downloading and Aggregating High Frequency Trading Data from Bovespa},
  author={Perlin, Marcelo and Henrique, Ramos},
  journal={Brazilian Review of Finance},
  volume={14},
  number={3},
  year={2016},
  publisher={Brazilian Society of Finance}
}'
  my.message <- paste('Thank you for using GetHFData! More details about the package can be found in:\n\n',
                      'http://bibliotecadigital.fgv.br/ojs/index.php/rbfin/article/view/64587/65702','\n\n',
                      'If applicable, please use the following citations in your research report. Thanks!',
                      '\n\nAPA:\n',citation.apa,
                      '\n\nbibtex:\n',citation.bibtex )
  packageStartupMessage(my.message)
}
