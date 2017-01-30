library(testthat)
library(GetHFData)

#test_that(desc = 'Test of download function',{
#          expect_equal(1, 1) } )

my.assets <- c('ABEVA20', 'PETRL78')
out.file <- system.file("extdata", 'NEG_OPCOES_20151126.zip', package = "GetHFData")

df.out <- ghfd_read_file(out.file, my.assets)

test_that(desc = 'Test of read function',{
  expect_true(nrow(df.out)>0)
  } )

#cat('\nDeleting test folder')
#unlink(dl.folder, recursive = T)

