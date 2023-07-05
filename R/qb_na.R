

qb_na <- function(x, test){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))

  out <- is.na(x[[test]])

  return(out)

}
