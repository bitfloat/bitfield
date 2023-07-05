

qb_range <- function(x, test, min, max){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertNumeric(x = min, len = 1, finite = TRUE)
  assertNumeric(x = max, len = 1, finite = TRUE)

  out <- x[[test]] > min & x[[test]] < max

  return(out)

}
