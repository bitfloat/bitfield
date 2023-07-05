

qb_match <- function(x, test, against){

  assertDataFrame(x = x)
  assertSubset(x = test, choices = names(x))
  assertClass(x = against, classes = class(x[[test]]))

  out <- x[[test]] %in% against

  return(out)

}
