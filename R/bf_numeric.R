

# bf_numeric(x, source, digits = 0){
#
#   assertDataFrame(x = x)
#   assertSubset(x = source, choices = names(x))
#   assertNumeric(x = round, len = 1, lower = 0, finite = TRUE)
#
#   out <- round(x[[source]], digits)
#
#   attr(out, which = "name") <- paste0("numeric_", source)
#   attr(out, which = "desc") <- paste0("the observation has the numeric value [...] in column '", source, "'.")
#
# }
