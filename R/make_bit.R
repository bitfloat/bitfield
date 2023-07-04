#' Derive a quality bit from provided information
#'
#' Take an input table and a set of functions to combine into a code of quality
#' information. The code will contain the value 1 in case the fail-criteria are
#' triggered.
#' @param input [`data.frame`][data.frame]\cr tidy table that contains all input
#'   information, this may be both the information to evaluate, but also
#'   additional columns carrying information that shall just be passed through
#'   the function.
#' @param ... tests to check deviations from the expected value; combination of
#'   the names of column in the table and a call to the \code{.bit} function.
#' @param sep Separator to use between values. Used for visualising the value
#'   for a human eye.
#' @details In case no additional check is specified for an attribute, merely
#'   the availability of any value is tested. In case there is an additional
#'   test, the outcome of that test is pasted right after the outcome of the
#'   availability test. In the below example 'year' is tested for availability
#'   and for whether values are NA after converting to integers (indicating
#'   false values) and thus occupy the two values at positions 3 and 4 in the
#'   resulting QC. 'commodity' also has an additional test and thus occupies the
#'   positions 5 and 6. In contrast 'landuse' will only be tested for
#'   availability and thus only occupies position 7.
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # make an example dataset
#' input <- tibble(x = sample(seq(23.3, 28.1, 0.1), 10),
#'                 y = sample(seq(57.5, 59.6, 0.1), 10),
#'                 year = rep(2021, 10),
#'                 commodity = rep(c("soybean", "maize"), 5),
#'                 landuse = sample(c("crop", "grazing", "forest"), size = 10, replace = TRUE),
#'                 some_other = rnorm(10))
#'
#' # make it have some errors
#' input$x[5] <- 259
#' input$y[9] <- NA_real_
#' input$year[c(2:3)] <- c(NA, "2021r")
#' input$commodity[c(3, 5)] <- c(NA_character_, "dog")
#'
#'
#' # derive valid values for commodities
#' validComm <- c("soybean", "maize")
#'
#' # specify the quality checks
#' input %>%
#'   make_bits(x = .bit(fun = function(x) is.na(x) | x %in% c(""), flags = 2),
#'             x = .bit(fun = function(x) x < -180 | x > 180, flags = 2),
#'             y = .bit(fun = function(x) is.na(x) | x %in% c(""), flags = 2),
#'             y = .bit(fun = function(x) x < -90 | x > 90, flags = 2),
#'             year = .bit(fun = function(x) is.na(as.integer(x)), flags = 2),
#'             commodity = .bit(fun = function(x) !x %in% validComm, flags = 2),
#'             some_other = .bit(fun = function(x) ifelse(x > 0.5, 0, ifelse(x > 0, 1, 2)),
#'                               flags = 3),
#'             sep = "_")
#' }
#'
#' @importFrom checkmate assertDataFrame assertCharacter testSubset
#' @importFrom tibble tibble
#' @importFrom dplyr select bind_cols mutate if_else left_join
#' @importFrom tidyselect contains everything all_of
#' @importFrom rlang quos eval_tidy :=
#' @importFrom tidyr unite
#' @export

make_bits <- function(input, ..., sep = ""){

  assertDataFrame(x = input)

  vars <- quos(...)
  # return(vars)

  out <- tibble(.rows = dim(input)[1])

  for(i in seq_along(unique(names(vars)))){

    thisAttrib <- unique(names(vars))[i]
    if(!testSubset(x = thisAttrib, choices = colnames(input))){
      message("'", thisAttrib, "' is not part of the column names of the input.")
      next
    }

    # test for availability
    temp <- input %>%
      select(value = all_of(thisAttrib))
    attribVars <- vars[which(names(vars) %in% thisAttrib)]

    # run other checks, in case available
    for(j in seq_along(attribVars)){
      thisTest <- eval_tidy(attribVars[[j]])
      thisFun <- eval_tidy(thisTest$fun)
      if(is.function(thisFun)){
        temp <- temp %>%
          mutate(flag = suppressWarnings(thisFun(value)))

        uVals <- length(unique(temp$flag))

        if(length(thisTest$flags$flag) < uVals){
          stop("please provide as many flags as there are results for testing '", thisAttrib, "' (", length(unique(temp$flag)), ").")
        }
        if(is.logical(temp$flag)){
          temp <- temp %>%
            mutate(!!paste0("code", j) := as.integer(temp$flag)) #incl. na.omit here?!
        } else {
          temp <- temp %>%
            left_join(thisTest$flags, by = "flag") %>%
            mutate(!!paste0("code", j) := bits)
        }

      } else {
        stop("please provide a function that either returns a logical value, or integers.")
      }
    }

    # combine the different tests
    out <- temp %>%
      unite(col = !!thisAttrib, contains("code"), sep = "") %>%
      select(!!thisAttrib) %>%
      bind_cols(out, .)

  }

  out <- out %>%
    unite(col = "QB", everything(), sep = sep)

  out <- bind_cols(input, out)
  return(out)

}
