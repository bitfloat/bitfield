#' bitfield: Handle Bitfields to record Meta Data
#'
#' The bitfield package provides tools to record analytic and algorithmic meta
#' data or just any ordinary values to store in a bitfield. A bitfield
#' accompanies a (modelled) dataset and can give insight into data quality,
#' provenance, and intermediate values, or can be used to store output values in
#' a highly compressed form.
#'
#' The general workflow consists of defining a registry, growing it by using the
#' required bitfield operators, putting everything together to a bitfield,
#' encoding this into an integer value that can be stored and published, or
#' decoded and re-used in a downstream application.
#'
#' Operators have four distinct types, based on the flags they grow. \itemize{
#'   \item binary operators, such as \code{\link{bf_na}} grow binary flags that
#'   encode yes/no decisions,
#'   \item case operators, such as \code{\link{bf_case}} grow flags that have
#'   one less values than there are cases to encode,
#'   \item count operators, such as \code{\link{bf_length}} grow flags that
#'   encode the count integers and
#'   \item numeric operators, such as \code{\link{bf_numeric}} grow binary
#'   floating point flags according to the given precision of the floating point
#'   encoding (for example 32 for single precision floating point values).
#' }
#'
#' @author \strong{Maintainer, Author}: Steffen Ehrmann
#'   \email{steffen.ehrmann@posteo.de}
#'
#' @seealso \itemize{ \item Github project:
#'   \href{https://github.com/EhrmannS/bitfield}{https://github.com/EhrmannS/bitfield}
#'   \item Report bugs:
#'   \href{https://github.com/EhrmannS/bitfield/issues}{https://github.com/EhrmannS/bitfield/issues}
#'   }
#'
#' @details
"_PACKAGE"
