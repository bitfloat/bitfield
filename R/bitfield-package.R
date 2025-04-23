#' bitfield: Handle Bitfields to record Meta Data
#'
#' The bitfield package provides tools to record analytic and algorithmic meta
#' data or just any ordinary values to store in a bitfield. A bitfield can
#' accompany any (modelled) dataset and can give insight into data quality,
#' provenance, and intermediate values, or can be used to store various output
#' values per observation in a highly compressed form.
#'
#' The general workflow consists of defining a registry with
#' \code{\link{bf_registry}}, mapping tests to bit-flags with
#' \code{\link{bf_map}}, to encode this with \code{\link{bf_encode}} into an
#' integer value that can be stored and published, or decoded (with
#' \code{\link{bf_decode}}) and re-used in a downstream application. Additional
#' bit-flag protocols can be defined (with \code{\link{bf_protocol}}) and shared
#' as standard with the community via \code{\link{bf_standards}}.
#'
#' @author \strong{Maintainer, Author}: Steffen Ehrmann
#'   \email{steffen.ehrmann@posteo.de}
#'
#' @seealso \itemize{ \item Github project:
#'   \href{https://github.com/bitfloat/bitfield}{https://github.com/bitfloat/bitfield}
#'   \item Report bugs:
#'   \href{https://github.com/bitfloat/bitfield/issues}{https://github.com/bitfloat/bitfield/issues}
#'   }
#'
#' @details
"_PACKAGE"
