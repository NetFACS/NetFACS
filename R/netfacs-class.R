#' Checks if argument is a \code{netfacs} object
#'
#' @param x An \R object
#'
#' @export
is.netfacs <- function(x) {
  inherits(x, "netfacs")
}

#' Checks if argument is a \code{netfacs_multiple} object
#'
#' @param x An \R object
#'
#' @export
is.netfacs_multiple <- function(x) {
  inherits(x, "netfacs_multiple")
}
