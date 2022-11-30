#' Extract results from a netfacs object
#'
#' Extract results from a \code{\link{netfacs}} object.
#'
#' @param netfacs.data An object of class \code{\link{netfacs}}.
#' @param combination.size Numeric, denoting the combination size(s) that should
#'   be extracted. If \code{NULL} (default), all combination sizes are returned.
#' @param significance Numeric value between 0 and 1, determining the p-value
#'   below which combinations are considered to be dissimilar enough from the
#'   null distribution.
#' @param min.count Numeric, denoting the minimum number of times an element
#'   combination occurred.
#' @param min.prob Numeric value between 0 and 1, denoting the minimum
#'   probability an element combination occurred to be displayed.
#'
#' @return Function returns a \code{\link[tibble:tibble]{tibble}} data.frame
#'   that contains the results of the \code{\link{netfacs}} object. By default,
#'   returns all results for all observed combinations, but can optionally
#'   pre-filter results.
#'
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 10,
#'   combination.size = 2
#' )
#'
#' netfacs_extract(angry.face)
netfacs_extract <- function(
    netfacs.data,
    combination.size = NULL,
    significance = 1,
    min.count = 0,
    min.prob = 0
) {
  
  if (isFALSE(is.netfacs(netfacs.data) | is.netfacs_multiple(netfacs.data))) {
    stop("'Argument 'netfacs.data' must be of class 'netfacs' or 'netfacs_multiple'.")
  }
   
  if (is.netfacs_multiple(netfacs.data)) {
    d <- 
      lapply(netfacs.data, function(x) x[[1]]) %>% 
      dplyr::bind_rows(.id = "condition") 
  } else {
    d <- 
      netfacs.data$result %>% 
      dplyr::mutate(condition = netfacs.data$used.parameters$test.condition,
                    .before = 1)
  }
  
  if (is.null(combination.size)) {
    combination.size <- unique(d$combination.size)
  }
  
  d <- 
    d %>% 
    dplyr::filter(
      .data$combination.size %in% {{combination.size}},
      .data$observed.prob >= min.prob,
      .data$count >= min.count,
      .data$pvalue <= significance
    ) %>% 
    tibble::as_tibble()
  
  return(d)
}


#' @rdname netfacs_extract
#' @export
netfacs.extract <- function(netfacs.data,
                            combination.size = NULL,
                            significance = 1,
                            min.count = 0,
                            min.prob = 0) {
  netfacs_extract(
    netfacs.data,
    combination.size,
    significance,
    min.count,
    min.prob
  )
}