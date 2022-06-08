#' Extract results from a netfacs object
#'
#' Extract results from a \code{\link{netfacs}} object.
#'
#' @param netfacs.data object resulting from \code{\link{netfacs}} function.
#' @param combination.size numeric, denoting the combination size(s) that should
#'   be extracted. If NULL (default), all combination sizes are returned.
#' @param significance numeric value between 0 and 1, determining the p-value
#'   below which combinations are considered to be dissimilar enough from the
#'   null distribution.
#' @param min.count numeric value, suggesting how many times a combination
#'   should at least occur to be displayed.
#' @param min.prob numeric value between 0 and 1, suggesting the probability at
#'   which a combination should at least occur to be displayed.
#' @param min.specificity numeric value between 0 and 1, suggesting the
#'   specificity a combination should at least have for the test condition to be
#'   displayed.
#'
#' @return Function returns a dataframe that contains the results of the
#'   \code{\link{netfacs}} object. By default, returns all results for all
#'   observed combinations
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
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
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' netfacs_extract(angry.face,
#'   combination.size = 2,
#'   significance = 0.01,
#'   min.count = 5,
#'   min.prob = 0.01,
#'   min.specificity = 0.5
#' )
netfacs_extract <- function(netfacs.data,
                            combination.size = NULL,
                            significance = 1,
                            min.count = 0,
                            min.prob = 0,
                            min.specificity = 0) {
  d <- netfacs.data$result 
  
  if (is.null(combination.size)) {
    combination.size <- unique(d$combination.size)
  }
  
  d <- 
    d %>% 
    filter(.data$combination.size %in% {{combination.size}},
           .data$observed.prob >= min.prob,
           .data$count >= min.count,
           .data$pvalue <= significance)
  
  # specificity is only relevant for bootstrap results
  if (attr(netfacs.data, "stat_method") == "bootstrap") {
    d <- 
      d %>% 
      filter(.data$specificity >= min.specificity)
  }
  
  return(d)
}


#' (Deprecated) Extract results from a \code{\link{netfacs}} object.
#'
#' This function is deprecated. Please see \code{\link{netfacs_extract}}
#' instead
#'
#' @inheritParams netfacs_extract
#' @param level deprecated. Please use combination.size instead.
#' 
#' @return Function returns a dataframe that contains the results of the
#'   \code{\link{netfacs}} object. By default, returns all results for all
#'   observed combinations
#'
#' @export
netfacs.extract <- function(netfacs.data,
                            combination.size = NULL,
                            significance = 1,
                            min.count = 0,
                            min.prob = 0,
                            min.specificity = 0,
                            level) {

  .Deprecated("netfacs_extract")
  
  if (!missing(level)) {
    warning("argument level is deprecated; please use combination.size instead.", 
            call. = FALSE)
    combination.size <- level
  }
  netfacs_extract(
    netfacs.data = netfacs.data,
    combination.size = combination.size,
    significance = significance,
    min.count = min.count,
    min.prob = min.prob,
    min.specificity = min.specificity
  )
}