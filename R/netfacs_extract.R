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
#'   \code{\link{netfacs}} object. By default, returns all combinations
#'   that had significantly higher probabilities of occurring than expected
#'   under the null condition.
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
#' netfacs.extract(angry.face,
#'   combination.size = 2,
#'   significance = 0.01,
#'   min.count = 5,
#'   min.prob = 0.01,
#'   min.specificity = 0.5
#' )
netfacs.extract <- function(netfacs.data,
                            combination.size = NULL,
                            significance = 0.05,
                            min.count = 0,
                            min.prob = 0,
                            min.specificity = 0) {

  # set digits printed to 3, restore user state
  op <- options(digits = 3)         
  on.exit(options(op), add = TRUE) 

  net.data <- netfacs.data$result 
  
  if (is.null(combination.size)) {
    combination.size <- unique(net.data$combination.size)
  }
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    net.data <- 
      net.data %>% 
      mutate(specificity = 1,
             prob.increase = .data$observed.prob / .data$expected.prob)
  }
  
  net.data <- 
    net.data %>% 
    filter(.data$combination.size %in% combination.size,
           .data$observed.prob >= min.prob,
           .data$count >= min.count,
           .data$specificity >= min.specificity,
           .data$pvalue <= significance)
  
  return(net.data)
}
