#' Extract results of 'netfacs' function for specific combination sizes
#' For the selected combination size, the function returns all combinations that had significantly higher probabilities of occuring than expected under the null condition
#'
#' @param netfacs.data object resulting from netfacs() function
#' @param level combination size for which all remaining combinations should be extracted
#' @param min.count numeric value, suggesting how many times a combination should at least occur to be displayed
#' @param min.prob numeric value, suggesting the probability at which a combination should at least occur to be displayed
#' @param min.specificity numeric value, suggesting the specificity a combination should at least have for the test condition to be displayed
#' @param significance numeric value, determining the p-value below which combinations are considered to be dissimilar enough from the null distribution
#'
#' @return Function returns a dataframe that includes all combinations for the selected combination size that fulfil the selected criteria
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
#'   level = 2,
#'   min.count = 5,
#'   min.prob = 0.01,
#'   min.specificity = 0.5,
#'   significance = 0.01
#' )
netfacs.extract <- function(netfacs.data,
                            level = 1,
                            min.count = 1,
                            min.prob = 0,
                            min.specificity = 0,
                            significance = 0.01) {

  # set digits printed to 3, restore user state
  op <- options(digits = 3)         
  on.exit(options(op), add = TRUE) 

  net.data <- netfacs.data$result[netfacs.data$result$combination.size == level, ]
  if (is.null(netfacs.data$used.parameters$test.condition)) {
    net.data$specificity <- 1
    net.data$prob.increase <- net.data$observed.prob / net.data$expected.prob
  }
  net.data <- net.data[net.data$observed.prob >= min.prob & net.data$count >= min.count & net.data$specificity >= min.specificity & net.data$pvalue <= significance, ]
  net.data <- net.data[!is.na(net.data$combination), ]
  rownames(net.data) <- net.data$combination

  return(net.data)
}
