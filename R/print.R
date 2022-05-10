#' Print method for objects of class \code{netfacs}
#'
#' @param x An object of class \code{netfacs}
#' @param ... Additional arguments that would be passed to or from other methods
#' @export
print.netfacs <- function(x, ...) {
  n_events <- nrow(x$used.data$data)
  elements <- length(colnames(x$used.data$data))
  n_combinations <- nrow(x$res)
  max_comb_size <- max(x$result$combination.size)
  method <- ifelse(is.null(x$used.data$condition), "Permutation", "Bootstrap")
  n_ran_trials <- ncol(x$used.data$random.prob)
  cat(
    "A netfacs object: \n",
    elements, "elements \n",
    n_combinations, "element combinations \n",
    "Maximum combination size:", max_comb_size, "\n",
    n_events, "observations \n",
    "Method:", method, "\n", 
    n_ran_trials, "random trials"
  )
}
