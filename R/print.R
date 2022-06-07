#' Print method for objects of class \code{netfacs}
#'
#' @param x An object of class \code{netfacs}
#' @param ... Additional arguments that would be passed to or from other methods
#' @export
print.netfacs <- function(x, ...) {
  n_events <- nrow(x$used.data$data)
  n_elements <- length(colnames(x$used.data$data))
  n_combinations <- nrow(x$res)
  max_comb_size <- max(x$result$combination.size)
  method <- attr(x, "stat_method")
  n_ran_trials <- attr(x, "random_trials")
  cat(
    "A netfacs object: \n",
    "Number of elements:", n_elements, "\n",
    "Number of element combinations:", n_combinations, "\n",
    "Maximum combination size:", max_comb_size, "\n",
    "Number of observations:", n_events, "\n",
    "Stat method:", method, "\n", 
    "Random trials:", n_ran_trials
  )
}

#' Print method for objects of class \code{netfacs_multiple}
#'
#' @param x An object of class \code{netfacs_multiple}
#' @param ... Additional arguments that would be passed to or from other methods
#' @export
print.netfacs_multiple <- function(x, ...) {
  n.conditions <- length(names(x))
  method <- attr(x, "stat_method")
  n_ran_trials <- attr(x, "random_trials")
  cat(
    "A netfacs_multiple object: \n",
    "Number of conditions:", n.conditions, "\n",
    "Stat method:", method, "\n", 
    "Random trials:", n_ran_trials
  )
}