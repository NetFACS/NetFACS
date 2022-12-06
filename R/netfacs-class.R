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
    "Random trials:", n_ran_trials,
    "\n"
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
    "Random trials:", n_ran_trials,
    "\n"
  )
}

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

#' Checks if argument is a \code{netfacs_specificity} object
#'
#' @param x An \R object
#'
#' @export
is.netfacs_specificity <- function(x) {
  inherits(x, "netfacs_specificity")
}

#' Extract used data from a \code{netfacs} object
#'
#' @param x extract data from the test condition of a \code{netfacs} object
#' @param condition one of "all" (default), "test" or "null".
#' @export
get_data <- function(x, 
                     condition = "all") {
  
  m <- x$used.data$data
  
  if (condition == "all") {
    return(m)
  }
  
  if (condition == "test") {
    if (attr(x, "stat_method") == "bootstrap") {
      out <- m[x$used.data$condition == x$used.parameters$test.condition, , 
               drop = FALSE]
      return(out)
    } else {
      # if the method is permutation the whole matrix is the test data
      return(m)
    }
  }
  
  if (condition == "null") {
    if (attr(x, "stat_method") == "bootstrap") {
      out <- m[x$used.data$condition == x$used.parameters$null.condition, ]
      return(out)
    } else {
      return(NULL)
    }
  }
}