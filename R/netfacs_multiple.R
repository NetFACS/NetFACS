#' Applies the \code{\link{netfacs}} function across multiple levels of the
#' condition and puts them in a list
#'
#' Take dataset and report observed and expected likelihood that elements and
#' combinations of elements occur in this dataset, and whether this differs from
#' a null condition. Expected values are based on bootstraps of null
#' distribution, so the values represent distribution of element co-occurrence
#' under null condition. The resulting object is the basis for most other
#' functions in this package.
#'
#' @inheritParams netfacs
#' @param condition character vector of same length as 'data' that contains
#'   information on the condition each event belongs to, so probabilities can be
#'   compared across conditions
#'
#' @return Function returns for each level of the condition a list equivalent to
#'   the results of the netfacs function; can be used to create multiple
#'   networks and graphs at the same time
#'
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces <- netfacs_multiple(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   ran.trials = 10, # only for example
#'   combination.size = 2
#' )
#'
#' head(emo.faces$anger$result, 5)
#' head(emo.faces$happy$result, 5)
netfacs_multiple <- function(data,
                             condition,
                             duration = NULL,
                             ran.trials = 1000,
                             control = NULL,
                             random.level = NULL,
                             combination.size = 2,
                             tail = "upper.tail",
                             use_parallel = TRUE,
                             n_cores = 2) {
  
  if (length(condition) != nrow(data)) {
    stop("condition vector must be the same length as nrow(data).",
         call. = FALSE
    )
  }
  conditions <- sort(unique(condition))
  
  out <- lapply(conditions, function(x) {
    xx <- netfacs(
      data = data,
      condition = condition,
      test.condition = x,
      duration = duration,
      ran.trials = ran.trials,
      control = control,
      random.level = random.level,
      combination.size = combination.size,
      tail = tail,
      use_parallel = use_parallel,
      n_cores = n_cores
    )
    return(xx)
  })
  names(out) <- conditions
  
  # set class and attributes
  out <- structure(
    out, 
    class = "netfacs_multiple",
    stat_method = "bootstrap",
    random_trials = ran.trials
  )
  
  return(out)
}


#' (Deprecated) Applies the \code{\link{netfacs}} function across multiple
#' levels of the condition and puts them in a list
#'
#' This function is deprecated. Please see \code{\link{netfacs_multiple}}
#' instead
#'
#' @inheritParams netfacs_multiple
#'
#' @return Function returns for each level of the condition a list equivalent to
#'   the results of the netfacs function; can be used to create multiple
#'   networks and graphs at the same time
#'
#' @export
multiple.netfacs <- function(data,
                             condition = NULL,
                             duration = NULL,
                             ran.trials = 1000,
                             control = NULL,
                             random.level = NULL,
                             combination.size = NULL,
                             tail = "upper.tail",
                             use_parallel = TRUE,
                             n_cores = 2) {
  
  .Deprecated("netfacs_multiple")
  
  netfacs_multiple(data,
                   condition,
                   duration,
                   ran.trials,
                   control,
                   random.level,
                   combination.size,
                   tail,
                   use_parallel,
                   n_cores)
  
}
