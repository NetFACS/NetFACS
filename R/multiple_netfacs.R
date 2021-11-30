#' Applies the netfacs function across multiple levels of the condition and puts
#' them in a list
#'
#' Take dataset and report observed and expected likelihood that elements and
#' combinations of elements occur in this dataset, and whether this differs from
#' a null condition Expected values are based on bootstraps of null
#' distribution, so the values represent distribution of element co-occurrence
#' under null condition; or permutations of the observed distribution to test it
#' against 'random'. The resulting object is the basis for most other functions
#' in this package.
#'
#' @param data matrix with one column per element, and one row per event,
#'   consisting of 1 (element was active during that event) and 0 (element was
#'   not active)
#' @param condition character vector of same length as 'data' that contains
#'   information on the condition each event belongs to, so probabilities can be
#'   compare across conditions; if NULL, all events will be tested against
#'   random
#' @param duration numeric vector that contains information on the duration of
#'   each event; if NULL, all rows have the same value
#' @param ran.trials Number of randomisations that will be performed to find the
#'   null distribution
#' @param random.level character vector of the level on which the randomization
#'   should take place. If NULL, the randomization takes place on the event
#'   level (i.e., every row can either be selected or not); if a vector is
#'   provided, the randomization takes place on the levels of that vector rather
#'   than individual events
#' @param control list of vectors that are used as control variables. During
#'   bootstraps, the ratio of events in each level will be adapted. So, for
#'   example, if in the test distribution, there are three angry participants
#'   for each happy participant, the null distribution will maintain that ratio
#' @param combination.size if not all combinations of elements are of interest
#'   (e.g., if the question only concerns single elements or dyads of elements),
#'   this variable allows to reduce the results to those combinations,
#'   increasing speed
#' @param tail either 'upper.tail' (proportion of null probabilities that are
#'   larger than observed probabilities), or 'lower.tail' (proportion of null
#'   probabilities that are smaller than observed probabilities); default is
#'   'upper.tail'
#' @param use_parallel logical, should the bootstrap be parallelized (default is
#'   \code{TRUE})
#' @param n_cores numeric, the number cores to be used for parallelization.
#'   Default is the number of available cores minus 1.
#'
#' @return Function returns for each level of the condition a list equivalent to
#'   the results of the netfacs function; can be used to create multiple
#'   networks and graphs at the same time
#'
#' @importFrom stats sd
#' @importFrom picante randomizeMatrix
#' @importFrom parallel mclapply
#' @importFrom parallel parLapply
#' @importFrom parallel makeCluster
#' @importFrom parallel detectCores
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces <- multiple.netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   ran.trials = 10, # only for example
#'   combination.size = 2
#' )
#'
#' head(emo.faces$anger$result, 5)
#' head(emo.faces$happy$result, 5)
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
  conditions <- sort(unique(condition))
  
  netfacs.condition <- lapply(conditions, function(x) {
    xx <- netfacs(
      data = data,
      condition = condition,
      test.condition = x,
      null.condition = NULL,
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
  names(netfacs.condition) <- conditions
  return(netfacs.condition)
}
