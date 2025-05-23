#' Creates network objects out of the netfacs data
#'
#' Takes the results of the nefacs object for combinations of 2 elements and
#' turns them into a network object (class \code{\link[igraph]{igraph}} and
#' \code{\link[tidygraph]{tbl_graph}}) that can be used for further plotting and
#' analyses
#'
#' @param netfacs.list list of multiple objects resulting from
#'   \code{\link{netfacs}} function or the \code{\link{netfacs_multiple}}
#'   function
#' @param link determines how nodes/elements are connected. 'unweighted' gives a
#'   1 to significant connections and 0 to all others; 'weighted' gives the
#'   difference between observed and expected probability of co-occurrence;
#'   'raw' just uses the observed probability of co-occurrence; 'SRI' uses the
#'   simple ratio index/affinity (probability of co-occurrence/ (probabilities
#'   of each element and the combination))
#' @param min.count numeric value, suggesting how many times a combination
#'   should at least occur to be displayed
#' @param min.prob numeric value, suggesting the probability at which a
#'   combination should at least occur to be displayed
#' @param significance numeric value, determining the p-value below which
#'   combinations are considered to be dissimilar enough from the null
#'   distribution
#' @param ignore.element vector of elements that will not be considered for the
#'   network, e.g. because they are too common or too rare or their
#'   interpretation is not relevant here
#'
#' @return Function returns a network object where the nodes are the elements,
#'   edges represent their co-occurrence, and the vertex and edge attributes
#'   contain all additional information from the netfacs object
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
#' emo.nets <- multiple_netfacs_network(emo.faces)
multiple_netfacs_network <-
  function(netfacs.list,
           link = "unweighted",
           significance = 0.01,
           min.count = 1,
           min.prob = 0,
           ignore.element = NULL) {
    multi.net <- lapply(netfacs.list, function(x) {
      xx <- netfacs_network(
        x,
        link = link,
        significance = significance,
        min.count = min.count,
        min.prob = min.prob,
        ignore.element = ignore.element
      )
      return(xx)
    })
    names(multi.net) <- names(netfacs.list)
    return(multi.net)
  }

#' @rdname multiple_netfacs_network
#' @export
multiple.netfacs.network <-
  function(netfacs.list,
           link = "unweighted",
           significance = 0.01,
           min.count = 1,
           min.prob = 0,
           ignore.element = NULL) {
    multiple_netfacs_network(netfacs.list,
                             link,
                             significance,
                             min.count,
                             min.prob,
                             ignore.element)
  }
