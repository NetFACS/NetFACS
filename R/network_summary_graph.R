#' Returns all kinds of graph-level network measures for the netfacs network
#'
#' Calculates graph level summary measures from the network object
#'
#' @param netfacs.net igraph network object resulting from netfacs.network() function
#'
#' @return Function returns a dataframe with the number of elements in the graph, the number of connected edges, mean strength of connections, transitivity (mean number of closed triads), diameter (furthest path between two elements), degree centralization, and mean distance between elements
#' @importFrom igraph mean_distance
#' @importFrom igraph centr_degree
#' @importFrom igraph diameter
#' @importFrom igraph transitivity
#' @importFrom igraph simplify
#' @importFrom igraph edge_density
#' @importFrom igraph as.undirected
#' @export
#'
#' @examples
#' data(emotions_set)
#' angry.face <- netfacs(
#'   data = emotions_set[[1]],
#'   condition = emotions_set[[2]]$emotion,
#'   test.condition = "anger",
#'   ran.trials = 100,
#'   combination.size = 2
#' )
#'
#' anger.net <- netfacs.network(
#'   netfacs.data = angry.face,
#'   link = "unweighted",
#'   significance = 0.01,
#'   min.count = 1,
#'   min.prob = 0,
#'   min.specificity = 0,
#'   ignore.element = NULL
#' )
#'
#' network.summary.graph(anger.net)
network.summary.graph <- function(netfacs.net) {
  # set result data frame

  net.from.igraph <-
    data.frame(
      nr.elements = length(vertex.attributes(netfacs.net)$name),
      nr.edges = length(edge.attributes(netfacs.net)$weight),
      density = edge_density(simplify(netfacs.net), loops = FALSE),
      transitivity = transitivity(simplify(netfacs.net), type = "global"),
      diameter = diameter(simplify(netfacs.net), directed = FALSE),
      degree_centralization = centr_degree(simplify(netfacs.net))$centralization,
      mean_distance = mean_distance(simplify(netfacs.net))
    )
  net.from.igraph[sapply(net.from.igraph, is.numeric)] <-
    lapply(net.from.igraph[sapply(net.from.igraph, is.numeric)], round, 3)
  net.from.igraph
}
